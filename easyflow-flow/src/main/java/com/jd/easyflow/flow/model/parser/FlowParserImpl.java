package com.jd.easyflow.flow.model.parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.engine.event.ExpFlowEventListener;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.engine.impl.ExpFlowRunner;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.ExpFilter;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.FlowPostHandler;
import com.jd.easyflow.flow.model.FlowPreHandler;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.action.ExpNodeAction;
import com.jd.easyflow.flow.model.action.FlowNodeAction;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.flow.post.ExpFlowPostHandler;
import com.jd.easyflow.flow.model.flow.pre.ExpFlowPreHandler;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.parser.event.ExpFlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;
import com.jd.easyflow.flow.model.parser.param.FlowParseParam;
import com.jd.easyflow.flow.model.parser.param.PostParseParam;
import com.jd.easyflow.flow.model.parser.param.PreParseParam;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;
import com.jd.easyflow.flow.model.post.ExpNodePostHandler;
import com.jd.easyflow.flow.model.post.FixedNodePostHandler;
import com.jd.easyflow.flow.model.pre.ExpNodePreHandler;
import com.jd.easyflow.flow.model.pre.InclusiveCheckPreHandler;
import com.jd.easyflow.flow.model.pre.MultiCheckPreHandler;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowParserImpl implements FlowParser {

    private static final Logger logger = LoggerFactory.getLogger(FlowParser.class);

    private static final String FLOW_STRING_KEY = "_flow_string";
    
    private static final String PARENT_FLOW_ID_KEY = "_parent_flow_id";
    
    private static final String MAIN_FLOW_ID_KEY = "_main_flow_id";


    @Override
    public List<Flow> parse(String data) {
        return parse(new FlowParseParam(data));
    }

    @Override
    public List<Flow> parse(Map<String, Object> map) {
        return parse(new FlowParseParam(map));
    }

    @Override
    public List<Flow> parse(String data, boolean parseEl) {
        return parse(new FlowParseParam(data, parseEl));
    }

    @Override
    public List<Flow> parse(Map<String, Object> map, boolean parseEl) {
        return parse(new FlowParseParam(map, parseEl));
    }
    
    @Override
    public List<Flow> parse(FlowParseParam param) {
        String stringDef = param.getStringDefinition();
        Object defObj = param.getObjectDefinition();

        if (stringDef == null && defObj == null) {
            throw new FlowException("definition is empty");
        }
               
        Object stringDefObj = stringDef == null ? null : JsonUtil.parseObject(stringDef, Object.class);;
        
        if (defObj == null) {
            defObj = stringDefObj;
        }
        if (stringDef == null) {
            stringDef = JsonUtil.toJsonString(defObj);
        }
        
        List<Flow> allFlowList = null;
        
        if (defObj instanceof Map) {
            allFlowList = parseMapDef((Map<String, Object>) defObj, param.isParseEl());
        } else {
            allFlowList = new ArrayList<Flow>();
            for (int i = 0; i < ((List) defObj).size(); i++) {
                Map<String, Object> defMap = ((List<Map<String, Object>>) defObj).get(i);
                List<Flow> flowList = parseMapDef(defMap, param.isParseEl());
                allFlowList.addAll(flowList);
                for (int j = 1; j < flowList.size(); j++) {
                    flowList.get(j).setProperty(PARENT_FLOW_ID_KEY, flowList.get(0).getId());
                }
                if (i > 0) {
                    flowList.get(0).setProperty(FLOW_STRING_KEY, JsonUtil.toJsonString(defMap));
                    flowList.get(0).setProperty(MAIN_FLOW_ID_KEY, allFlowList.get(0).getId());
                }
            }
        }
        allFlowList.get(0).setProperty(FLOW_STRING_KEY, stringDef);
        return allFlowList;
    }
    
    private List<Flow> parseMapDef(Map<String, Object> mapDef, boolean parseEl) {
        List<Flow> flowList = new ArrayList<Flow>();
        parse(mapDef, flowList, parseEl);
        return flowList;
    }


    private Flow parse(Map<String, Object> map, List<Flow> flowList, boolean parseEl) {        
        Flow flow = new Flow();
        flow.setFlowParser(this);
        flowList.add(flow);
        
        List<FlowParseEventListener> parseListeners = parseParseListeners(map, flow, parseEl);
        triggerParseEvent(parseListeners, FlowParseEventTypes.PARSE_FLOW_START, map, flow, null);
        
        flow.setId((String) map.get(DefConstants.COMMON_PROP_ID));
        flow.setName((String) map.get(DefConstants.COMMON_PROP_NAME));
        flow.setLogFlag((Boolean) map.get(DefConstants.FLOW_PROP_LOG_FLAG));
        // Parse property
        Map<String, Object> properties = (Map<String, Object>) map.get(DefConstants.COMMON_PROP_PROPERTIES);
        flow.putProperties(properties);
        // Parse flow pre handler
        parseFlowPreHandler(map.get(DefConstants.FLOW_PROP_PRE), flow, parseEl);
        // Parse node
        List<Map<String, Object>> nodeListConf = (List<Map<String, Object>>) map.get(DefConstants.FLOW_PROP_NODES);
        List<String> startNodeIdList = new ArrayList<String>();
        if (nodeListConf != null) {
            for (Map<String, Object> nodeConf : nodeListConf) {
                // start node judge
                boolean start = nodeConf.get(DefConstants.NODE_PROP_START) == null ? false
                        : (Boolean) nodeConf.get(DefConstants.NODE_PROP_START);
                if (start) {
                    startNodeIdList.add((String) nodeConf.get(DefConstants.COMMON_PROP_ID));
                }
                // create node
                String type = (String) nodeConf.get(DefConstants.COMMON_PROP_TYPE);
                if (DefConstants.COMMON_PROP_CREATE.equals(type)
                        || nodeConf.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                    if (parseEl) {
                        String exp = (String) nodeConf.get(DefConstants.COMMON_PROP_CREATE_EXP);
                        Map<String, Object> elContext = createElContext(nodeConf, null, flow);
                        FlowNode node = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                        if (node != null) {
                            node.postConstruct(nodeConf, null);
                            flow.addNode(node);
                        }
                    }
                } else {
                    NodeImpl node = new NodeImpl();
                    node.setId((String) nodeConf.get(DefConstants.COMMON_PROP_ID));
                    node.setName((String) nodeConf.get(DefConstants.COMMON_PROP_NAME));
                    node.putProperties((Map<String, Object>) nodeConf.get(DefConstants.COMMON_PROP_PROPERTIES));
                    node.setPreHandler(parseNodePreHandler(
                            new PreParseParam(nodeConf.get(DefConstants.NODE_PROP_PRE), parseEl, node)));
                    node.setAction(parseNodeAction(new ActionParseParam(nodeConf.get(DefConstants.NODE_PROP_ACTION),
                            flowList, parseEl, node)));
                    node.setPostHandler(parseNodePostHandler(
                            new PostParseParam(nodeConf.get(DefConstants.NODE_PROP_POST), parseEl, node)));
                    node.postConstruct(nodeConf, null);
                    flow.addNode(node);
                }
            }
        }
        // set start node.
        if (startNodeIdList.size() > 0) {
            flow.setStartNodeIds(startNodeIdList.toArray(new String[] {}));
        }
        
        // Parse flow post handler
        parseFlowPostHandler(map.get(DefConstants.FLOW_PROP_POST), flow, parseEl);
        // Listener
        parseListeners(map, flow, parseEl);
        // Filter
        parseFilters(map, flow, parseEl);
        // Node filter
        parseNodeFilter(map, flow, parseEl);

        // Node pre handler filter
        parseNodePreHandlerFilter(map, flow, parseEl);
        // Node action filter
        parseNodeActionFilter(map, flow, parseEl);
        // Node post handler filter
        parseNodePostHandlerFilter(map, flow, parseEl);

        // Flow runner
        parseRunner(map, flow, parseEl);
        
        flow.postConstruct(map, null);
        
        triggerParseEvent(parseListeners, FlowParseEventTypes.PARSE_FLOW_END, map, flow, null);
        
        
        triggerParseEvent(parseListeners, FlowParseEventTypes.INIT_FLOW_START, map, flow, null);
        InitContext initContext = new InitContext();
        initContext.setFlowParser(this);
        initContext.setParseEl(parseEl);
        initContext.setFlowList(flowList);
        initContext.setFlowDefinitionMap(map);
        flow.init(initContext, null);
        triggerParseEvent(parseListeners, FlowParseEventTypes.INIT_FLOW_END, map, flow, null);
        return flow;
    }
    
    protected void parseFlowPreHandler(Object preDef, Flow flow, boolean parseEl) {
        if (preDef == null) {
            return;
        }
        if (preDef instanceof String) {
            ExpFlowPreHandler handler = new ExpFlowPreHandler();
            handler.setExp((String) preDef);
            flow.setPreHandler(handler);
            return;
        }
        Map<String, Object> pre = (Map<String, Object>) preDef;
        String type = (String) pre.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return;
            }
            String exp = (String) pre.get(DefConstants.COMMON_PROP_CREATE_EXP);
            Map<String, Object> elContext = createElContext(pre, null, flow);
            FlowPreHandler preHandler = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
            if (preHandler != null) {
                preHandler.postConstruct(pre, null);
                flow.setPreHandler(preHandler);
            }
            return;
        } 
        throw new IllegalArgumentException("Param illegal " + preDef);
    }
    
    protected void parseFlowPostHandler(Object postDef, Flow flow, boolean parseEl) {
        if (postDef == null) {
            return;
        }
        if (postDef instanceof String) {
            ExpFlowPostHandler handler = new ExpFlowPostHandler();
            handler.setExp((String) postDef);
            flow.setPostHandler(handler);
            return;
        }
        Map<String, Object> post = (Map<String, Object>) postDef;
        String type = (String) post.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || post.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return;
            }
            String exp = (String) post.get(DefConstants.COMMON_PROP_CREATE_EXP);
            Map<String, Object> elContext = createElContext(post, null, flow);
            FlowPostHandler postHandler = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
            if (postHandler != null) {
                postHandler.postConstruct(post, null);
                flow.setPostHandler(postHandler);
            }
            return;
        } 
        throw new IllegalArgumentException("Param illegal " + postDef);
    }

    /**
     * Parse listener.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseListeners(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> listeners = (List<Map<String, Object>>) map.get(DefConstants.FLOW_PROP_LISTENERS);
        if (listeners != null) {
            for (Object listenerObj : listeners) {
                if (listenerObj instanceof String) {
                    ExpFlowEventListener expListener = new ExpFlowEventListener((String) listenerObj);
                    flow.getEventTrigger().addListener(expListener);
                } else {
                    Map<String, Object> listener = (Map<String, Object>) listenerObj;
                    String type = (String) listener.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || listener.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) listener.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(listener, null, flow);
                            FlowEventListener eventListener = ElFactory.get().evalWithDefaultContext(exp, elContext,
                                    false);
                            if (eventListener != null) {
                                eventListener.postConstruct(listener, null);
                                flow.getEventTrigger().addListener(eventListener);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse filter.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseFilters(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> filters = (List<Map<String, Object>>) map.get(DefConstants.FLOW_PROP_FILTERS);
        if (filters != null) {
            for (Object filterObj : filters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    flow.addFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(filter, null, flow);
                            Filter flowFilter = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                            if (flowFilter != null) {
                                flowFilter.postConstruct(filter, null);
                                flow.addFilter(flowFilter);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse node filter.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseNodeFilter(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> nodeFilters = (List<Map<String, Object>>) map
                .get(DefConstants.FLOW_PROP_NODE_FILTERS);
        if (nodeFilters != null) {
            for (Object filterObj : nodeFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    flow.addFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(filter, null, flow);
                            Filter nodeFilter = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                            if (nodeFilter != null) {
                                nodeFilter.postConstruct(filter, null);
                                flow.addNodeFilter(nodeFilter);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse node pre handler filter.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseNodePreHandlerFilter(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> nodePreHandlerFilters = (List<Map<String, Object>>) map
                .get(DefConstants.FLOW_PROP_NODE_PRE_HANDLER_FILTERS);
        if (nodePreHandlerFilters != null) {
            for (Object filterObj : nodePreHandlerFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    flow.addFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(filter, null, flow);
                            Filter nodePreHandlerFilter = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                            if (nodePreHandlerFilter != null) {
                                nodePreHandlerFilter.postConstruct(filter, null);
                                flow.addNodePreHandlerFilter(nodePreHandlerFilter);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse node action filter.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseNodeActionFilter(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> nodeActionFilters = (List<Map<String, Object>>) map
                .get(DefConstants.FLOW_PROP_NODE_ACTION_FILTERS);
        if (nodeActionFilters != null) {
            for (Object filterObj : nodeActionFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    flow.addFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(filter, null, flow);
                            Filter nodeActionFilter = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                            if (nodeActionFilter != null) {
                                nodeActionFilter.postConstruct(filter, null);
                                flow.addNodeActionFilter(nodeActionFilter);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse node post handler filter.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseNodePostHandlerFilter(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> nodePostHandlerFilters = (List<Map<String, Object>>) map
                .get(DefConstants.FLOW_PROP_NODE_POST_HANDLER_FILTERS);
        if (nodePostHandlerFilters != null) {
            for (Object filterObj : nodePostHandlerFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    flow.addFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type)
                            || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext(filter, null, flow);
                            Filter nodePostHandlerFilter = ElFactory.get().evalWithDefaultContext(exp, elContext,
                                    false);
                            if (nodePostHandlerFilter != null) {
                                nodePostHandlerFilter.postConstruct(filter, null);
                                flow.addNodePostHandlerFilter(nodePostHandlerFilter);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Parse flow runner.
     * 
     * @param map
     * @param flow
     * @param parseEl
     */
    private void parseRunner(Map<String, Object> map, Flow flow, boolean parseEl) {
        Object runnerObj = (Map<String, Object>) map.get(DefConstants.FLOW_PROP_RUNNER);
        if (runnerObj != null) {
            if (runnerObj instanceof String) {
                ExpFlowRunner expFlowRunner = new ExpFlowRunner((String) runnerObj);
                flow.setRunner(expFlowRunner);
            } else {
                Map<String, Object> runner = (Map<String, Object>) runnerObj;
                String type = (String) runner.get(DefConstants.COMMON_PROP_TYPE);
                if (DefConstants.COMMON_PROP_CREATE.equals(type)
                        || runner.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                    if (parseEl) {
                        String exp = (String) runner.get(DefConstants.COMMON_PROP_CREATE_EXP);
                        Map<String, Object> elContext = createElContext(runner, null, flow);
                        FlowRunner flowRunner = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
                        if (flowRunner != null) {
                            flowRunner.postConstruct(runner, null);
                            flow.setRunner(flowRunner);
                        }
                    }
                }
            }
        }
    }

    @Override
    public NodePreHandler parseNodePreHandler(PreParseParam param) {
        Object preDef = param.getPreDef();
        if (preDef == null) {
            return null;
        }
        if (preDef instanceof String) {
            ExpNodePreHandler handler = new ExpNodePreHandler();
            handler.setExp((String) preDef);
            return handler;
        }
        Map<String, Object> pre = (Map<String, Object>) preDef;
        String type = (String) pre.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!param.isParseEl()) {
                return null;
            }
            String exp = (String) pre.get(DefConstants.COMMON_PROP_CREATE_EXP);
            Map<String, Object> elContext = createElContext(pre, param.getNode(), null);
            NodePreHandler preHandler = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
            if (preHandler != null) {
                preHandler.postConstruct(pre, null);
            }
            return preHandler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_EXP)) {
            String exp = (String) pre.get(DefConstants.COMMON_PROP_EXP);
            ExpNodePreHandler preHandler = new ExpNodePreHandler();
            preHandler.setExp(exp);
            if (preHandler != null) {
                preHandler.postConstruct(pre, null);
            }
            return preHandler;
        } else if (DefConstants.NODE_PRE_TYPE_INCLUSIVECHECK.equals(type)) {
            List<String> preNodes = (List<String>) pre.get(DefConstants.NODE_PRE_PROP_PRE_NODES);
            InclusiveCheckPreHandler preHandler = new InclusiveCheckPreHandler();
            preHandler.setPreNodes(preNodes);
            if (preHandler != null) {
                preHandler.postConstruct(pre, null);
            }
            return preHandler;
        } else if (DefConstants.NODE_PRE_TYPE_MULTICHECK.equals(type)
                || pre.containsKey(DefConstants.NODE_PRE_PROP_PRE_NODES)) {
            List<String> preNodes = (List<String>) pre.get(DefConstants.NODE_PRE_PROP_PRE_NODES);
            MultiCheckPreHandler preHandler = new MultiCheckPreHandler();
            preHandler.setPreNodes(preNodes);
            if (preHandler != null) {
                preHandler.postConstruct(pre, null);
            }
            return preHandler;
        }
        throw new IllegalArgumentException("Param illegal " + pre);

    }

    @Override
    public NodeAction parseNodeAction(ActionParseParam param) {
        Object actionDef = param.getActionDef();
        if (actionDef == null) {
            return null;
        }
        if (actionDef instanceof String) {
            ExpNodeAction nodeAction = new ExpNodeAction();
            nodeAction.setExp((String) actionDef);
            return nodeAction;
        }
        Map<String, Object> action = (Map<String, Object>) actionDef;
        String type = (String) action.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || action.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!param.isParseEl()) {
                return null;
            }
            String exp = (String) action.get(DefConstants.COMMON_PROP_CREATE_EXP);
            Map<String, Object> elContext = createElContext(action, param.getNode(), null);
            NodeAction nodeAction = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
            if (nodeAction != null) {
                nodeAction.postConstruct(action, null);
            }
            return nodeAction;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || action.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpNodeAction nodeAction = new ExpNodeAction();
            String exp = (String) action.get(DefConstants.COMMON_PROP_EXP);
            nodeAction.setExp(exp);
            if (nodeAction != null) {
                nodeAction.postConstruct(action, null);
            }
            return nodeAction;
        } else if (DefConstants.COMMON_PROP_FLOW.equals(type) || action.containsKey(DefConstants.COMMON_PROP_FLOW)
                || action.containsKey(DefConstants.COMMON_PROP_FLOW_ID)) {
            FlowNodeAction nodeAction = new FlowNodeAction();
            if (action.containsKey(DefConstants.COMMON_PROP_FLOW)) {
                Flow flow = parse((Map<String, Object>) action.get(DefConstants.COMMON_PROP_FLOW), param.getFlowList(),
                        param.isParseEl());
                flow.setProperty(FLOW_STRING_KEY, JsonUtil.toJsonString(action.get(DefConstants.COMMON_PROP_FLOW)));
                nodeAction.setFlowId(flow.getId());
            } else if (action.containsKey(DefConstants.COMMON_PROP_FLOW_ID)) {
                nodeAction.setFlowId((String) action.get(DefConstants.COMMON_PROP_FLOW_ID));
            }
            Object startNodeId = action.get(DefConstants.NODE_ACTION_PROP_START_NODE_ID);
            if (startNodeId != null) {
                if (startNodeId instanceof String) {
                    nodeAction.setStartNodeIds(new String[] { (String) startNodeId });
                } else {
                    nodeAction.setStartNodeIds(((List<String>) startNodeId).toArray(new String[] {}));
                }
            }
            Boolean inherit = (Boolean) action.get(DefConstants.NODE_ACTION_PROP_INHERIT);
            if (inherit != null) {
                nodeAction.setInherit(inherit);
            }
            nodeAction.postConstruct(action, null);
            return nodeAction;
        }
        throw new IllegalArgumentException("Param illegal " + action);
    }

    @Override
    public NodePostHandler parseNodePostHandler(PostParseParam param) {
        Object postDef = param.getPostDef();
        if (postDef == null) {
            return null;
        }
        if (postDef instanceof String) {
            ExpNodePostHandler postHandler = new ExpNodePostHandler();
            String exp = (String) postDef;
            postHandler.setExp(exp);
            return postHandler;
        }
        Map<String, Object> post = (Map<String, Object>) postDef;
        String type = (String) post.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || post.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!param.isParseEl()) {
                return null;
            }
            String exp = (String) post.get(DefConstants.COMMON_PROP_CREATE_EXP);
            Map<String, Object> elContext = createElContext(post, param.getNode(), null);
            NodePostHandler postHandler = ElFactory.get().evalWithDefaultContext(exp, elContext, false);
            if (postHandler != null) {
                postHandler.postConstruct(post, null);
            }
            return postHandler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || post.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpNodePostHandler postHandler = new ExpNodePostHandler();
            String exp = (String) post.get(DefConstants.COMMON_PROP_EXP);
            postHandler.setExp(exp);
            postHandler.postConstruct(post, null);
            return postHandler;
        } else if (DefConstants.NODE_POST_TYPE_CONDITION.equals(type)
                || post.containsKey(DefConstants.NODE_POST_PROP_CONDITIONS)
                || post.containsKey(DefConstants.NODE_POST_PROP_WHEN)) {
            List<Map<String, Object>> conditionList = null;
            if (post.containsKey(DefConstants.NODE_POST_PROP_CONDITIONS)) {
                conditionList = (List<Map<String, Object>>) post.get(DefConstants.NODE_POST_PROP_CONDITIONS);
            } else {
                conditionList = Arrays.asList(post);
            }
            for (Map<String, Object> condition : conditionList) {
                if (condition.containsKey(DefConstants.NODE_POST_PROP_WHEN)) {
                    Object value = condition.get(DefConstants.NODE_POST_PROP_WHEN);
                    if (value instanceof Map) {
                        if (param.isParseEl()) {
                            String createExp = (String) ((Map<String, Object>) value)
                                    .get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Map<String, Object> elContext = createElContext((Map<String, Object>) value,
                                    param.getNode(), null);
                            condition.put(DefConstants.NODE_POST_PROP_WHEN,
                                    ElFactory.get().evalWithDefaultContext(createExp, elContext, false));
                        }
                    }
                }
            }

            String conditionType = (String) post.get(DefConstants.NODE_POST_PROP_CONDITION_TYPE);
            Object defaultBranch = post.get(DefConstants.NODE_POST_PROP_DEFAULT_TO);

            ConditionalNodePostHandler postHandler = new ConditionalNodePostHandler(conditionType, conditionList,
                    defaultBranch);
            postHandler.postConstruct(post, null);
            return postHandler;

        } else if (DefConstants.NODE_POST_TYPE_FIXED.equals(type) || post.containsKey(DefConstants.NODE_POST_PROP_TO)) {
            Object nextStartId = post.get(DefConstants.NODE_POST_PROP_TO);
            FixedNodePostHandler postHandler = new FixedNodePostHandler(nextStartId);
            postHandler.postConstruct(post, null);
            return postHandler;
        }
        throw new IllegalArgumentException("Param illegal " + post);
    }
    
    protected List<FlowParseEventListener> parseParseListeners(Map<String, Object> map, Flow flow, boolean parseEl) {
        List<Map<String, Object>> parseListenerConfList = (List<Map<String, Object>>) map
                .get(DefConstants.FLOW_PROP_PARSE_LISTENERS);
        if (parseListenerConfList == null) {
            return null;
        }
        List<FlowParseEventListener> parseListeners = new ArrayList<>();
        for (Object listenerObj : parseListenerConfList) {
            if (listenerObj instanceof String) {
                ExpFlowParseEventListener parseListener = new ExpFlowParseEventListener((String) listenerObj);
                parseListeners.add(parseListener);
            } else {
                Map<String, Object> listener = (Map<String, Object>) listenerObj;
                String type = (String) listener.get(DefConstants.COMMON_PROP_TYPE);
                if (DefConstants.COMMON_PROP_CREATE.equals(type)
                        || listener.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                    if (parseEl) {
                        String exp = (String) listener.get(DefConstants.COMMON_PROP_CREATE_EXP);
                        Map<String, Object> elContext = createElContext(listener, null, flow);
                        FlowParseEventListener parseListener = ElFactory.get().evalWithDefaultContext(exp, elContext,
                                false);
                        if (parseListener != null) {
                            parseListener.postConstruct(listener, null);
                            parseListeners.add(parseListener);
                        }
                    }
                }
            }
        }
        return parseListeners;
    }

    private void triggerParseEvent(List<FlowParseEventListener> listeners, String eventType,
            Map<String, Object> flowDef, Flow flow, Object data) {
        if (listeners == null || listeners.size() == 0) {
            return;
        }
        FlowParseEvent event = new FlowParseEvent();
        event.setType(eventType);
        event.setFlow(flow);
        event.setFlowDef(flowDef);
        event.setData(data);
        for (FlowParseEventListener listener : listeners) {
            listener.on(event);
        }
    }

    /**
     * 
     * Convert java model to string.
     *
     * @param flow
     * @return
     */
    @Override
    public String stringify(Flow flow) {
        if (flow.getProperty(FLOW_STRING_KEY) != null) {
            return flow.getProperty(FLOW_STRING_KEY);
        }
        logger.warn("No original string definition, unsupported now. flowId: " + flow.getId());
        return null;
    }

    private Map<String, Object> createElContext(Map<String, Object> currentDef, FlowNode node, Flow flow) {
        Map<String, Object> context = new HashMap<>(3);
        context.put("definition", currentDef);
        if (node != null) {
            context.put("node", node);
        }
        if (flow != null) {
            context.put("flow", flow);
        }
        context.put("flowParser", this);
        return context;
    }

}
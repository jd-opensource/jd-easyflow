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
import com.jd.easyflow.flow.filter.ExpFilter;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.action.ExpNodeAction;
import com.jd.easyflow.flow.model.action.FlowNodeAction;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;
import com.jd.easyflow.flow.model.post.ExpNodePostHandler;
import com.jd.easyflow.flow.model.post.FixedNodePostHandler;
import com.jd.easyflow.flow.model.pre.ExpNodePreHandler;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowParserImpl implements FlowParser {

    private static final Logger logger = LoggerFactory.getLogger(FlowParser.class);

    private static final String FLOW_STRING_KEY = "_flow_string";

    @Override
    public List<Flow> parse(String data) {
        List<Flow> flowList = parse(JsonUtil.parseObject(data, Map.class));
        flowList.get(0).setProperty(FLOW_STRING_KEY, data);
        return flowList;
    }

    @Override
    public List<Flow> parse(Map<String, Object> map) {
        List<Flow> flowList = new ArrayList<Flow>();
        parse(map, flowList, true);
        flowList.get(0).setProperty(FLOW_STRING_KEY, JsonUtil.toJsonString(map));
        return flowList;
    }

    @Override
    public List<Flow> parse(String data, boolean parseEl) {
        List<Flow> flowList = parse(JsonUtil.parseObject(data, Map.class), parseEl);
        flowList.get(0).setProperty(FLOW_STRING_KEY, data);
        return flowList;
    }

    @Override
    public List<Flow> parse(Map<String, Object> map, boolean parseEl) {
        List<Flow> flowList = new ArrayList<Flow>();
        parse(map, flowList, parseEl);
        flowList.get(0).setProperty(FLOW_STRING_KEY, JsonUtil.toJsonString(map));
        return flowList;
    }

    private Flow parse(Map<String, Object> map, List<Flow> flowList, boolean parseEl) {
        Flow flow = new Flow();
        flow.setFlowParser(this);
        flowList.add(flow);
        flow.setId((String) map.get(DefConstants.COMMON_PROP_ID));
        flow.setName((String) map.get(DefConstants.COMMON_PROP_NAME));
        // Parse property
        Map<String, Object> properties = (Map<String, Object>) map.get(DefConstants.COMMON_PROP_PROPERTIES);
        flow.putProperties(properties);
        // Parse node
        List<Map<String, Object>> nodeListConf = (List<Map<String, Object>>) map.get(DefConstants.FLOW_PROP_NODES);
        List<String> startNodeIdList = new ArrayList<String>();
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
                    Map<String, Object> context = new HashMap<>();
                    context.put("nodeDef", nodeConf);
                    context.put("flowParser", this);
                    FlowNode node = ElFactory.get().evalWithDefaultContext(exp, context, false);
                    flow.addNode(node);
                }
            } else {
                NodeImpl node = new NodeImpl();
                node.setId((String) nodeConf.get(DefConstants.COMMON_PROP_ID));
                node.setName((String) nodeConf.get(DefConstants.COMMON_PROP_NAME));
                node.putProperties((Map<String, Object>) nodeConf.get(DefConstants.COMMON_PROP_PROPERTIES));
                node.setPreHandler(parsePre(nodeConf.get(DefConstants.NODE_PROP_PRE), parseEl));
                node.setAction(parseAction(nodeConf.get(DefConstants.NODE_PROP_ACTION), flowList, parseEl));
                node.setPostHandler(parsePost(nodeConf.get(DefConstants.NODE_PROP_POST), parseEl));
                flow.addNode(node);
            }
        }
        // set start node.
        if (startNodeIdList.size() > 0) {
            flow.setStartNodeIds(startNodeIdList.toArray(new String[] {}));
        }
        // Listener
        parseListeners(map, flow, parseEl);
        // Filter
        parseFilters(map, flow, parseEl);
        // Node filter
        parseNodeFilter(map, flow, parseEl);
        // Node action filter
        parseNodeActionFilter(map, flow, parseEl);
        // Flow runner
        parseRunner(map, flow, parseEl);
        InitContext initContext = new InitContext();
        initContext.setFlowParser(this);
        initContext.setParseEl(parseEl);
        flow.init(initContext);
        return flow;
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
                            FlowEventListener eventListener = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            flow.getEventTrigger().addListener(eventListener);
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
                            Filter flowFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            flow.addFilter(flowFilter);
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
                            Filter flowFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            flow.addNodeFilter(flowFilter);
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
                            Filter nodeActionFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            flow.addNodeActionFilter(nodeActionFilter);
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
                        FlowRunner flowRunner = ElFactory.get().evalWithDefaultContext(exp, null, false);
                        flow.setRunner(flowRunner);
                    }
                }
            }
        }
    }

    @Override
    public NodePreHandler parsePre(Object preParam, boolean parseEl) {
        if (preParam == null) {
            return null;
        }
        if (preParam instanceof String) {
            ExpNodePreHandler handler = new ExpNodePreHandler();
            handler.setExp((String) preParam);
            return handler;
        }
        Map<String, Object> pre = (Map<String, Object>) preParam;
        String type = (String) pre.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return null;
            }
            String exp = (String) pre.get(DefConstants.COMMON_PROP_CREATE_EXP);
            NodePreHandler handler = ElFactory.get().evalWithDefaultContext(exp, null, false);
            return handler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_EXP)) {
            String exp = (String) pre.get(DefConstants.COMMON_PROP_EXP);
            ExpNodePreHandler handler = new ExpNodePreHandler();
            handler.setExp(exp);
            return handler;
        }
        throw new IllegalArgumentException("Param illegal " + pre);

    }

    @Override
    public NodeAction parseAction(Object actionParam, List<Flow> flowList, boolean parseEl) {
        if (actionParam == null) {
            return null;
        }
        if (actionParam instanceof String) {
            ExpNodeAction nodeAction = new ExpNodeAction();
            nodeAction.setExp((String) actionParam);
            return nodeAction;
        }
        Map<String, Object> action = (Map<String, Object>) actionParam;
        String type = (String) action.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || action.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return null;
            }
            String exp = (String) action.get(DefConstants.COMMON_PROP_CREATE_EXP);
            NodeAction nodeAction = ElFactory.get().evalWithDefaultContext(exp, null, false);
            return nodeAction;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || action.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpNodeAction nodeAction = new ExpNodeAction();
            String exp = (String) action.get(DefConstants.COMMON_PROP_EXP);
            nodeAction.setExp(exp);
            return nodeAction;
        } else if (DefConstants.COMMON_PROP_FLOW.equals(type) || action.containsKey(DefConstants.COMMON_PROP_FLOW)
                || action.containsKey(DefConstants.COMMON_PROP_FLOW_ID)) {
            FlowNodeAction nodeAction = new FlowNodeAction();
            if (action.containsKey(DefConstants.COMMON_PROP_FLOW)) {
                Flow flow = parse((Map<String, Object>) action.get(DefConstants.COMMON_PROP_FLOW), flowList, parseEl);
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
            return nodeAction;
        }
        throw new IllegalArgumentException("Param illegal " + action);
    }

    @Override
    public NodePostHandler parsePost(Object postParam, boolean parseEl) {
        if (postParam == null) {
            return null;
        }
        if (postParam instanceof String) {
            ExpNodePostHandler postHandler = new ExpNodePostHandler();
            String exp = (String) postParam;
            postHandler.setExp(exp);
            return postHandler;
        }
        Map<String, Object> post = (Map<String, Object>) postParam;
        String type = (String) post.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || post.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return null;
            }
            String exp = (String) post.get(DefConstants.COMMON_PROP_CREATE_EXP);
            NodePostHandler postHandler = ElFactory.get().evalWithDefaultContext(exp, null, false);
            return postHandler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || post.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpNodePostHandler postHandler = new ExpNodePostHandler();
            String exp = (String) post.get(DefConstants.COMMON_PROP_EXP);
            postHandler.setExp(exp);
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
                        if (parseEl) {
                            String createExp = (String) ((Map<String, Object>) value)
                                    .get(DefConstants.COMMON_PROP_CREATE_EXP);
                            condition.put(DefConstants.NODE_POST_PROP_WHEN,
                                    ElFactory.get().evalWithDefaultContext(createExp, null, false));
                        }
                    }
                }
            }

            String conditionType = (String) post.get(DefConstants.NODE_POST_PROP_CONDITION_TYPE);
            Object defaultBranch = post.get(DefConstants.NODE_POST_PROP_DEFAULT_TO);

            ConditionalNodePostHandler postHandler = new ConditionalNodePostHandler(conditionType, conditionList,
                    defaultBranch);
            return postHandler;

        } else if (DefConstants.NODE_POST_TYPE_FIXED.equals(type) || post.containsKey(DefConstants.NODE_POST_PROP_TO)) {
            Object nextStartId = post.get(DefConstants.NODE_POST_PROP_TO);
            FixedNodePostHandler postHandler = new FixedNodePostHandler(nextStartId);
            return postHandler;
        }
        throw new IllegalArgumentException("Param illegal " + post);
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

}

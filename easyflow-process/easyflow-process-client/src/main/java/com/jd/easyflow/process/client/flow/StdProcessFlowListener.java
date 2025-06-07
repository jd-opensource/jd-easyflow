package com.jd.easyflow.process.client.flow;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.event.BaseFlowEventListener;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.model.pre.NodePrePropertyGetter;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.FlowNodeLinkUtil;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExport;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessInstanceReq;
import com.jd.easyflow.process.client.common.PropertiesUtil;
import com.jd.easyflow.process.client.fsm.StdFsmProcessConstants;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeService;
import com.jd.easyflow.process.client.runtime.StdFlowConstants;
import com.jd.easyflow.process.client.runtime.StdNode;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcess;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * 
 * @author liyuliang5
 * 
 */
public class StdProcessFlowListener extends BaseFlowEventListener {
    
    private static final Logger log = LoggerFactory.getLogger(StdProcessFlowListener.class);


    private static final Pair<String, Integer>[] DEFAULT_ACCEPTED_EVENTS = new Pair[] {
            Pair.of(FlowEventTypes.FLOW_ENGINE_START, FlowConstants.EVENT_ORDER_START + 100),
            Pair.of(FlowEventTypes.FLOW_START, FlowConstants.EVENT_ORDER_START + 100),
            Pair.of(FlowEventTypes.NODE_START, FlowConstants.EVENT_ORDER_START + 100),
            Pair.of(FlowEventTypes.NODE_END, -FlowConstants.EVENT_ORDER_START - 100),
            Pair.of(FlowEventTypes.FLOW_END, -FlowConstants.EVENT_ORDER_START - 100),
            Pair.of(FlowEventTypes.FLOW_COMPLETE, -FlowConstants.EVENT_ORDER_START - 100) };
   

    @Autowired
    private ProcessRuntimeService processRuntimeService;

    private ProcessInstanceExport processInstanceExport;
    
    public StdProcessFlowListener() {
        acceptedEvents = DEFAULT_ACCEPTED_EVENTS;
    }
    
    @Override
    public void on(FlowEvent flowEvent) {
        switch (flowEvent.getType()) {
        case FlowEventTypes.FLOW_ENGINE_START: {
            onFlowEngineStart(flowEvent);
            break;
        }
        case FlowEventTypes.FLOW_START: {
            onFlowStart(flowEvent);
            break;
        }
        case FlowEventTypes.NODE_START: {
            onNodeStart(flowEvent);
            break;
        }
        case FlowEventTypes.NODE_END: {
            onNodeEnd(flowEvent);
            break;
        }
        case FlowEventTypes.FLOW_END: {
            onFlowEnd(flowEvent);
            break;
        }
        case FlowEventTypes.FLOW_COMPLETE: {
            onFlowComplete(flowEvent);
            break;
        }
        default:
            break;
        }

    }

    public void onFlowEngineStart(FlowEvent event) {
        FlowParam param = event.getMapData("param");
        FlowEngine flowEngine = (FlowEngine) event.getMapData("flowEngine");
        onFlowEngineStart(param, flowEngine);
    }

    public void onFlowEngineStart(FlowParam param, FlowEngine flowEngine) {
        if (param != null && param.getFlowId() == null) {
            String processType = param.get(StdFlowProcessConstants.FLOW_PARAM_PROCESS_TYPE);
            String bizNo = param.get(StdFlowProcessConstants.FLOW_PARAM_BIZNO);
            String instanceNo = param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCENO);
            ProcessInstanceDTO processInstanceDto = null;
            if (StringUtils.isNotEmpty(instanceNo)) {
                processInstanceDto = ExportResponseUtil
                        .unwrap(getProcessInstanceExport().getProcessInstance(new ExportRequest<>(instanceNo)));
            } else if (StringUtils.isNotEmpty(processType) && StringUtils.isNotEmpty(bizNo)) {
                QueryProcessInstanceReq instanceReq = QueryProcessInstanceReq.builder().processType(processType)
                        .bizNo(bizNo).build();
                processInstanceDto = ExportResponseUtil.unwrap(getProcessInstanceExport()
                        .queryProcessInstanceByProcessTypeAndBizNo(new ExportRequest<>(instanceReq)));
            }
            if (processInstanceDto != null) {
                String flowId = processInstanceDto.getProcessDefId().substring("FLOW-".length());
                if (flowEngine != null && (flowEngine instanceof StdFlowEngineImpl)) {
                    if (!flowId.contains(StdFlowProcessConstants.VERSION_PREFIX)) {
                        flowId = flowId + StdFlowProcessConstants.VERSION_PREFIX;
                    }
                }
                param.setFlowId(flowId);
            }
        }
    }

    public void onFlowStart(FlowEvent event) {
        onFlowStart(event.getContext());
    }

    public void onFlowStart(FlowContext context) {
        StdProcessContext processContext = new StdProcessContext();
        context.put(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX, processContext);
        processContext.setEngineProcessContext(context);
        
        processContext.setProcessParamProperties(context.getParam().get(StdFlowProcessConstants.FLOW_PROP_PROCESS));
        StdProcess process = new StdProcess();
        process.setProcessProperties(context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS));
        process.putExtProperty(StdProcessConstants.EXT_PROP_ENGINE, StdProcessConstants.ENGINE_FLOW);
        processContext.setProcess(process);
        fillProcessContextData(context);
        processContext.setSubProcess(Boolean.TRUE.equals(context.get(StdFlowProcessConstants.FLOW_CTX_IS_SUB_PROCESS)));
        if (processContext.isSubProcess() && Boolean.TRUE.equals(PropertiesUtil
                .get(processContext.getProcess().getProcessProperties(), StdProcessConstants.PROP_DATA_FLUSH_BY_PARENT))) {
            StdProcessContext parentProcessContext = context.get(StdFlowProcessConstants.FLOW_CTX_PARENT_CTX);
            processContext.setCache(parentProcessContext.getCache());
        }
        processContext.setOpType(context.get(StdFlowProcessConstants.FLOW_CTX_OP_TYPE));
        ProcessInstanceDTO instance = new ProcessInstanceDTO();
        fillProcessInstanceField(instance, context);
        processContext.setProcessType(instance.getProcessType());
        processContext.setBizNo(instance.getBizNo());
        String requestId = processRuntimeService.lockProcessInstance(instance.getProcessType(), instance.getBizNo());
        processContext.setLockRequestId(requestId);
        Boolean checkStartNode = PropertiesUtil.get(
                context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                StdProcessConstants.PROP_CHECK_START_NODE);
        checkStartNode = checkStartNode == null || checkStartNode;
        processContext.setCheckStartNode(checkStartNode);
        
        processContext.setProcessProperties(assembleProcessProperties(context));
        String processDefId = StdProcessConstants.PROCESS_DEF_FLOW + context.getFlowId();
        if (context.getFlowEngine() instanceof StdFlowEngineImpl) {
            if (processDefId.endsWith(StdFlowProcessConstants.VERSION_PREFIX)) {
                processDefId = processDefId.substring(0,
                        processDefId.length() - StdFlowProcessConstants.VERSION_PREFIX.length());
            }
        }
        instance.setProcessDefId(processDefId);
        processContext.setNodeFunction(nodeId -> {
            StdNode node = new StdNode();
            node.setProcessProperties(
                    context.getFlow().getNode(nodeId).getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS));
            return node;
        });

        processContext.setStartNodesFunction(processInstance -> {
            if (processInstance != null && StdProcessConstants.STATUS_CLOSE.equals(processInstance.getStatus())) {
                String exeucteClosePoliy = PropertiesUtil.get(
                        context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                        StdProcessConstants.PROP_EXECUTE_CLOSE_POLICY);
                if (StdProcessConstants.POLICY_EMPTY_RUN.equals(exeucteClosePoliy)) {
                    log.info("Process instance finish, empty run");
                    context.getParam().setNodeIds(new String[] {});
                    return new ArrayList<String>();
                }
            }
            if (ArrayUtils.isNotEmpty(context.getParam().getNodeIds())) {
                return Arrays.asList(context.getParam().getNodeIds());
            } else if (processInstance != null) {
                Set<String> openNodeIds = processRuntimeService.getManager().findOpenNodeIds(processContext);
                if (openNodeIds.size() > 0) {
                    context.getParam().setNodeIds(openNodeIds.toArray(new String[] {}));
                    return new ArrayList<String>(openNodeIds);
                } else {
                    List<String> startNodeList = (List<String>) processContext.getProcessProperties()
                            .get(StdProcessConstants.PROP_START_NODE_IDS);
                    context.getParam().setNodeIds(startNodeList.toArray(new String[] {}));
                    return startNodeList;
                }
            } else {
                List<String> startNodeList = (List<String>) processContext.getProcessProperties()
                        .get(StdProcessConstants.PROP_START_NODE_IDS);
                context.getParam().setNodeIds(startNodeList.toArray(new String[] {}));
                return startNodeList;
            }
        });
        processContext.setVariableSetter(variables -> {
            context.put(StdProcessConstants.CTX_VARIABLES, variables);
        });
        processContext.setVariableGetter(() -> {
            return (Map<String, String>) context.get(StdProcessConstants.CTX_VARIABLES);
        });
        processContext.setNodeVariableGetter(stdNodeContext -> {
            NodeContext nodeContext = stdNodeContext.getEngineNodeContext();
            return nodeContext.get(StdProcessConstants.NODE_CTX_VARIABLES);
        });
        processContext.setNodeVariableSetter(pair -> {
            StdNodeContext stdNodeContext = pair.getLeft();
            NodeContext nodeContext = stdNodeContext.getEngineNodeContext();
            nodeContext.put(StdProcessConstants.NODE_CTX_VARIABLES, pair.getRight());
        });
        processContext.setEventTriggerFunction(param -> {
            Object[] params = (Object[]) param;
            context.getFlow().triggerEvent((String) params[0], params[1], context, false);
            return null;
        });
        processContext.setNodeStartEventPolicy(StdProcessConstants.NODE_START_EVENT_POLICY_ACTIVE);
        processRuntimeService.processStartExec(instance, processContext);
        ProcessInstanceDTO processInstance = processRuntimeService.getProcessInstance(processContext);
        context.put(StdFlowProcessConstants.FLOW_CTX_INSTANCE, processInstance);
    }

    private void fillProcessContextData(FlowContext context) {
        FlowParam param = context.getParam();
        if (context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCENO) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCENO) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_INSTANCENO,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCENO));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCE_NAME) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCE_NAME) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_INSTANCE_NAME,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCE_NAME));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_BIZNO) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_BIZNO, param.get(StdFlowProcessConstants.FLOW_PARAM_BIZNO));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_TYPE) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_PROCESS_TYPE) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_PROCESS_TYPE,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_PROCESS_TYPE));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_USER) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_USER) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_USER, param.get(StdFlowProcessConstants.FLOW_PARAM_USER));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_PRODUCT_CODE) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_PRODUCT_CODE));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_KEY_FIELD) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_KEY_FIELD));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD2) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_KEY_FIELD2) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD2,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_KEY_FIELD2));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_DATA) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_BIZ_DATA) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_BIZ_DATA,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_BIZ_DATA));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_STATUS) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_BIZ_STATUS) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_BIZ_STATUS,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_BIZ_STATUS));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_OP_TYPE) == null
                && param.get(StdFlowProcessConstants.FLOW_PARAM_OP_TYPE) != null) {
            context.put(StdFlowProcessConstants.FLOW_CTX_OP_TYPE,
                    param.get(StdFlowProcessConstants.FLOW_PARAM_OP_TYPE));
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO) == null) {
            String parentInstanceNoOfParam = param.get(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO);
            if (parentInstanceNoOfParam != null) {
                context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO, parentInstanceNoOfParam);
            } else if (context.getParentContext() != null) {
                StdProcessContext stdProcessContext = context.getParentContext().get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
                if (stdProcessContext != null) {
                    context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO,
                            stdProcessContext.getInstanceNo());
                    context.put(StdFlowProcessConstants.FLOW_CTX_IS_SUB_PROCESS, true);
                    context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_CTX, stdProcessContext);
                }
            }    else {
                Object parentContext = context.get(FlowConstants.CTX_PARENT_CONTEXT);
                if (parentContext != null) {
                    if (parentContext instanceof FlowContext) {
                        StdProcessContext stdProcessContext = ((FlowContext) parentContext)
                                .get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
                        if (stdProcessContext != null) {
                            context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO,
                                    stdProcessContext.getInstanceNo());
                            context.put(StdFlowProcessConstants.FLOW_CTX_IS_SUB_PROCESS, true);
                            context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_CTX, stdProcessContext);
                        }
                    } else if (parentContext instanceof FsmContext) {
                        StdProcessContext stdProcessContext = ((FsmContext) parentContext)
                                .getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
                        if (stdProcessContext != null) {
                            context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO,
                                    stdProcessContext.getInstanceNo());
                            context.put(StdFlowProcessConstants.FLOW_CTX_IS_SUB_PROCESS, true);
                            context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_CTX, stdProcessContext);
                        }
                    } else {
                        throw new IllegalArgumentException("Parent context class type error," + parentContext.getClass());
                    }
                }
            }
        }
        if (context.get(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO) == null) {
            String parentNodeInstanceNoOfParam = param.get(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO);
            if (parentNodeInstanceNoOfParam != null) {
                context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO, parentNodeInstanceNoOfParam);
            } else if (context.getParentNodeContext() != null) {
                NodeContext parentNodeContext = context.getParentNodeContext();
                StdNodeContext stdNodeContext = parentNodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
                if (stdNodeContext != null) {
                    context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO,
                            stdNodeContext.getNodeInstanceNo());
                }
            } else {
                NodeContext parentNodeContext = context.get(FlowConstants.CTX_PARENT_NODE_CONTEXT);
                if (parentNodeContext != null) {
                    StdNodeContext stdNodeContext = parentNodeContext
                            .get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
                    if (stdNodeContext != null) {
                        context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO,
                                stdNodeContext.getNodeInstanceNo());
                    }
                } else {
                    TransitionContext fsmParentTransitionContext = context.get(FsmConstants.CTX_PARENT_TRANSITION_CONTEXT);
                    if (fsmParentTransitionContext != null) {
                        StdNodeContext stdNodeContext = fsmParentTransitionContext
                                .get(StdFsmProcessConstants.FSM_TST_CTX_NODE_CTX);
                        if (stdNodeContext != null) {
                            context.put(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO,
                                    stdNodeContext.getNodeInstanceNo());
                        }
                    }
                }
            }
        }
        

        
    }

    private void fillProcessInstanceField(ProcessInstanceDTO instance, FlowContext context) {
        instance.setInstanceNo(context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCENO));
        instance.setInstanceName(context.get(StdFlowProcessConstants.FLOW_CTX_INSTANCE_NAME));
        instance.setBizNo(context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO));
        instance.setProcessType(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_TYPE));
        instance.setCreator(context.get(StdFlowProcessConstants.FLOW_CTX_USER));
        instance.setProductCode(context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE));
        instance.setKeyField(context.get(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD));
        instance.setKeyField2(context.get(StdFlowProcessConstants.FLOW_CTX_KEY_FIELD2));
        instance.setBizData(context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_DATA));
        instance.setBizStatus(context.get(StdFlowProcessConstants.FLOW_CTX_BIZ_STATUS));
        instance.setParentInstanceNo(context.get(StdFlowProcessConstants.FLOW_CTX_PARENT_INSTANCE_NO));
        instance.setParentNodeInstanceNo(context.get(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO));
    }

    public void onNodeStart(FlowEvent event) {
        com.jd.easyflow.flow.model.NodeContext flowNodeContext = (com.jd.easyflow.flow.model.NodeContext) event
                .getData();
        onNodeStart(flowNodeContext, event.getContext());
    }

    public void onNodeStart(NodeContext flowNodeContext, FlowContext context) {
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        String currentNodeId = flowNodeContext.getNodeId();
        NodeContext previousNodeCtx = flowNodeContext.getPreviousNode();
        StdNodeContext nodeContext = new StdNodeContext();
        nodeContext.setStdProcessContext(processContext);
        nodeContext.setNodeId(currentNodeId);
        nodeContext.setEngineNodeContext(flowNodeContext);
        flowNodeContext.put(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX, nodeContext);
        FlowNode currentNode = context.getFlow().getNode(nodeContext.getNodeId());
        StdNode node = new StdNode();
        node.setProcessProperties(currentNode.getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS));
        nodeContext.setNode(node);

        nodeContext
                .setPreviousNodeId(previousNodeCtx == null || previousNodeCtx.getNodeId().equals(currentNodeId) ? null
                        : previousNodeCtx.getNodeId());
        nodeContext.setExecutionStartTime(new Date());
        nodeContext.setExtData(flowNodeContext.get(StdFlowProcessConstants.FLOW_NODE_CTX_PROCESS_EXT_DATA));

        List<String> configPreNodes = null;
        if (currentNode instanceof NodeImpl) {
            NodePreHandler preHandler = ((NodeImpl) currentNode).getPreHandler();
            if (preHandler != null && preHandler instanceof NodePrePropertyGetter) {
                configPreNodes = ((NodePrePropertyGetter) preHandler).getPreNodes(flowNodeContext, context);
            }
        }
        if (configPreNodes == null) {
            configPreNodes = FlowNodeLinkUtil.getPreCheckNodes(currentNode, context.getFlow());
        }
        String preCheckType = FlowNodeLinkUtil.getPreCheckType(flowNodeContext.getNodeId(), context.getFlow());
        if (configPreNodes != null && configPreNodes.size() > 0 && (preCheckType == null || FlowNodeLinkUtil.NODE_PRE_CHECK_TYPE_UNKNOWN.equals(preCheckType))) {
            preCheckType = FlowConstants.NODE_PRE_CHECK_TYPE_MULTICHECK;
        }
        
        nodeContext.setConfigPreNodeIds(configPreNodes);
        nodeContext.setPreCheckType(preCheckType);
        processRuntimeService.nodeStartExec(nodeContext, processContext);
        if (StringUtils.isNotEmpty(preCheckType)) {
            flowNodeContext.put(StdFlowConstants.NODECTX_PRE_RESULT, nodeContext.getPreResult());
        }
    }

    public void onNodeEnd(FlowEvent event) {
        com.jd.easyflow.flow.model.NodeContext flowNodeContext = (com.jd.easyflow.flow.model.NodeContext) event
                .getData();
        onNodeEnd(flowNodeContext, event.getContext());
    }

    public void onNodeEnd(NodeContext nodeCtx, FlowContext context) {
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        StdNodeContext nodeContext = nodeCtx.get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
        com.jd.easyflow.flow.model.NodeContext[] nextNodes = nodeCtx.getNextNodes();
        String[] nextNodeIds = null;
        if (nextNodes != null && nextNodes.length > 0) {
            nextNodeIds = new String[nextNodes.length];
            for (int i = 0; i < nextNodes.length; i++) {
                nextNodeIds[i] = nextNodes[i].getNodeId();
            }
        }
        nodeContext.setNextNodeIds(nextNodeIds);
        if (processContext.getBizNo() == null) {
            processContext.setBizNo(context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO));
        }
        if (context.isInterrupted()) {
            processContext.setInterrupted();
        }
        processRuntimeService.nodeEndExec(nodeContext, processContext);
    }

    public void onFlowEnd(FlowEvent event) {
        onFlowEnd(event.getContext());
    }

    public void onFlowEnd(FlowContext context) {
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        if (context.isInterrupted()) {
            processContext.setInterrupted();
        }
        processRuntimeService.processEndExec(processContext);
    }

    public void onFlowComplete(FlowEvent event) {
        onFlowComplete(event.getContext());
    }

    public void onFlowComplete(FlowContext context) {
        StdProcessContext processContext = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
        processRuntimeService.processCompleteExec(processContext);
    }

    private Map<String, Object> assembleProcessProperties(FlowContext context) {
        Map<String, Object> properties = new ConcurrentHashMap<String, Object>();
        Map<String, Object> ctxProperties = context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS);
        if (ctxProperties != null) {
            properties.putAll(ctxProperties);
        }
        String dataFlushPolicy = initDataFlushPolicy(context);
        properties.put(StdProcessConstants.PROP_DATA_FLUSH_POLICY, dataFlushPolicy);

        boolean checkFlushNodes = Boolean.TRUE
                .equals(PropertiesUtil.get(context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                        StdProcessConstants.PROP_CHECK_FLUSH_NODES));
        properties.put(StdProcessConstants.PROP_CHECK_FLUSH_NODES, checkFlushNodes);
        if (checkFlushNodes) {
            String[] flushNodes = initFlushNodes(context);
            PropertiesUtil.put(properties, StdProcessConstants.PROP_FLUSH_NODES, flushNodes);
        }
        initStartAndEndNodeIds(context, properties);

        Map<String, Object> processInstanceStatusMessage = PropertiesUtil.get(
                context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                StdProcessConstants.PROP_PROCESS_INSTANCE_STATUS_MESSAGE);
        if (processInstanceStatusMessage != null) {
            properties.put(StdProcessConstants.PROP_PROCESS_INSTANCE_STATUS_MESSAGE, processInstanceStatusMessage);
        }
        Map<String, Object> nodeInstanceStatusMessage = PropertiesUtil.get(
                context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                StdProcessConstants.PROP_NODE_INSTANCE_STATUS_MESSAGE);
        if (nodeInstanceStatusMessage != null) {
            properties.put(StdProcessConstants.PROP_NODE_INSTANCE_STATUS_MESSAGE, nodeInstanceStatusMessage);
        }
        if (log.isDebugEnabled()) {
            log.debug("Assemble process context properties:" + properties);
        }
        return properties;
    }

    private String initDataFlushPolicy(FlowContext context) {
        String flushPolicy = PropertiesUtil.get(context.get(StdFlowProcessConstants.FLOW_CTX_PROCESS),
                StdProcessConstants.PROP_DATA_FLUSH_POLICY);
        if (StringUtils.isNotEmpty(flushPolicy)) {
            return flushPolicy;
        }
        flushPolicy = PropertiesUtil.get(context.getParam().get(StdFlowProcessConstants.FLOW_PARAM_PROCESS),
                StdProcessConstants.PROP_DATA_FLUSH_POLICY);
        if (StringUtils.isNotEmpty(flushPolicy)) {
            return flushPolicy;
        }
        flushPolicy = PropertiesUtil.get(context.getFlow().getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                StdProcessConstants.PROP_DATA_FLUSH_POLICY);
        if (StringUtils.isNotEmpty(flushPolicy)) {
            return flushPolicy;
        }
        flushPolicy = StdProcessConstants.FLUSH_AFTER_PROCESS;
        return flushPolicy;

    }

    private String[] initFlushNodes(FlowContext context) {
        List<String> list = new ArrayList<String>();
        context.getFlow().getNodeList().forEach(node -> {
            if (Boolean.TRUE.equals(PropertiesUtil.get(node.getProperty(StdFlowProcessConstants.FLOW_PROP_PROCESS),
                    StdProcessConstants.PROP_FLUSH))) {
                list.add(node.getId());
            }
        });
        return list.toArray(new String[] {});
    }

    private void initStartAndEndNodeIds(FlowContext context, Map<String, Object> properties) {
        List<String> startNodeIds = Optional.ofNullable(context.getFlow().getStartNodeIds())
                .map(ids -> Arrays.asList(ids)).orElse(new ArrayList<>());
        List<String> endNodeIds = new ArrayList<String>();
        context.getFlow().getNodeList().forEach(node -> {
            if (Boolean.TRUE.equals(node.getProperty(StdProcessConstants.PROP_END))) {
                endNodeIds.add(node.getId());
            }
        });
        properties.put(StdProcessConstants.PROP_START_NODE_IDS, startNodeIds);
        properties.put(StdProcessConstants.PROP_END_NODE_IDS, endNodeIds);
    }

    private String getLockKey(String processType, String bizNo) {
        return processType + "_" + bizNo;
    }

    public ProcessRuntimeService getProcessRuntimeService() {
        return processRuntimeService;
    }

    public void setProcessRuntimeService(ProcessRuntimeService processRuntimeService) {
        this.processRuntimeService = processRuntimeService;
    }

    private ProcessInstanceExport getProcessInstanceExport() {
        if (processInstanceExport == null) {
            processInstanceExport = ObjectFactorys.getDefault().getObject(ProcessInstanceExport.class);
        }
        return processInstanceExport;
    }
    
    @Override
    public void init(InitContext initContext, Object parent) {}
    
    @Override
    public void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}
    
    @Override
    public void destroy() {}

}

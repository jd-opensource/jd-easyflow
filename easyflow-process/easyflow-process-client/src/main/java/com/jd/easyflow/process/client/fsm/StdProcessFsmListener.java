package com.jd.easyflow.process.client.fsm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.event.FsmEvent;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.model.InitContext;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.fsm.util.FsmEventTypes;
import com.jd.easyflow.fsm.util.Pair;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExport;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessInstanceReq;
import com.jd.easyflow.process.client.common.PropertiesUtil;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeService;
import com.jd.easyflow.process.client.runtime.StdNode;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcess;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;
import com.jd.easyflow.utils.json.JSON;

/**
 *
 * @author liyuliang5
 * 
 */
public class StdProcessFsmListener implements FsmEventListener {
    
    private static final Logger log = LoggerFactory.getLogger(StdProcessFsmListener.class);

    private ProcessRuntimeService processRuntimeService;

    private ProcessInstanceExport processInstanceExport;

    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return new Pair[] { Pair.of(FsmEventTypes.FSM_MANAGER_START, FsmConstants.EVENT_ORDER_START),
                Pair.of(FsmEventTypes.FSM_START, FsmConstants.EVENT_ORDER_START + 100),
                Pair.of(FsmEventTypes.FSM_END, -FsmConstants.EVENT_ORDER_START + 100),
                Pair.of(FsmEventTypes.FSM_COMPLETE, FsmConstants.EVENT_ORDER_START - 100),
                Pair.of(FsmEventTypes.TST_START, FsmConstants.EVENT_ORDER_START + 100),
                Pair.of(FsmEventTypes.TST_END, -FsmConstants.EVENT_ORDER_START - 100) };
    }

    @Override
    public void on(FsmEvent event) {
        switch (event.getType()) {
        case FsmEventTypes.FSM_MANAGER_START: {
            onFsmManagerStart(event);
            break;
        }
        case FsmEventTypes.FSM_START: {
            onFsmStart(event);
            break;
        }
        case FsmEventTypes.TST_START: {
            onTstStart(event);
            break;
        }
        case FsmEventTypes.TST_END: {
            onTstEnd(event);
            break;
        }
        case FsmEventTypes.FSM_END: {
            onFsmEnd(event);
            break;
        }
        case FsmEventTypes.FSM_COMPLETE: {
            onFsmComplete(event);
            break;
        }
        default:
            break;
        }

    }
    
    private void onFsmStart(FsmEvent event) {
        onFsmStart(event.getContext());
    }

    protected void onFsmStart(FsmContext context) {
        StdProcessContext processContext = new StdProcessContext();
        context.putData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX, processContext);
        processContext.setEngineProcessContext(context);

        processContext.setProcessParamProperties(context.getParam().get(StdFsmProcessConstants.FSM_PROP_PROCESS));
        StdProcess process = new StdProcess();
        process.setProcessProperties(context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS));
        process.putExtProperty(StdProcessConstants.EXT_PROP_ENGINE, StdProcessConstants.ENGINE_FSM);
        processContext.setProcess(process);
        fillFsmContextData(context);
        processContext.setSubProcess(Boolean.TRUE.equals(context.getData(StdFsmProcessConstants.FSM_CTX_IS_SUB_PROCESS)));
        if (processContext.isSubProcess() && Boolean.TRUE.equals(PropertiesUtil
                .get(processContext.getProcess().getProcessProperties(), StdProcessConstants.PROP_DATA_FLUSH_BY_PARENT))) {
            StdProcessContext parentProcessContext = context.getData(StdFsmProcessConstants.FSM_CTX_PARENT_CTX);
            processContext.setCache(parentProcessContext.getCache());
        }
        processContext.setOpType(context.getData(StdFsmProcessConstants.FSM_CTX_OP_TYPE));
        ProcessInstanceDTO instance = new ProcessInstanceDTO();
        instance.setInstanceNo(context.getData(StdFsmProcessConstants.FSM_CTX_INSTANCENO));
        instance.setInstanceName(context.getData(StdFsmProcessConstants.FSM_CTX_INSTANCE_NAME));
        instance.setBizNo(context.getParam().getInstanceId());
        instance.setProcessType(context.getParam().getBizType());
        instance.setCreator(context.getData(StdFsmProcessConstants.FSM_CTX_USER));
        instance.setProductCode(context.getData(StdFsmProcessConstants.FSM_CTX_PRODUCT_CODE));
        instance.setParentInstanceNo(context.getData(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO));
        instance.setParentNodeInstanceNo(context.getData(StdFsmProcessConstants.FSM_CTX_PARENT_NODE_INSTANCE_NO));
        String processDefId = StdProcessConstants.PROCESS_DEF_FSM + context.getFsm().getId();
        if (processDefId.endsWith(StdFsmProcessConstants.VERSION_PREFIX)) {
            processDefId = processDefId.substring(0, processDefId.length() - StdFsmProcessConstants.VERSION_PREFIX.length());
        }
        instance.setProcessDefId(processDefId);      
        instance.setBizData(context.getData(StdFsmProcessConstants.FSM_CTX_BIZ_DATA));
        instance.setBizStatus(context.getData(StdFsmProcessConstants.FSM_CTX_BIZ_STATUS));
        processContext.setProcessType(instance.getProcessType());
        processContext.setBizNo(instance.getBizNo());

        String requestId = processRuntimeService.lockProcessInstance(instance.getProcessType(), instance.getBizNo());
        processContext.setLockRequestId(requestId);

        processContext.setProcessProperties(assembleProcessProperties(context));

        Boolean checkStartNode = PropertiesUtil.get(StdProcessConstants.PROP_CHECK_START_NODE,
                context.getData(StdFsmProcessConstants.FSM_PROP_PROCESS),
                context.getParam().get(StdFsmProcessConstants.FSM_PROP_PROCESS),
                context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS)
                );
        checkStartNode = checkStartNode != null && checkStartNode;
        processContext.setCheckStartNode(checkStartNode);

        processContext.setNodeFunction(nodeId -> {
            StdNode node = new StdNode();
            node.setProcessProperties(
                    context.getFsm().getState(nodeId).getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS));
            return node;
        });

        processContext.setStartNodesFunction((processInstance -> {
            if (processInstance != null && StdProcessConstants.STATUS_CLOSE.equals(processInstance.getStatus())) {
                String exeucteClosePoliy = PropertiesUtil.get(
                        context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS),
                        StdProcessConstants.PROP_EXECUTE_CLOSE_POLICY);
                if (StdProcessConstants.POLICY_EMPTY_RUN.equals(exeucteClosePoliy)) {
                    log.info("Process instance finish, empty run.");
                    context.getParam().setEventId(StdFsmProcessConstants.EVENT_NONE_FOR_CLOSE);
                }
            }
            
            if (context.getCurrentEvent() == null) {
                if (processInstance == null || processInstance.getCurrentNodeIds() == null || processInstance.getCurrentNodeIds().isEmpty()) {
                    context.setCurrentEvent(FsmConstants.COMMON_ENTER_EVENT);
                } else {
                    context.setCurrentEvent(FsmConstants.COMMON_CHECK_EVENT);
                }
            }
            
            if (context.getCurrentState() != null) {
                return Arrays.asList(context.getCurrentState().getId());
            }
            if (context.getParam().getCurrentStateId() != null) {
                return Arrays.asList(context.getCurrentState().getId());
            }
            if (processInstance == null) {
                return context.getFsm().getStartStateId() == null ? new ArrayList<String>() : Arrays.asList(context.getFsm().getStartStateId());
            } else {
                Set<String> activeNodeIds = processRuntimeService.getManager().findOpenNodeIds(processContext);
                if (activeNodeIds != null && ! activeNodeIds.isEmpty()) {
                    if (activeNodeIds.size() > 1) {
                        throw new IllegalStateException("Fsm can only have one active node, " + activeNodeIds);
                    } else {
                        String currentStateId = activeNodeIds.iterator().next();
                        context.setCurrentState(context.getFsm().getState(currentStateId));
                        log.info("Start node of fsm set to:{}", currentStateId);
                        return Arrays.asList(currentStateId);
                    }
                } else {
                    return context.getFsm().getStartStateId() == null ? new ArrayList<String>() : Arrays.asList(context.getFsm().getStartStateId());
                }
            }

        }));

        processContext.setEventTriggerFunction(param -> {
            Object[] params = (Object[]) param;
            context.getFsm().getEventTrigger().triggerEvent((String) params[0], params[1], context, false);
            return null;
        });

        processContext.setVariableSetter(variables -> {
            context.putData(StdProcessConstants.CTX_VARIABLES, variables);
        });
        processContext.setVariableGetter(() -> {
            return (Map<String, String>) context.getData(StdProcessConstants.CTX_VARIABLES);
        });
        processContext.setNodeVariableGetter(stdNodeContext -> {
            TransitionContext nodeContext = stdNodeContext.getEngineNodeContext();
            return nodeContext.get(StdProcessConstants.NODE_CTX_VARIABLES);
        });
        processContext.setNodeVariableSetter(pair -> {
            StdNodeContext stdNodeContext = pair.getLeft();
            TransitionContext nodeContext = stdNodeContext.getEngineNodeContext();
            nodeContext.put(StdProcessConstants.NODE_CTX_VARIABLES, pair.getRight());
        });
        
        processContext.setNodeStartEventPolicy(StdProcessConstants.NODE_START_EVENT_POLICY_CREATE);
        processRuntimeService.processStartExec(instance, processContext);
        ProcessInstanceDTO processInstance = processRuntimeService.getProcessInstance(processContext);
        context.putData(StdFsmProcessConstants.FSM_CTX_INSTANCE, processInstance);
    }

    private void fillFsmContextData(FsmContext context) {
        FsmParam param = context.getParam();
        if (context.getData(StdFsmProcessConstants.FSM_CTX_INSTANCENO) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_INSTANCENO) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_INSTANCENO,
                    param.get(StdFsmProcessConstants.FSM_PARAM_INSTANCENO));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_INSTANCE_NAME) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_INSTANCE_NAME) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_INSTANCE_NAME,
                    param.get(StdFsmProcessConstants.FSM_PARAM_INSTANCE_NAME));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_USER) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_USER) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_USER, param.get(StdFsmProcessConstants.FSM_PARAM_USER));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_PRODUCT_CODE) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_PRODUCT_CODE) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_PRODUCT_CODE,
                    param.get(StdFsmProcessConstants.FSM_PARAM_PRODUCT_CODE));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_OP_TYPE) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_OP_TYPE) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_OP_TYPE,
                    param.get(StdFsmProcessConstants.FSM_PARAM_OP_TYPE));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_BIZ_DATA) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_BIZ_DATA) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_BIZ_DATA,
                    param.get(StdFsmProcessConstants.FSM_PARAM_BIZ_DATA));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_BIZ_STATUS) == null
                && param.get(StdFsmProcessConstants.FSM_PARAM_BIZ_STATUS) != null) {
            context.putData(StdFsmProcessConstants.FSM_CTX_BIZ_STATUS,
                    param.get(StdFsmProcessConstants.FSM_PARAM_BIZ_STATUS));
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO) == null) {
            String parentInstanceNoOfParam = param.get(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO);
            if (parentInstanceNoOfParam != null) {
                context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO, parentInstanceNoOfParam);
            } else {
                Object parentContext = context.getData(FsmConstants.CTX_PARENT_CONTEXT);
                if (parentContext != null) {
                    if (parentContext instanceof FsmContext) {
                        StdProcessContext stdProcessContext = ((FsmContext) parentContext)
                                .getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
                        if (stdProcessContext != null) {
                            context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO,
                                    stdProcessContext.getInstanceNo());
                            context.putData(StdFsmProcessConstants.FSM_CTX_IS_SUB_PROCESS, true);
                            context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_CTX, stdProcessContext);
                        }
                    } else if (parentContext instanceof FlowContext) {
                        StdProcessContext stdProcessContext = ((FlowContext) parentContext)
                                .get(StdFlowProcessConstants.FLOW_CTX_PROCESS_CTX);
                        if (stdProcessContext != null) {
                            context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_INSTANCE_NO,
                                    stdProcessContext.getInstanceNo());
                            context.putData(StdFsmProcessConstants.FSM_CTX_IS_SUB_PROCESS, true);
                            context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_CTX, stdProcessContext);
                        }
                    }  else {
                        throw new IllegalArgumentException("Parent context class type illegal," + parentContext.getClass());
                    }
                }
            }
        }
        if (context.getData(StdFsmProcessConstants.FSM_CTX_PARENT_NODE_INSTANCE_NO) == null) {
            String parentNodeInstanceNoOfParam = param.get(StdFsmProcessConstants.FSM_CTX_PARENT_NODE_INSTANCE_NO);
            if (parentNodeInstanceNoOfParam != null) {
                context.putData(StdFsmProcessConstants.FSM_CTX_PARENT_NODE_INSTANCE_NO, parentNodeInstanceNoOfParam);
            } else {
                TransitionContext fsmParentTransitionContext = context
                        .getData(FsmConstants.CTX_PARENT_TRANSITION_CONTEXT);
                if (fsmParentTransitionContext != null) {
                    StdNodeContext stdNodeContext = fsmParentTransitionContext
                            .get(StdFsmProcessConstants.FSM_TST_CTX_NODE_CTX);
                    if (stdNodeContext != null) {
                        context.putData(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO,
                                stdNodeContext.getNodeInstanceNo());
                    }
                } else {
                    NodeContext parentNodeContext = context.getData(FlowConstants.CTX_PARENT_NODE_CONTEXT);
                    if (parentNodeContext != null) {
                        StdNodeContext stdNodeContext = parentNodeContext
                                .get(StdFlowProcessConstants.FLOW_NODE_CTX_NODE_CTX);
                        if (stdNodeContext != null) {
                            context.putData(StdFlowProcessConstants.FLOW_CTX_PARENT_NODE_INSTANCE_NO,
                                    stdNodeContext.getNodeInstanceNo());
                        }
                    }
                }
            }

        }
    }
    
    private void onTstStart(FsmEvent event) {
        TransitionContext tstContext = (TransitionContext) event.getData();
        onTstStart(tstContext, event.getContext());
    }

    protected void onTstStart(TransitionContext tstContext, FsmContext context) {
        StdProcessContext processContext = context.getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
        String currentStateId = context.getCurrentState().getId();
        String currentEventId = context.getCurrentEvent().getId();
        State previousState = context.getPreviousState();
        StdNodeContext nodeContext = new StdNodeContext();
        nodeContext.setStdProcessContext(processContext);
        nodeContext.setNodeId(currentStateId);
        nodeContext.setEventId(currentEventId);
        State state = context.getFsm().getState(currentStateId);
        StdNode node = new StdNode();
        node.setProcessProperties(state.getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS));
        nodeContext.setNode(node);

        nodeContext.setPreviousNodeId(
                previousState == null || previousState.getId().equals(currentStateId) ? null : previousState.getId());
        nodeContext.setExecutionStartTime(new Date());
        nodeContext.setEngineNodeContext(tstContext);
        tstContext.put(StdFsmProcessConstants.FSM_TST_CTX_NODE_CTX, nodeContext);
        Map<String, Object> processExtData = tstContext.get(StdFsmProcessConstants.FSM_TST_CTX_PROCESS_EXT_DATA);
        nodeContext.setExtData(processExtData);
        processRuntimeService.nodeStartExec(nodeContext, processContext);
    }
    
    private void onTstEnd(FsmEvent event) {
        TransitionContext tstContext = (TransitionContext) event.getData();
        onTstEnd(tstContext, event.getContext());        
    }

    protected void onTstEnd(TransitionContext tstContext, FsmContext context) {
        StdProcessContext processContext = context.getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
        StdNodeContext nodeContext = tstContext.get(StdFsmProcessConstants.FSM_TST_CTX_NODE_CTX);
        String currentStateId = context.getCurrentState().getId();
        nodeContext.setNextNodeIds(
                Objects.equals(currentStateId, nodeContext.getNodeId()) ? null : new String[] { currentStateId });
        nodeContext.setActionResult(tstContext.getActionResult());
        if (processContext.getBizNo() == null) {
            processContext.setBizNo(context.getStateInstanceId());
        }
        if (context.isInterrupted()) {
            processContext.setInterrupted();
        }
        processRuntimeService.nodeEndExec(nodeContext, processContext);
    }
    
    private void onFsmEnd(FsmEvent event) {
        onFsmEnd(event.getContext());
    }

    protected void onFsmEnd(FsmContext context) {
        StdProcessContext processContext = context.getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
        if (context.isInterrupted()) {
            processContext.setInterrupted();
        }
        processRuntimeService.processEndExec(processContext);
    }
    
    private void onFsmComplete(FsmEvent event) {
        onFsmComplete(event.getContext());
    }

    protected void onFsmComplete(FsmContext context) {
        StdProcessContext processContext = context.getData(StdFsmProcessConstants.FSM_CTX_PROCESS_CTX);
        processRuntimeService.processCompleteExec(processContext);
    }

    private Map<String, Object> assembleProcessProperties(FsmContext context) {
        Map<String, Object> properties = new ConcurrentHashMap<String, Object>();
        Map<String, Object> ctxProperties = context.getData(StdFsmProcessConstants.FSM_PROP_PROCESS);
        if (ctxProperties != null) {
            properties.putAll(ctxProperties);
        }
        String dataFlushPolicy = initDataFlushPolicy(context);
        properties.put(StdProcessConstants.PROP_DATA_FLUSH_POLICY, dataFlushPolicy);

        boolean checkFlushNodes = Boolean.TRUE
                .equals(PropertiesUtil.get(context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS),
                        StdProcessConstants.PROP_CHECK_FLUSH_NODES));
        properties.put(StdProcessConstants.PROP_CHECK_FLUSH_NODES, checkFlushNodes);
        if (checkFlushNodes) {
            String[] flushNodes = initFlushNodes(context);
            PropertiesUtil.put(properties, StdProcessConstants.PROP_FLUSH_NODES, flushNodes);
        }
        initStartAndEndNodeIds(context, properties);
        
        Map<String, Object> processInstanceStatusMessage = PropertiesUtil.get(
                context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS),
                StdProcessConstants.PROP_PROCESS_INSTANCE_STATUS_MESSAGE);
        if (processInstanceStatusMessage != null) {
            properties.put(StdProcessConstants.PROP_PROCESS_INSTANCE_STATUS_MESSAGE, processInstanceStatusMessage);
        }
        Map<String, Object> nodeInstanceStatusMessage = PropertiesUtil.get(
                context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS),
                StdProcessConstants.PROP_NODE_INSTANCE_STATUS_MESSAGE);
        if (nodeInstanceStatusMessage != null) {
            properties.put(StdProcessConstants.PROP_NODE_INSTANCE_STATUS_MESSAGE, nodeInstanceStatusMessage);
        }
        
        if (log.isDebugEnabled()) {
            log.debug("Assembled process context properties:" + JSON.toJSONString(properties));
        }
        return properties;
    }

    private String initDataFlushPolicy(FsmContext context) {
        String flushPolicy = PropertiesUtil.get(StdProcessConstants.PROP_DATA_FLUSH_POLICY,
                context.getData(StdFsmProcessConstants.FSM_PROP_PROCESS),
                context.getParam().get(StdFsmProcessConstants.FSM_PROP_PROCESS),
                context.getFsm().getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS)
                );
        if (flushPolicy == null) {
            flushPolicy = StdProcessConstants.FLUSH_AFTER_PROCESS;
        }
        return flushPolicy;

    }

    private String[] initFlushNodes(FsmContext context) {
        List<String> list = new ArrayList<String>();
        context.getFsm().getStateList().forEach(node -> {
            if (!Boolean.FALSE.equals(PropertiesUtil.get(node.getProperty(StdFsmProcessConstants.FSM_PROP_PROCESS),
                    StdProcessConstants.PROP_FLUSH))) {
                list.add(node.getId());
            }
        });
        return list.toArray(new String[] {});
    }

    private void initStartAndEndNodeIds(FsmContext context, Map<String, Object> properties) {
        List<String> startNodeIds = new ArrayList<String>();
        String startStateId = context.getFsm().getStartStateId();
        if (startStateId != null) {
            startNodeIds.add(startStateId);
        }
        properties.put(StdProcessConstants.PROP_START_NODE_IDS, startNodeIds);

        List<String> endStateIds = new ArrayList<String>();
        context.getFsm().getStateList().forEach(state -> {
            if (Boolean.TRUE.equals(state.getProperty(StdProcessConstants.PROP_END))) {
                endStateIds.add(state.getId());
            }
        });
        properties.put(StdProcessConstants.PROP_END_NODE_IDS, endStateIds);

    }
    
    private void onFsmManagerStart(FsmEvent event) {
        FsmParam param = (FsmParam) ((Map<String, Object>) event.getData()).get("param");
        FsmManager fsmManager = (FsmManager) ((Map<String, Object>) event.getData()).get("fsmManager");
        onFsmManagerStart(param, fsmManager);
    }

    protected void onFsmManagerStart(FsmParam param, FsmManager fsmManager) {
        if (param != null && param.getFsmId() == null) {
            String processType = param.get(StdFlowProcessConstants.FLOW_PARAM_PROCESS_TYPE);
            String bizNo = param.get(StdFlowProcessConstants.FLOW_PARAM_BIZNO);
            String instanceNo = param.get(StdFlowProcessConstants.FLOW_PARAM_INSTANCENO);
            ProcessInstanceDTO processInstanceDto = null;
            if (instanceNo != null) {
                processInstanceDto = ExportResponseUtil
                        .unwrap(getProcessInstanceExport().getProcessInstance(new ExportRequest<>(instanceNo)));
            } else if (processType != null && bizNo != null) {
                QueryProcessInstanceReq instanceReq = QueryProcessInstanceReq.builder().processType(processType)
                        .bizNo(bizNo).build();
                processInstanceDto = ExportResponseUtil.unwrap(getProcessInstanceExport()
                        .queryProcessInstanceByProcessTypeAndBizNo(new ExportRequest<>(instanceReq)));
            }
            if (processInstanceDto != null) {
                String fsmId = processInstanceDto.getProcessDefId().substring("FSM-".length());
                if (fsmManager != null && (fsmManager instanceof StdFsmManager)) {
                    if (!fsmId.contains(StdFsmProcessConstants.VERSION_PREFIX)) {
                        fsmId = fsmId + StdFsmProcessConstants.VERSION_PREFIX;
                    }
                    param.setFsmId(fsmId);
                }
            }
        }
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
    public void postConstruct(Map<String, Object> definition, Map<String, Object> context) {}
    
    @Override
    public void init(InitContext initContext, Object parent) {}

    @Override
    public void destroy() {}
}

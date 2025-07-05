package com.jd.easyflow.process.client.flow.compensate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.action.compensate.CompensateAction;
import com.jd.easyflow.flow.model.action.compensate.CompensateHelper;
import com.jd.easyflow.flow.model.action.compensate.CompensateNodeFilter;
import com.jd.easyflow.flow.model.node.NodeImpl;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.LockUtil;
import com.jd.easyflow.flow.util.Triple;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.flow.util.StdProcessFlowUtil;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 */
public class StdProcessCompensateNodeFilter extends CompensateNodeFilter {

    private static final Logger logger = LoggerFactory.getLogger(StdProcessCompensateNodeFilter.class);
    
    @Autowired
    private ProcessRuntimeManager processRuntimeManager;
    
    public StdProcessCompensateNodeFilter() {
        
    }
    
    public StdProcessCompensateNodeFilter(int order) {
        this.order = order;
    }

    @Override
    public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
            FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
        FlowContext context = request.getRight();
        NodeContext nodeContext = request.getMiddle();
        if (! Boolean.TRUE.equals(nodeContext.get(StdProcessCompensateHelper.NODE_CONTEXT_COMPENSATE_NODE_FLAG))) {
            // Normal scenario.
            NodeContext newNodeContext = chain.doFilter(request);
            newNodeContext.put(StdProcessCompensateNodeFilter.class.getName(), true);
            if (CompensateHelper.isCompensating(context)) {
                Object lockObj = LockUtil.getFlowContextLock("_std_process_compensate_end_nodes_lock", context);
                synchronized (lockObj) {
                    StdNodeContext stdNodeContext = StdProcessFlowUtil.getStdNodeContext(newNodeContext);
                    StdProcessContext processContext = StdProcessFlowUtil.getStdProcessContext(context);
                    // post to previous node.
                    List<NodeContext> compensatedNextNodes = new ArrayList<NodeContext>();
                    // current node previous nodes.
                    compensatedNextNodes.add(StdProcessCompensateHelper.createCompensateNode(newNodeContext, processRuntimeManager));
                    // add end nodes.
                    if (!Boolean.TRUE.equals(context.get(StdProcessCompensateHelper.FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG))) {
                        processContext.putVariable(StdProcessConstants.PROCESS_VAR_COMPENSATE_FLAG, Boolean.TRUE.toString());
                        List<String> processNodeInstanceList = getEndNodes(context, processRuntimeManager);
                        for (String nodeInstanceNo : processNodeInstanceList) {
                            if (!nodeInstanceNo.equals(stdNodeContext.getNodeInstanceNo())) {
                                compensatedNextNodes.add(StdProcessCompensateHelper.createCompensateNode(nodeInstanceNo, processContext, processRuntimeManager));
                            }
                        }
                        context.put(StdProcessCompensateHelper.FLOW_CONTEXT_COMPENSATE_END_NODES_FLAG, true);
                    }
                    NodeContextAccessor.setNextNodes(newNodeContext, compensatedNextNodes.toArray(new NodeContext[compensatedNextNodes.size()]));
                }
            }
            return newNodeContext; 
            
        } else {
            // Compensate scenario.
            NodeImpl nodeImpl = (NodeImpl) context.getFlow().getNode(nodeContext.getNodeId());
            StdProcessContext processContext = StdProcessFlowUtil.getStdProcessContext(context);
            String originalNodeInstanceNo = nodeContext.get(StdProcessCompensateHelper.NODE_CONTEXT_COMPENSATE_FOR);
            StdNodeContext stdNodeContext = StdProcessFlowUtil.getStdNodeContext(nodeContext);
            stdNodeContext.putVariable(StdProcessConstants.NODE_VAR_COMPENSATE_NODE_FLAG, Boolean.TRUE.toString());
            stdNodeContext.putVariable(StdProcessConstants.NODE_VAR_COMPENSATE_FOR, originalNodeInstanceNo);
            ProcessNodeInstanceDTO originProcessNodeInstance = processRuntimeManager.getNodeInstance(originalNodeInstanceNo, processContext);
            
            Object lockObj = LockUtil.getFlowContextLock("_node_instance_no_lock_" + originalNodeInstanceNo, context);
            synchronized (lockObj) {
                boolean compensatedFlag = Boolean.TRUE.toString().equals(getVar(StdProcessConstants.NODE_VAR_COMPENSATED_FLAG, originProcessNodeInstance));
                if (compensatedFlag) {
                    return nodeContext;
                }
                // Judge compensate
                String nextNodeInstancesStr = originProcessNodeInstance.getNextNodeInstances();
                String[] nextNodeInstanceNos = nextNodeInstancesStr == null ? null : nextNodeInstancesStr.split(",");
    
                if (nextNodeInstanceNos != null && nextNodeInstanceNos.length > 0) {
                    String notCompensated = null;
                    for (String nextNodeInstanceNo : nextNodeInstanceNos) {
                        if (nextNodeInstanceNo.equals(stdNodeContext.getNodeInstanceNo())) {
                            continue;
                        }
                        ProcessNodeInstanceDTO processNodeInstance = processRuntimeManager.getNodeInstance(nextNodeInstanceNo, processContext);
                        boolean flag = Boolean.TRUE.toString().equals(getVar(StdProcessConstants.NODE_VAR_COMPENSATED_FLAG, processNodeInstance));
                        if (!flag) {
                            notCompensated = nextNodeInstanceNo;
                            break;
                        }
                    }
                    if (notCompensated != null) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("Next nodes of " + originalNodeInstanceNo + ":" + nodeContext.getNodeId() + " not all compensated:" + notCompensated);
                        }
                        return nodeContext;
                    }
                }
                
                // Execute compensate action

                    // NOOP
                if (StdProcessConstants.NODE_STATUS_INACTIVE.equals(originProcessNodeInstance.getStatus())) {
                    putVar(StdProcessConstants.NODE_VAR_COMPENSATED_BY, stdNodeContext.getNodeInstanceNo(), originProcessNodeInstance, processContext);
                    putVar(StdProcessConstants.NODE_VAR_COMPENSATED_FLAG, Boolean.TRUE.toString(), originProcessNodeInstance, processContext);
                } else {
                    NodeAction nodeAction = nodeImpl.getAction();
                    Object compensateResult = null;
                    NodeAction compensateNodeAction = (NodeAction) nodeImpl.getProperty(FlowConstants.PROP_RUNTIME_COMPENSATE_ACTION);
                    if (compensateNodeAction != null) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("compensate using NodeAction:" + compensateNodeAction.getClass());
                        }
                        compensateResult = compensateNodeAction.execute(nodeContext, context);
                    } else if (nodeAction instanceof CompensateAction) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("compensate using CompensateAction");
                        }
                        compensateResult = ((CompensateAction) nodeAction).compensate(nodeContext, context);
                    }
                    NodeContextAccessor.setActionResult(nodeContext, compensateResult);
                    putVar(StdProcessConstants.NODE_VAR_COMPENSATED_BY, stdNodeContext.getNodeInstanceNo(), originProcessNodeInstance, processContext);
                    putVar(StdProcessConstants.NODE_VAR_COMPENSATED_FLAG, Boolean.TRUE.toString(), originProcessNodeInstance, processContext);
                }
                
                // post to previous node.
                String previousNodeInstancesStr = originProcessNodeInstance.getPreviousNodeInstances();
                String[] previousNodeInstances = previousNodeInstancesStr == null ? null : previousNodeInstancesStr.split(",");
                if (previousNodeInstances == null) {
                    NodeContextAccessor.setNextNodes(nodeContext, null);
                } else {
                    NodeContext[] previouses = new NodeContext[previousNodeInstances.length];
                    for (int i = 0; i < previousNodeInstances.length; i++) {
                        previouses[i] = StdProcessCompensateHelper.createCompensateNode(previousNodeInstances[i], processContext, processRuntimeManager);
                    }
                    NodeContextAccessor.setNextNodes(nodeContext, previouses);
    
                    
                }
                return nodeContext;
            }
        } 
    }


    private List<String> getEndNodes(FlowContext context, ProcessRuntimeManager manager) {
        
        StdProcessContext processContext = StdProcessFlowUtil.getStdProcessContext(context);
        List<ProcessNodeInstanceDTO> openNodeInstances = manager.findOpenNodeInstances(processContext);
        Set<String> nodeInstanceNos = new HashSet<String>();
        for (ProcessNodeInstanceDTO nodeInstance : openNodeInstances) {
            nodeInstanceNos.add(nodeInstance.getNodeInstanceNo());
        }
        excludeRunningNode(nodeInstanceNos, context);
        return new ArrayList(nodeInstanceNos);
    }
    
    private void excludeRunningNode(Set<String> nodeInstanceNos, FlowContext context) {
        for (NodeContext startNodeContext : context.getStartNodes()) {
            excludeRunningNode(nodeInstanceNos, startNodeContext);
        }
    }
    
   private void excludeRunningNode(Set<String> nodeInstanceNos, NodeContext nodeContext) {
        StdNodeContext stdNodeContext = StdProcessFlowUtil.getStdNodeContext(nodeContext);
        nodeInstanceNos.remove(stdNodeContext.getNodeInstanceNo());
        if (nodeContext.getNextNodes() != null && nodeContext.getNextNodes().length > 0) {
            for (NodeContext nodeCtx : nodeContext.getNextNodes()) {
                excludeRunningNode(nodeInstanceNos, nodeCtx);
            }
        }
    }
    
    private String getVar(String key, ProcessNodeInstanceDTO processNodeInstance) {
        String varsString = processNodeInstance.getVars();
        if (varsString == null) {
            return null;
        }
        Map<String, String> vars = JSON.parseObject(varsString, Map.class);
        return vars.get(key);
     }
    
    private void putVar(String key, String value, ProcessNodeInstanceDTO processNodeInstanceDTO, StdProcessContext processContext) {
        Map<String, String> vars = JSON.parseObject(processNodeInstanceDTO.getVars(), Map.class);
        if (vars == null) {
            vars = new HashMap<String, String>();
        }
        vars.put(key, value);
        processNodeInstanceDTO.setVars(JSON.toJSONString(vars));
        processRuntimeManager.updateNodeInstance(processNodeInstanceDTO, processContext);
    }

    public ProcessRuntimeManager getProcessRuntimeManager() {
        return processRuntimeManager;
    }

    public void setProcessRuntimeManager(ProcessRuntimeManager processRuntimeManager) {
        this.processRuntimeManager = processRuntimeManager;
    }
    
     
}
  

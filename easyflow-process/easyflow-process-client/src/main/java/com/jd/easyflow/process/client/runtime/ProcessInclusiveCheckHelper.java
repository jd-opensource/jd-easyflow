package com.jd.easyflow.process.client.runtime;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.FlowNodeLinkUtil;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;

/**
 * Inner process inclusive check helper.
 */
class ProcessInclusiveCheckHelper  {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessInclusiveCheckHelper.class);
    
    static final String CTX_WAITING_NODE_MAP = "_WAITING_NODE_MAP";
    
    static void nodeStartExec(StdNodeContext nodeContext, StdProcessContext context, ProcessNodeInstanceDTO nodeInstance, ProcessRuntimeManager manager) {
        if (StdProcessConstants.NODE_STATUS_ACTIVE.equals(nodeInstance.getStatus())) {
            log.info("Node " + nodeContext + " finish precheck");
            nodeContext.setPreResult(false);
        } else if (StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus())) {
            boolean preResult = false;
            FlowContext flowContext = context.getEngineProcessContext();
            Map<String, WaitingNodeInfo> map = getWaitingNodeInfoMap(context, manager);
            WaitingNodeInfo waitingNodeInfo = map.get(nodeContext.getNodeId());
            if (waitingNodeInfo == null) {
                waitingNodeInfo = createWaitingNodeInfo(nodeContext.getNodeId(), nodeContext.getConfigPreNodeIds(), flowContext.getFlow());
                map.put(nodeContext.getNodeId(), waitingNodeInfo);
            }
            boolean active = false;
            String previousNodeId = nodeContext.getPreviousNodeId();
            if (waitingNodeInfo.unknownPreNodes.contains(previousNodeId)) {
                waitingNodeInfo.finishedPreNodes.add(previousNodeId);
                waitingNodeInfo.unknownPreNodes.remove(previousNodeId);
            }
            if (waitingNodeInfo.unknownPreNodes.size() == 0) {
                log.info("Node:" + nodeContext.getNodeId() + " is activated");
                map.remove(nodeContext.getNodeId());
                active = true;
            } else {
                ProcessInclusiveCheckHelper.judgeOneWaitingNode(waitingNodeInfo, context, manager);
                if (waitingNodeInfo.unknownPreNodes.size() == 0) {
                        log.info("Node:" + nodeContext.getNodeId() + " is activated");
                    map.remove(nodeContext.getNodeId());
                    active = true;
                } else {
                    if (log.isDebugEnabled()) {
                        log.debug("Finish nodes:" + waitingNodeInfo.finishedPreNodes + " Unreachable nodes:" + waitingNodeInfo.unreachablePreNodes + " Unknown nodes:" + waitingNodeInfo.unknownPreNodes);
                    }
                    active = false;
                }
            }

            if (active) {
                log.info(nodeInstance.getNodeInstanceNo() + " " + nodeInstance.getNodeId() + " node is active");
                nodeInstance.setStatus(StdProcessConstants.NODE_STATUS_ACTIVE);
                manager.updateNodeInstance(nodeInstance, context);
                if (context.getNodeStartEventPolicy() == StdProcessConstants.NODE_START_EVENT_POLICY_ACTIVE) {
                    nodeInstance.setStartTime(new Date());
                    context.getEventTriggerFunction().apply(new Object[] { StdProcessConstants.EVENT_NODE_INSTANCE_START, new Object[] {nodeInstance, nodeContext} });
                }
                preResult = true;
            }
            nodeContext.setPreResult(preResult);
        } else {
            throw new IllegalStateException("Illegal node status:" + nodeInstance);
        }
    
    }
    
    private static Map<String, WaitingNodeInfo> getWaitingNodeInfoMap(StdProcessContext context, ProcessRuntimeManager manager) {
        Map<String, WaitingNodeInfo> map = context.get(CTX_WAITING_NODE_MAP);
        if (map == null) {
            map = new ConcurrentHashMap<String, WaitingNodeInfo>();
            context.put(ProcessInclusiveCheckHelper.CTX_WAITING_NODE_MAP, map);
            // 查询所有Inactive节点，并判断是否是inclusive.
            List<ProcessNodeInstanceDTO> openNodeInstances = manager.findOpenNodeInstances(context);
            FlowContext flowContext = (FlowContext) context.getEngineProcessContext();
            Flow flow = flowContext.getFlow();
            for (ProcessNodeInstanceDTO nodeInstance : openNodeInstances) {
                if (! StdProcessConstants.NODE_STATUS_INACTIVE.equals(nodeInstance.getStatus())) {
                    continue;
                }
                
                String nodeId = nodeInstance.getNodeId();
                String preCheckType = FlowNodeLinkUtil.getPreCheckType(nodeId, flow);
                if (FlowConstants.NODE_PRE_CHECK_TYPE_INCLUSIVECHECK.equals(preCheckType)) {
                    List<String> preNodes = FlowNodeLinkUtil.getPreCheckNodes(nodeId, flow);
                    WaitingNodeInfo waitingNodeInfo = createWaitingNodeInfo(nodeId, preNodes, flow);
                    List<ProcessNodeInstanceDTO> previousNodeInstances = getPreviousNodeInstances(nodeInstance, context, manager);
                    for (ProcessNodeInstanceDTO previousNodeInstance : previousNodeInstances) {
                        waitingNodeInfo.finishedPreNodes.add(previousNodeInstance.getNodeId());
                        waitingNodeInfo.unknownPreNodes.remove(previousNodeInstance.getNodeId());
                    }
                    map.put(nodeId, waitingNodeInfo);
                }
            }
        }
        return map;
    }
    
    private static WaitingNodeInfo createWaitingNodeInfo(String nodeId, List<String> configPreNodes, Flow flow) {
        WaitingNodeInfo waitingNodeInfo = new WaitingNodeInfo();
        waitingNodeInfo.waitNodeId = nodeId;
        waitingNodeInfo.unknownPreNodes = new HashSet<String>();
        waitingNodeInfo.finishedPreNodes = new HashSet<String>();
        waitingNodeInfo.unreachablePreNodes = new HashSet<String>();
        
        for (String preNode : configPreNodes) {
            if (FlowNodeLinkUtil.isReachable(preNode, nodeId, flow)) {
                waitingNodeInfo.unknownPreNodes.add(preNode);
            } else {
                waitingNodeInfo.unreachablePreNodes.add(preNode);
            }
        }
        return waitingNodeInfo;
    }
    
    static void nodeEndExec(StdNodeContext nodeContext, StdProcessContext context, ProcessRuntimeManager manager) {
        if (! (context.getEngineProcessContext() instanceof FlowContext)) {
            return;
        }
        Map<String, WaitingNodeInfo> map = getWaitingNodeInfoMap(context, manager);
        if (map != null && !map.isEmpty()) {
            List<NodeContext> additionalNextNodes = null;
            NodeContext nodeCtx = nodeContext.getEngineNodeContext();
            for (WaitingNodeInfo info : map.values()) {
                if (info.waitNodeId.equals(nodeContext.getNodeId())) {
                    continue;
                }
                ProcessInclusiveCheckHelper.judgeOneWaitingNode(info, context, manager);
                if (info.unknownPreNodes.isEmpty()) {
                        log.info("Node:" + info.waitNodeId + " is activated in node end exec");
                    if (additionalNextNodes == null) {
                        additionalNextNodes = new ArrayList<NodeContext>();
                        if (nodeCtx.getNextNodes() != null) {
                            for (NodeContext nc : nodeCtx.getNextNodes()) {
                                additionalNextNodes.add(nc);
                            }
                        }
                    }
                    NodeContext additionNode = new NodeContext(info.waitNodeId);
                    additionalNextNodes.add(additionNode);
                }
            }
            if (additionalNextNodes != null) {
                nodeCtx.setNextNodes(additionalNextNodes.toArray(new NodeContext[additionalNextNodes.size()]));
                String[] nextNodeIds = new String[additionalNextNodes.size()];
                for (int i = 0; i < additionalNextNodes.size(); i++) {
                    nextNodeIds[i] = additionalNextNodes.get(i).getNodeId();
                }
                nodeContext.setNextNodeIds(nextNodeIds);
            }
        }
    }

    static void judgeOneWaitingNode(WaitingNodeInfo info, StdProcessContext processContext, ProcessRuntimeManager manager) {
        FlowContext context = (FlowContext) processContext.getEngineProcessContext();
        List<ProcessNodeInstanceDTO> openNodeInstances = manager.findOpenNodeInstances(processContext);
        List<String> endNodeIds = processContext.getProcessProperty(StdProcessConstants.PROP_END_NODE_IDS);

        Iterator<String> iterator = info.unknownPreNodes.iterator();
        while (iterator.hasNext()) {
            String unknownPreNode = iterator.next();
            boolean reachable = false;
            for (ProcessNodeInstanceDTO nodeInstance : openNodeInstances) {
                if (endNodeIds != null && endNodeIds.contains(nodeInstance.getNodeId())) {
                    continue;
                }
                if (nodeInstance.getNodeId().equals(unknownPreNode)) {
                    reachable = true;
                    break;
                }
                if (nodeInstance.getNodeId().equals(info.waitNodeId)) {
                    if (previousNodesContainsNodeId(nodeInstance, unknownPreNode, processContext, manager)) {
                        reachable = true;
                        break;
                    } else {
                        continue;
                    }
                }
                if (FlowNodeLinkUtil.isReachable(nodeInstance.getNodeId(), unknownPreNode, context.getFlow())) {
                    reachable = true;
                    break;
                }
            }
            if (! reachable) {
                info.unreachablePreNodes.add(unknownPreNode);
                iterator.remove();

            }
        }
    }
    
    private static boolean previousNodesContainsNodeId(ProcessNodeInstanceDTO nodeInstance, String nodeId,
            StdProcessContext context, ProcessRuntimeManager manager) {
        List<ProcessNodeInstanceDTO> previousNodeInstances = getPreviousNodeInstances(nodeInstance, context, manager);
        for (ProcessNodeInstanceDTO previousNodeInstance : previousNodeInstances) {
            if (previousNodeInstance.getNodeId().equals(nodeId)) {
                return true;
            }
        }
        return false;
    }

    private static List<ProcessNodeInstanceDTO> getPreviousNodeInstances(ProcessNodeInstanceDTO nodeInstance, StdProcessContext context, ProcessRuntimeManager manager) {
        List<ProcessNodeInstanceDTO> previousNodeInstances = new ArrayList<ProcessNodeInstanceDTO>();
        String previousNodeInstanceStr = nodeInstance.getPreviousNodeInstances();
        if (previousNodeInstanceStr != null && ! previousNodeInstanceStr.isEmpty()) {
            String[] previousNodeInstanceNos = previousNodeInstanceStr.split(",");
            for (String nodeInstanceNo : previousNodeInstanceNos) {
                ProcessNodeInstanceDTO previousNodeInstance = manager.getNodeInstance(nodeInstanceNo, context);
                previousNodeInstances.add(previousNodeInstance);
            }
        }
        return previousNodeInstances;
    }
    

}



class WaitingNodeInfo {
    
    String waitNodeId;
    
    Set<String> finishedPreNodes;
    Set<String> unreachablePreNodes;
    Set<String> unknownPreNodes;
    
    
}

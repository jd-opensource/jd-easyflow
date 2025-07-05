package com.jd.easyflow.flow.model.action.compensate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.impl.BaseFlowFilter;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * @author liyuliang5
 */
public class CompensateFlowFilter extends BaseFlowFilter {
    
    private static final String CTX_KEY_ORIGINAL_START_NODES = "_original_start_nodes";
    
    public CompensateFlowFilter() {
        
    }
    
    public CompensateFlowFilter(int order) {
        this.order = order;
    }

    @Override
    public FlowResult doFilter(FlowContext request, FilterChain<FlowContext, FlowResult> chain) {
        if (CompensateHelper.isCompensating(request)) {
            List<NodeContext> originalStartNodes = request.get(CTX_KEY_ORIGINAL_START_NODES);
            if (originalStartNodes == null) {
                originalStartNodes = request.getStartNodes();
                request.put(CTX_KEY_ORIGINAL_START_NODES, originalStartNodes);
            }
            Set<NodeContext> unCompensatedNodes = new HashSet<NodeContext>();
            getUnCompensatedEndNodes(unCompensatedNodes, originalStartNodes);
            List<NodeContext> startNodes = new ArrayList<NodeContext>();
            for (NodeContext endNode : unCompensatedNodes) {
                NodeContext startNode = CompensateHelper.createCompensateNode(endNode);
                startNodes.add(startNode);
            }   
            request.setStartNodes(startNodes);
        }
        FlowResult result = chain.doFilter(request);
        return result;
    }
    
    private void getUnCompensatedEndNodes(Set<NodeContext> nodes, List<NodeContext> flowStartNodes) {
        if (flowStartNodes == null || flowStartNodes.size() == 0) {
            return;
        }
        for (NodeContext nodeContext : flowStartNodes) {
            if (Boolean.TRUE.equals(nodeContext.get(FlowConstants.NODECTX_COMPENSATE_NODE_FLAG))) {
                continue;
            }
            if (nodeContext.getNextNodes() == null || nodeContext.getNextNodes().length == 0) {
                nodes.add(nodeContext);
                continue;
            }
            boolean hasNext = false;
            for (NodeContext next : nodeContext.getNextNodes()) {
                if (! Boolean.TRUE.equals(nodeContext.get(FlowConstants.NODECTX_COMPENSATE_NODE_FLAG))) {
                    hasNext = true;
                    break;
                }
            }
            if (! hasNext) {
                nodes.add(nodeContext);
            } else {
                getUnCompensatedEndNodes(nodes, Arrays.asList(nodeContext.getNextNodes()));
            }
            
            
            
        }
        
    }

}

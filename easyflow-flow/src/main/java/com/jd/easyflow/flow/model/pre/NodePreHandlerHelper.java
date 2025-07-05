package com.jd.easyflow.flow.model.pre;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * @author liyuliang5
 */
public class NodePreHandlerHelper {


    public static void setNextNodesOfPreviousNode(List<NodeContext> previousNodes, NodeContext nodeContext) {
        for (NodeContext previous : previousNodes) {
            if (previous.getNextNodes().length == 1) {
                if (! previous.getNextNodes()[0].equals(nodeContext)) {
                    previous.getNextNodes()[0].put(FlowConstants.NODECTX_PREVIOUS_NODES, new ArrayList<>(0));
                }
                previous.put(FlowConstants.NODECTX_NEXT_NODES, Arrays.asList(nodeContext));

            } else {
                synchronized (previous) {
                    List<NodeContext> nextNodes = previous.get(FlowConstants.NODECTX_PREVIOUS_NODES);
                    if (nextNodes == null) {
                        nextNodes = new ArrayList<NodeContext>(previous.getNextNodes().length);
                        for (NodeContext ctx : previous.getNextNodes()) {
                            nextNodes.add(ctx);
                        }
                        previous.put(FlowConstants.NODECTX_NEXT_NODES, nextNodes);
                    }
                    for (int i = 0; i < nextNodes.size(); i++) {
                        NodeContext next = nextNodes.get(i);
                        if (next.getNodeId().equals(nodeContext.getNodeId()) && ! next.equals(nodeContext)) {
                            nextNodes.get(i).put(FlowConstants.NODECTX_PREVIOUS_NODES, new ArrayList<>(0));
                            nextNodes.set(i, nodeContext);
                        }
                    }
                }
            }
        }
    }

}

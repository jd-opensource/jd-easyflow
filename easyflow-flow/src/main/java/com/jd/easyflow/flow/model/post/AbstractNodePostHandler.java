package com.jd.easyflow.flow.model.post;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class AbstractNodePostHandler implements NodePostHandler {

    private static final String IDX_VAR_PREFIX = "$";

    protected List<String> parseToNodeIds(Object to, NodeContext nodeContext, FlowContext flowContext) {
        // String type
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                return Arrays.asList(toStr);
            } else {
                return Arrays.asList(parseIndexVar(toStr, nodeContext, flowContext));
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            return Arrays.asList(flowContext.getFlow().getNodeList().get(toIdx).getId());
            // List type
        } else if (to instanceof List) {
            List<Object> toList = (List) to;
            List<String> toResult = new ArrayList<>(toList.size());
            for (Object toObj : toList) {
                if (toObj instanceof String) {
                    String toStr = (String) toObj;
                    if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                        toResult.add(toStr);
                    } else {
                        toResult.add(parseIndexVar(toStr, nodeContext, flowContext));
                    }
                } else if (toObj instanceof Integer) {
                    int toIdx = (Integer) toObj;
                    toResult.add(flowContext.getFlow().getNodeList().get(toIdx).getId());
                }
            }
            return toResult;
        } else {
            throw new UnsupportedOperationException("Unsupported type:" + to.getClass());
        }

    }

    private String parseIndexVar(String var, NodeContext nodeContext, FlowContext flowContext) {
        int index = -1;
        Flow flow = flowContext.getFlow();
        switch (var) {
        case "$first": {
            index = 0;
            break;
        }
        case "$last": {
            index = flow.getNodeList().size() - 1;
            break;
        }
        case "$previous": {
            index = flow.getNodeIndex(nodeContext.getNodeId()) - 1;
            break;
        }
        case "$next": {
            index = flow.getNodeIndex(nodeContext.getNodeId()) + 1;
            break;
        }
        default : {
            throw new UnsupportedOperationException("Unsupported vars:" + var);
        }
        }
        return flow.getNodeList().get(index).getId();
    }

    protected NodeContext[] nodeIds2Nodes(List<String> nodeIds) {
        if (nodeIds == null) {
            return null;
        }
        NodeContext[] nodes = new NodeContext[nodeIds.size()];
        for (int i = 0; i < nodeIds.size(); i++) {
            nodes[i] = new NodeContext(nodeIds.get(i));
        }
        return nodes;
    }
}

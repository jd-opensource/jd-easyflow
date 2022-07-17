package com.jd.easyflow.flow.model.post;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.jd.easyflow.flow.el.ElFactory;
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

    protected NodeContext[] parseToNodes(Object to, NodeContext nodeContext, FlowContext flowContext) {
        if (to == null) {
            return null;
        }
        // String type
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                return new NodeContext[] { nodeId2Node(toStr) };
            } else {
                return new NodeContext[] { nodeId2Node(parseIndexVar(toStr, nodeContext, flowContext)) };
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            return new NodeContext[] { nodeId2Node(flowContext.getFlow().getNodeList().get(toIdx).getId()) };
            // List type
        } else if (to instanceof List) {
            List<Object> toList = (List) to;
            List<NodeContext> toResult = new ArrayList<>(toList.size());
            for (Object toObj : toList) {
                NodeContext[] nodes = parseToNodes(toObj, nodeContext, flowContext);
                addArray2List(nodes, toResult);
            }
            NodeContext[] result = new NodeContext[toResult.size()];
            return toResult.toArray(result);
        } else if (to instanceof Map) {
            Map<String, Object> toMap = (Map) to;
            NodeContext[] toNodes = null;
            String toExp = (String) toMap.get("exp");
            if (toExp != null) {
                // parse exp
                Object result = ElFactory.get().eval(toExp, nodeContext, flowContext, null);
                if (result == null) {
                    return null;
                } else if (result instanceof NodeContext[]) {
                    toNodes = (NodeContext[]) result;
                } else {
                    toNodes = parseToNodes(result, nodeContext, flowContext);
                }
            } else {
                // parse node
                Object node = toMap.get("node");
                toNodes = parseToNodes(node, nodeContext, flowContext);
            }
            // fill context data
            Map<String, String> dataConf = (Map<String, String>) toMap.get("data");
            if (dataConf != null && toNodes != null) {
                for (Entry<String, String> entry : dataConf.entrySet()) {
                    Object value = ElFactory.get().eval(entry.getValue(), nodeContext, flowContext, null);
                    for (NodeContext node : toNodes) {
                        node.put(entry.getKey(), value);
                    }
                }

            }
            return toNodes;
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
        default: {
            throw new UnsupportedOperationException("Unsupported vars:" + var);
        }
        }
        return flow.getNodeList().get(index).getId();
    }

    protected NodeContext[] list2Array(List<NodeContext> list) {
        if (list == null) {
            return null;
        }
        NodeContext[] nodes = new NodeContext[list.size()];
        return list.toArray(nodes);
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

    protected NodeContext nodeId2Node(String nodeId) {
        if (nodeId == null) {
            return null;
        }
        return new NodeContext(nodeId);
    }

    protected void addArray2List(NodeContext[] nodes, List<NodeContext> list) {
        if (nodes == null) {
            return;
        }
        for (NodeContext node : nodes) {
            list.add(node);
        }
    }
}

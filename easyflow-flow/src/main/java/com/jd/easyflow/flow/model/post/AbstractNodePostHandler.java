package com.jd.easyflow.flow.model.post;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class AbstractNodePostHandler implements NodePostHandler {

    private static final String IDX_VAR_PREFIX = "$";

    protected NodeContext[] parseToNodes(Object to, NodeContext nodeContext, FlowContext context) {
        if (to == null) {
            return null;
        }
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                return new NodeContext[] { nodeId2Node(toStr) };
            } else {
                return new NodeContext[] { nodeId2Node(parseIndexVar(toStr, nodeContext, context)) };
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            return new NodeContext[] { nodeId2Node(context.getFlow().getNodeList().get(toIdx).getId()) };
        } else if (to instanceof List) {
            List<Object> toList = (List) to;
            List<NodeContext> toResult = new ArrayList<>(toList.size());
            for (Object toObj : toList) {
                NodeContext[] nodes = parseToNodes(toObj, nodeContext, context);
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
                Object result = context.getElEvaluator().eval(toExp, nodeContext, context, null);
                if (result == null) {
                    return null;
                } else if (result instanceof NodeContext[]) {
                    toNodes = (NodeContext[]) result;
                } else {
                    toNodes = parseToNodes(result, nodeContext, context);
                }
            } else {
                // parse node
                Object node = toMap.get("node");
                toNodes = parseToNodes(node, nodeContext, context);
            }
            // fill context data
            Map<String, Object> dataConf = (Map<String, Object>) toMap.get("data");
            if (dataConf != null && toNodes != null) {
                for (Entry<String, Object> entry : dataConf.entrySet()) {
                    Object value = parseDataValue(entry.getValue(), nodeContext, context);
                    for (NodeContext node : toNodes) {
                        node.put(entry.getKey(), value);
                    }
                }
            }
            return toNodes;
        } else if (to instanceof NodeExecutor) {
            Object result = ((NodeExecutor) to).execute(nodeContext, context);
            if (result == null) {
                return null;
            } else if (result instanceof NodeContext[]) {
                return (NodeContext[]) result;
            } else {
                return parseToNodes(result, nodeContext, context);
            }
            
        } else {
            throw new UnsupportedOperationException("Unsupported type:" + to.getClass());
        }
    }
    
    protected Object parseToDefinition(Object to, FlowNode node, InitContext initContext) {
        if (to instanceof List) {
            List list = (List) to;
            for (int i = 0; i < list.size(); i++) {
                Object element = list.get(i);
                if (element instanceof List || element instanceof Map) {
                    list.set(i, parseToDefinition(element, node, initContext));
                }
            }
        } else if (to instanceof Map) {
            Map map = (Map) to;
            if (map.get("type") != null) {
                throw new IllegalArgumentException("type is reserved");
            }
            Map<String, Object> data = (Map<String, Object>) map.get("data");

            String createExp = (String) map.get(DefConstants.COMMON_PROP_CREATE_EXP);
            if (createExp != null && initContext.isParseEl()) {
                if (data != null) {
                    throw new IllegalArgumentException("data is reserved");
                }
                Map<String, Object> context = new HashMap<>(3);
                context.put("definition", map);
                context.put("node", node);
                context.put("flow", initContext.getFlow());
                context.put("flowParser", initContext.getFlowParser());
                NodeExecutor<Boolean> executor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, context, false);
                return executor;
            }
            
            if (data != null) {
                for (Entry<String, Object> entry : data.entrySet()) {
                    if (entry.getValue() instanceof Map) {
                        entry.setValue(parseDataValueDefinition(entry.getValue(), node, initContext));
                    }
                }
            }
            
        }
        return to;
    }
    
    private Object parseDataValueDefinition(Object dataValue, FlowNode node, InitContext initContext) {
        if (dataValue instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) dataValue;
            if (map.get("type") != null) {
                throw new IllegalArgumentException("type is reserved");
            }
            String createExp = (String) map.get(DefConstants.COMMON_PROP_CREATE_EXP);
            if (createExp != null && initContext.isParseEl()) {
                Map<String, Object> context = new HashMap<>(3);
                context.put("definition", map);
                context.put("node", node);
                context.put("flow", initContext.getFlow());
                context.put("flowParser", initContext.getFlowParser());
                NodeExecutor<Boolean> executor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, context, false);
                return executor;
            }
            
        }
        return dataValue;
    }
    
    private Object parseDataValue(Object value, NodeContext nodeContext, FlowContext context) {
        if (value instanceof String) {
            return context.getElEvaluator().eval((String) value, nodeContext, context, null);
        } else if (value instanceof Map) {
            Map<String, Object> valueMap = (Map<String, Object>) value;
            if (valueMap.containsKey(DefConstants.COMMON_PROP_EXP)) {
                String exp = (String) valueMap.get(DefConstants.COMMON_PROP_EXP);
                return context.getElEvaluator().eval(exp, nodeContext, context, null);
            } else if (valueMap.containsKey("fixedValue")) {
                return valueMap.get("fixedValue");
            } else {
                throw new IllegalArgumentException("illegal data map, " + value);
            }
            
        } else if (value instanceof NodeExecutor) {
            return ((NodeExecutor) value).execute(nodeContext, context);
        }  else {
            throw new IllegalArgumentException("illegal data " + value);
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
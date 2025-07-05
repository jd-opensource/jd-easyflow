package com.jd.easyflow.flow.model.node;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public class NodeImpl implements FlowNode {

    protected String id;

    protected String name;

    protected Map<String, Object> properties = new ConcurrentHashMap<>();

    protected NodePreHandler preHandler;

    protected NodeAction action;

    protected NodePostHandler postHandler;
    
    private Function<Pair<NodeContext, FlowContext>, Boolean> outerNodePreHandlerInvoker = p -> invokePreHandler(p.getLeft(), p.getRight());
    private Function<Pair<NodeContext, FlowContext>, Boolean> innerNodePreHandlerInvoker = p -> preHandler.preHandle(p.getLeft(), p.getRight());
    private Function<Pair<NodeContext, FlowContext>, Object> outerNodeActionInvoker = p -> invokeAction(p.getLeft(), p.getRight());
    private Function<Pair<NodeContext, FlowContext>, Object> innerNodeActionInvoker = p -> action.execute(p.getLeft(), p.getRight());
    private Function<Pair<NodeContext, FlowContext>, NodeContext[]> outerNodePostHandlerInvoker = p -> invokePostHandler(p.getLeft(), p.getRight());
    private Function<Pair<NodeContext, FlowContext>, NodeContext[]> innerNodePostHandlerInvoker = p -> postHandler.postHandle(p.getLeft(), p.getRight());

    @Override
    public void init(InitContext initContext, Object parent) {
        if (preHandler != null) {
            preHandler.init(initContext, this);
        }
        if (action != null) {
            action.init(initContext,  this);
        }
        if (postHandler != null) {
            postHandler.init(initContext, this);
        }
    }
    
    @Override
    public void destroy() {
        if (preHandler != null) {
            preHandler.destroy();
        }
        if (action != null) {
            action.destroy();
        }
        if (postHandler != null) {
            postHandler.destroy();
        }
    }

    @Override
    public NodeContext execute(NodeContext nodeContext, FlowContext context) {
        if (!executePreHandler(nodeContext, context)) {
            return nodeContext;
        }
        executeAction(nodeContext, context);
        executePostHandler(nodeContext, context);
        return nodeContext;
    }

    protected boolean executePreHandler(NodeContext nodeContext, FlowContext context) {
        if (context.getFlow().getFilterManager().noOuterNodePreHandlerFilter()) {
            return invokePreHandler(nodeContext, context);
        } else {
            Boolean preResult = context.getFlow().getFilterManager().doOuterNodePreHandlerFilter(Pair.of(nodeContext, context), outerNodePreHandlerInvoker);
            NodeContextAccessor.setPreResult(nodeContext, preResult);
            return preResult == null ? true : preResult;
        }
    }

    protected boolean invokePreHandler(NodeContext nodeContext, FlowContext context) {
        if (preHandler != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_PRE_START, nodeContext, context, false);
            boolean preResult;
            if (context.getFlow().getFilterManager().noInnerNodePreHandlerFilter()) {
                preResult = preHandler.preHandle(nodeContext, context);
            } else {
                Boolean result = context.getFlow().getFilterManager().doInnerNodePreHandlerFilter(Pair.of(nodeContext, context), innerNodePreHandlerInvoker);
                preResult = result == null ? true : result;
            }
            NodeContextAccessor.setPreResult(nodeContext, preResult);
            context.getFlow().triggerEvent(FlowEventTypes.NODE_PRE_END, nodeContext, context, false);
        }
        return nodeContext.getPreResult() == null ? true : nodeContext.getPreResult();
    }

    /**
     * Execute node action.
     * 
     * @param nodeContext
     * @param context
     */
    protected void executeAction(NodeContext nodeContext, FlowContext context) {
        if (context.getFlow().getFilterManager().noOuterNodeActionFilter()) {
            invokeAction(nodeContext, context);
        } else {
            Object result = context.getFlow().getFilterManager().doOuterNodeActionFilter(Pair.of(nodeContext, context), outerNodeActionInvoker);
            NodeContextAccessor.setActionResult(nodeContext,result);
        }
    }

    protected Object invokeAction(NodeContext nodeContext, FlowContext context) {
        if (action != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_ACTION_START, nodeContext, context, false);
            Object result = null;
            if (context.getFlow().getFilterManager().noInnerNodeActionFilter()) {
                result = action.execute(nodeContext, context);
            } else {
                result = context.getFlow().getFilterManager().doInnerNodeActionFilter(Pair.of(nodeContext, context), innerNodeActionInvoker);
            }
            NodeContextAccessor.setActionResult(nodeContext,result);
            context.getFlow().triggerEvent(FlowEventTypes.NODE_ACTION_END, nodeContext, context, false);
        }
        return nodeContext.getActionResult();
    }

    protected void executePostHandler(NodeContext nodeContext, FlowContext context) {
        if (context.getFlow().getFilterManager().noOuterNodePostHandlerFilter()) {
            invokePostHandler(nodeContext, context);
        } else {
            NodeContext[] result = context.getFlow().getFilterManager().doOuterNodePostHandlerFilter(Pair.of(nodeContext, context), outerNodePostHandlerInvoker);
            NodeContextAccessor.setNextNodes(nodeContext, result);
        }
    }

    protected NodeContext[] invokePostHandler(NodeContext nodeContext, FlowContext context) {
        if (postHandler != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_POST_START, nodeContext, context, false);
            NodeContext[] nextNodes = null;
            if (context.getFlow().getFilterManager().noInnerNodePostHandlerFilter()) {
                nextNodes = postHandler.postHandle(nodeContext, context);
            } else {
                nextNodes = context.getFlow().getFilterManager().doInnerNodePostHandlerFilter(Pair.of(nodeContext, context), innerNodePostHandlerInvoker);
            }
            if (nextNodes != null) {
                NodeContextAccessor.setNextNodes(nodeContext,nextNodes);
            }
            context.getFlow().triggerEvent(FlowEventTypes.NODE_POST_END, nodeContext, context, false);
        }
        return nodeContext.getNextNodes();
    }

    public NodeAction getAction() {
        return action;
    }

    public void setAction(NodeAction action) {
        this.action = action;
    }

    public NodePreHandler getPreHandler() {
        return preHandler;
    }

    public void setPreHandler(NodePreHandler preHandler) {
        this.preHandler = preHandler;
    }

    public NodePostHandler getPostHandler() {
        return postHandler;
    }

    @Override
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setPostHandler(NodePostHandler postHandler) {
        this.postHandler = postHandler;
    }

    @Override
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public Map<String, Object> getProperties() {
        return properties;
    }
    
    public void setProperties(Map<String, Object> properties) {
        this.properties.clear();
        this.putProperties(properties);
    }

    public void putProperties(Map<String, Object> properties) {
        if (properties == null) {
            return;
        }
        for (Entry<String, Object> entry : properties.entrySet()) {
            if (entry.getValue() == null) {
                this.properties.remove(entry.getKey());
            } else {
                this.properties.put(entry.getKey(), entry.getValue());
            }
        }
    }

}

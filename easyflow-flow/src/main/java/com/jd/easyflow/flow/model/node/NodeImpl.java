package com.jd.easyflow.flow.model.node;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodePostHandler;
import com.jd.easyflow.flow.model.NodePreHandler;
import com.jd.easyflow.flow.util.FlowEventTypes;

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
        List<Filter<Pair<NodeContext, FlowContext>, Boolean>> filters = context.getFlow().getNodePreHandlerFilters();
        if (filters == null || filters.size() == 0) {
            return invokePreHandler(nodeContext, context);
        }
        FilterChain<Pair<NodeContext, FlowContext>, Boolean> chain = new FilterChain<Pair<NodeContext, FlowContext>, Boolean>(
                filters, p -> {
                    return invokePreHandler(nodeContext, context);
                });
        Boolean result = chain.doFilter(Pair.of(nodeContext, context));
        nodeContext.setPreResult(result);
        return result == null ? true : result;
    }

    protected boolean invokePreHandler(NodeContext nodeContext, FlowContext context) {
        if (preHandler != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_PRE_START, nodeContext, context, false);
            boolean result = preHandler.preHandle(nodeContext, context);
            nodeContext.setPreResult(result);
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
        List<Filter<Pair<NodeContext, FlowContext>, Object>> filters = context.getFlow().getNodeActionFilters();
        if (filters == null || filters.size() == 0) {
            invokeAction(nodeContext, context);
            return;
        }
        FilterChain<Pair<NodeContext, FlowContext>, Object> chain = new FilterChain<Pair<NodeContext, FlowContext>, Object>(
                filters, p -> {
                    return invokeAction(nodeContext, context);
                });
        Object result = chain.doFilter(Pair.of(nodeContext, context));
        nodeContext.setActionResult(result);
    }

    protected Object invokeAction(NodeContext nodeContext, FlowContext context) {
        if (action != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_ACTION_START, nodeContext, context, false);
            Object result = action.execute(nodeContext, context);
            nodeContext.setActionResult(result);
            context.getFlow().triggerEvent(FlowEventTypes.NODE_ACTION_END, nodeContext, context, false);
        }
        return nodeContext.getActionResult();
    }

    protected void executePostHandler(NodeContext nodeContext, FlowContext context) {
        List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> filters = context.getFlow()
                .getNodePostHandlerFilters();
        if (filters == null || filters.size() == 0) {
            invokePostHandler(nodeContext, context);
            return;
        }
        FilterChain<Pair<NodeContext, FlowContext>, NodeContext[]> chain = new FilterChain<Pair<NodeContext, FlowContext>, NodeContext[]>(
                filters, p -> {
                    return invokePostHandler(nodeContext, context);
                });
        NodeContext[] result = chain.doFilter(Pair.of(nodeContext, context));
        nodeContext.setNextNodes(result);
    }

    protected NodeContext[] invokePostHandler(NodeContext nodeContext, FlowContext context) {
        if (postHandler != null) {
            context.getFlow().triggerEvent(FlowEventTypes.NODE_POST_START, nodeContext, context, false);
            NodeContext[] nextNodes = postHandler.postHandle(nodeContext, context);
            if (nextNodes != null) {
                nodeContext.setNextNodes(nextNodes);
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
        this.properties = properties;
    }

    public void putProperties(Map<String, Object> properties) {
        if (properties != null) {
            this.properties.putAll(properties);
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((id == null) ? 0 : id.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        NodeImpl other = (NodeImpl) obj;
        if (id == null) {
            if (other.id != null) {
                return false;
            }
        } else if (!id.equals(other.id)) {
            return false;
        }
        return true;
    }

}

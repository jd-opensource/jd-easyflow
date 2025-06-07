package com.jd.easyflow.flow.model.filter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.filter.FilterChainImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class FlowFilterManager {
    

    private List<Filter<FlowContext, FlowResult>> filters;

    private List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> nodeFilters;

    /**
     * nodeAction is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, Object>> nodeActionFilters;

    /**
     * nodePreHandler is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, Boolean>> nodePreHandlerFilters;

    /**
     * nodePostHandler is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> nodePostHandlerFilters;
    
    private List<Filter<FlowContext, Boolean>> flowPreHandlerFilters;
    
    private List<Filter<FlowContext, Void>> flowPostHandlerFilters;
    
    public void init(InitContext initContext, Flow flow) {
        if (filters != null) {
            for (Filter filter: filters) {
                filter.init(initContext, flow);
            }
            refeshFilters();
        }
        if (nodeFilters != null) {
            for (Filter filter: nodeFilters) {
                filter.init(initContext, flow);
            }
            refreshNodeFilters();
        }
        if (nodeActionFilters != null) {
            for (Filter filter: nodeActionFilters) {
                filter.init(initContext, flow);
            }
            refeshNodeActionFilters();
        }
        if (nodePreHandlerFilters != null) {
            for (Filter filter: nodePreHandlerFilters) {
                filter.init(initContext, flow);
            }
            refreshNodePreHandlerFilters();
        }
        if (nodePostHandlerFilters != null) {
            for (Filter filter: nodePostHandlerFilters) {
                filter.init(initContext, flow);
            }
            refeshNodePostHandlerFilters();
        }
        if (flowPreHandlerFilters != null) {
            for (Filter filter: flowPreHandlerFilters) {
                filter.init(initContext, flow);
            }
            refreshFlowPreHandlerFilters();
        }
        if (flowPostHandlerFilters != null) {
            for (Filter filter: flowPostHandlerFilters) {
                filter.init(initContext, flow);
            }
            refreshFlowPostHandlerFilters();
        }
    }
    
    public void destry() {
        if (filters != null) {
            for (Filter filter: filters) {
                filter.destroy();
            }
        }
        if (nodeFilters != null) {
            for (Filter filter: nodeFilters) {
                filter.destroy();
            }
        }
        if (nodeActionFilters != null) {
            for (Filter filter: nodeActionFilters) {
                filter.destroy();
            }
        }
        if (nodePreHandlerFilters != null) {
            for (Filter filter: nodePreHandlerFilters) {
                filter.destroy();
            }
        }
        if (nodePostHandlerFilters != null) {
            for (Filter filter: nodePostHandlerFilters) {
                filter.destroy();
            }
        }
        if (flowPreHandlerFilters != null) {
            for (Filter filter: flowPreHandlerFilters) {
                filter.destroy();
            }
        }
        if (flowPostHandlerFilters != null) {
            for (Filter filter: flowPostHandlerFilters) {
                filter.destroy();
            }
        } 
    }


    /* Filters */
    private List<Filter<FlowContext, FlowResult>> innerFilters;
    private List<Filter<FlowContext, FlowResult>> outerFilters;
    
    public List<Filter<FlowContext, FlowResult>> getFilters() {
        return filters;
    }
    

    public List<Filter<FlowContext, FlowResult>> getInnerFilters() {
        return innerFilters;
    }

    public List<Filter<FlowContext, FlowResult>> getOuterFilters() {
        return outerFilters;
    }

    public void setFilters(List<Filter<FlowContext, FlowResult>> filters) {
        this.filters = filters;
        refeshFilters();
    }
    
    private void refeshFilters() {
        if (filters == null) {
            this.innerFilters = this.outerFilters = null;
        } else {
            this.innerFilters = new ArrayList<Filter<FlowContext, FlowResult>>();
            this.outerFilters = new ArrayList<Filter<FlowContext, FlowResult>>();
            for (Filter filter : filters) {
                addFilter(filter, this.innerFilters, this.outerFilters);
            }
        }
    }

    public void addFilter(Filter<FlowContext, FlowResult> filter) {
        if (this.filters == null) {
            this.filters = new ArrayList<Filter<FlowContext, FlowResult>>();
            this.innerFilters = new ArrayList<Filter<FlowContext,FlowResult>>();
            this.outerFilters = new ArrayList<Filter<FlowContext,FlowResult>>();
        }
        this.filters.add(filter);
        addFilter(filter, this.innerFilters, this.outerFilters);
    }
    
    public boolean noOuterFilter() {
        return outerFilters == null || outerFilters.size() == 0;
    }
    
    public FlowResult doOuterFilter(FlowContext context, Function<FlowContext, FlowResult> outerInvoker) {
        FilterChain<FlowContext, FlowResult> chain = new FilterChainImpl<FlowContext, FlowResult>(outerFilters, outerInvoker);
        return chain.doFilter(context);
    }
    
    public boolean noInnerFilter() {
        return innerFilters == null || innerFilters.size() == 0;
    }
    
    public FlowResult doInnerFilter(FlowContext context, Function<FlowContext, FlowResult> innerInvoker) {
        FilterChain<FlowContext, FlowResult> chain = new FilterChainImpl<FlowContext, FlowResult>(innerFilters, innerInvoker);
        return chain.doFilter(context);
    }
    
    /* Node Filters */
    private List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> innerNodeFilters;
    private List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> outerNodeFilters;
    
    public List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> getNodeFilters() {
        return nodeFilters;
    }
    
    public List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> getInnerNodeFilters() {
        return innerNodeFilters;
    }

    public List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> getOuterNodeFilters() {
        return outerNodeFilters;
    }

    public void setNodeFilters(List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> nodeFilters) {
        this.nodeFilters = nodeFilters;
        refreshNodeFilters();
    }
    
    private void refreshNodeFilters() {
        if (nodeFilters.size() == 0) {
            this.innerNodeFilters = this.outerNodeFilters = null;
        } else {
            this.innerNodeFilters = new ArrayList<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>>();
            this.outerNodeFilters = new ArrayList<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>>();
            for (Filter filter : nodeFilters) {
                addFilter(filter, this.innerNodeFilters, this.outerNodeFilters);
            }
        }
    }

    public void addNodeFilter(Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> filter) {
        if (this.nodeFilters == null) {
            this.nodeFilters = new ArrayList<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>>();
            this.innerNodeFilters = new ArrayList<Filter<Triple<FlowNode,NodeContext,FlowContext>,NodeContext>>();
            this.outerNodeFilters = new ArrayList<Filter<Triple<FlowNode,NodeContext,FlowContext>,NodeContext>>();            
        }
        this.nodeFilters.add(filter);
        addFilter(filter, this.innerNodeFilters, this.outerNodeFilters);
    }
    
    public boolean noOuterNodeFilter() {
        return outerNodeFilters == null || outerNodeFilters.size() == 0;
    }
    
    public NodeContext doOuterNodeFilter(Triple<FlowNode, NodeContext, FlowContext> p, Function<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> outerNodeInvoker) {
        FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain = new FilterChainImpl<>(outerNodeFilters, outerNodeInvoker);
        return chain.doFilter(p);
    }
    
    public boolean noInnerNodeFilter() {
        return innerNodeFilters == null || innerNodeFilters.size() == 0;
    }
    
    public NodeContext doInnerNodeFilter(Triple<FlowNode, NodeContext, FlowContext> p, Function<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> innerNodeInvoker) {
        FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain = new FilterChainImpl<>(innerNodeFilters, innerNodeInvoker);
        return chain.doFilter(p);
    }

    /* NodeActionFilters */
    private List<Filter<Pair<NodeContext, FlowContext>, Object>> innerNodeActionFilters;
    private List<Filter<Pair<NodeContext, FlowContext>, Object>> outerNodeActionFilters;
    
    public List<Filter<Pair<NodeContext, FlowContext>, Object>> getNodeActionFilters() {
        return nodeActionFilters;
    }
    
    public List<Filter<Pair<NodeContext, FlowContext>, Object>> getInnerNodeActionFilters() {
        return innerNodeActionFilters;
    }
    
    public List<Filter<Pair<NodeContext, FlowContext>, Object>> getOuterNodeActionFilters() {
        return outerNodeActionFilters;
    }
    
    public void setNodeActionFilters(List<Filter<Pair<NodeContext, FlowContext>, Object>> nodeActionFilters) {
        this.nodeActionFilters = nodeActionFilters;
        refeshNodeActionFilters();
    }
    
    private void refeshNodeActionFilters() {
        if (nodeActionFilters == null) {
            this.innerNodeActionFilters = this.outerNodeActionFilters = null;
        } else {
            this.innerNodeActionFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,Object>>();
            this.outerNodeActionFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,Object>>();
            for (Filter filter : nodeActionFilters) {
                addFilter(filter, this.innerNodeActionFilters, this.outerNodeActionFilters);
            }
        }
    }

    public void addNodeActionFilter(Filter<Pair<NodeContext, FlowContext>, Object> filter) {
        if (this.nodeActionFilters == null) {
            this.nodeActionFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Object>>();
            this.innerNodeActionFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Object>>();
            this.outerNodeActionFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Object>>();
        }
        this.nodeActionFilters.add(filter);
        addFilter(filter, this.innerNodeActionFilters, this.outerNodeActionFilters);
        
    }
    
    public boolean noOuterNodeActionFilter() {
        return outerNodeActionFilters == null || outerNodeActionFilters.size() == 0;
    }
    
    public Object doOuterNodeActionFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, Object> outerNodeActionInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, Object> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, Object>(outerNodeActionFilters, outerNodeActionInvoker);
        return chain.doFilter(p);
    }
    
    public boolean noInnerNodeActionFilter() {
        return innerNodeActionFilters == null || innerNodeActionFilters.size() == 0;
    }
    
    public Object doInnerNodeActionFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, Object> innerNodeActionInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, Object> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, Object>(innerNodeActionFilters, innerNodeActionInvoker);
        return chain.doFilter(p);
    }
    
    /* NodePreHandlerFilters*/
    private List<Filter<Pair<NodeContext, FlowContext>, Boolean>> innerNodePreHandlerFilters;
    private List<Filter<Pair<NodeContext, FlowContext>, Boolean>> outerNodePreHandlerFilters;

    public List<Filter<Pair<NodeContext, FlowContext>, Boolean>> getNodePreHandlerFilters() {
        return nodePreHandlerFilters;
    }
    
    public List<Filter<Pair<NodeContext, FlowContext>, Boolean>> getInnerNodePreHandlerFilters() {
        return innerNodePreHandlerFilters;
    }

    public List<Filter<Pair<NodeContext, FlowContext>, Boolean>> getOuterNodePreHandlerFilters() {
        return outerNodePreHandlerFilters;
    }

    public void setNodePreHandlerFilters(List<Filter<Pair<NodeContext, FlowContext>, Boolean>> nodePreHandlerFilters) {
        this.nodePreHandlerFilters = nodePreHandlerFilters;
        refeshNodeActionFilters();
    }
    
    private void refreshNodePreHandlerFilters() {
        if (nodePreHandlerFilters == null) {
            this.innerNodePreHandlerFilters = this.outerNodePreHandlerFilters = null;
        } else {
            this.innerNodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Boolean>>();
            this.outerNodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Boolean>>();
            for (Filter filter : nodePreHandlerFilters) {
                addFilter(filter, this.innerNodePreHandlerFilters, this.outerNodePreHandlerFilters);
            }
        }
    }

    public void addNodePreHandlerFilter(Filter<Pair<NodeContext, FlowContext>, Boolean> filter) {
        if (this.nodePreHandlerFilters == null) {
            this.nodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Boolean>>();
            this.innerNodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,Boolean>>();
            this.outerNodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,Boolean>>();
        }
        this.nodePreHandlerFilters.add(filter);
        addFilter(filter, this.innerNodePreHandlerFilters, this.outerNodePreHandlerFilters);
    }
    
    public boolean noOuterNodePreHandlerFilter() {
        return outerNodePreHandlerFilters == null || outerNodePreHandlerFilters.size() == 0;
    }
    
    public Boolean doOuterNodePreHandlerFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, Boolean> outerNodePreHandlerInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, Boolean> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, Boolean>(outerNodePreHandlerFilters, outerNodePreHandlerInvoker);
        return chain.doFilter(p);
    }
    
    public boolean noInnerNodePreHandlerFilter() {
        return innerNodePreHandlerFilters == null || innerNodePreHandlerFilters.size() == 0;
    }
    
    public Boolean doInnerNodePreHandlerFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, Boolean> innerNodePreHandlerInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, Boolean> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, Boolean>(innerNodePreHandlerFilters, innerNodePreHandlerInvoker);
        return chain.doFilter(p);
    }

    /* NodePostHandlerFilter */
    private List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> innerNodePostHandlerFilters;
    private List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> outerNodePostHandlerFilters;
    
    public List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> getNodePostHandlerFilters() {
        return nodePostHandlerFilters;
    }

    public List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> getInnerNodePostHandlerFilters() {
        return innerNodePostHandlerFilters;
    }

    public List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> getOuterNodePostHandlerFilters() {
        return outerNodePostHandlerFilters;
    }

    public void setNodePostHandlerFilters(
            List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> nodePostHandlerFilters) {
        this.nodePostHandlerFilters = nodePostHandlerFilters;
        refeshNodePostHandlerFilters();
    }
    
    private void refeshNodePostHandlerFilters() {
        if (nodePostHandlerFilters == null) {
            this.innerNodePostHandlerFilters = this.outerNodePostHandlerFilters = null;
        } else {
            this.innerNodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,NodeContext[]>>();
            this.outerNodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext,FlowContext>,NodeContext[]>>();
            for (Filter filter : nodePostHandlerFilters) {
                addFilter(filter, this.innerNodePostHandlerFilters, this.outerNodePostHandlerFilters);
            }
        }
    }
    
    public void addNodePostHandlerFilter(Filter<Pair<NodeContext, FlowContext>, NodeContext[]> filter) {
        if (this.nodePostHandlerFilters == null) {
            this.nodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>>();
            this.innerNodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>>();
            this.outerNodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>>();
        }
        this.nodePostHandlerFilters.add(filter);
        addFilter(filter, this.innerNodePostHandlerFilters, this.outerNodePostHandlerFilters);
    }
    
    public boolean noOuterNodePostHandlerFilter() {
        return outerNodePostHandlerFilters == null || outerNodePostHandlerFilters.size() == 0;
    }
    
    public NodeContext[] doOuterNodePostHandlerFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, NodeContext[]> outerNodePostHandlerInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, NodeContext[]> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, NodeContext[]>(outerNodePostHandlerFilters, outerNodePostHandlerInvoker);
        return chain.doFilter(p);
    }
    
    public boolean noInnerNodePostHandlerFilter() {
        return innerNodePostHandlerFilters == null || innerNodePostHandlerFilters.size() == 0;
    }
    
    public NodeContext[] doInnerNodePostHandlerFilter(Pair<NodeContext, FlowContext> p, Function<Pair<NodeContext, FlowContext>, NodeContext[]> innerNodePostHandlerInvoker) {
        FilterChain<Pair<NodeContext, FlowContext>, NodeContext[]> chain = new FilterChainImpl<Pair<NodeContext,FlowContext>, NodeContext[]>(innerNodePostHandlerFilters, innerNodePostHandlerInvoker);
        return chain.doFilter(p);
    }
    
    /* FlowPreHandlerFilter */
    private List<Filter<FlowContext, Boolean>> innerFlowPreHandlerFilters;
    private List<Filter<FlowContext, Boolean>> outerFlowPreHandlerFilters;
    
    public List<Filter<FlowContext, Boolean>> getFlowPreHandlerFilters() {
        return flowPreHandlerFilters;
    }
    
    public List<Filter<FlowContext, Boolean>> getInnerFlowPreHandlerFilters() {
        return innerFlowPreHandlerFilters;
    }

    public List<Filter<FlowContext, Boolean>> getOuterFlowPreHandlerFilters() {
        return outerFlowPreHandlerFilters;
    }

    public void setFlowPreHandlerFilters(List<Filter<FlowContext, Boolean>> flowPreHandlerFilters) {
        this.flowPreHandlerFilters = flowPreHandlerFilters;
        refreshFlowPreHandlerFilters();
    }
    
    private void refreshFlowPreHandlerFilters() {
        if (flowPreHandlerFilters == null) {
            this.innerFlowPreHandlerFilters = this.outerFlowPreHandlerFilters = null;
        } else {
            this.innerFlowPreHandlerFilters = new ArrayList<Filter<FlowContext,Boolean>>();
            this.outerFlowPreHandlerFilters = new ArrayList<Filter<FlowContext,Boolean>>();
            for (Filter filter : flowPreHandlerFilters) {
                addFilter(filter, this.innerFlowPreHandlerFilters, this.outerFlowPreHandlerFilters);
            }
        }
    }
    
    public void addFlowPreHandlerFilter(Filter<FlowContext, Boolean> filter) {
        if (this.flowPreHandlerFilters == null) {
            this.flowPreHandlerFilters = new ArrayList<Filter<FlowContext, Boolean>>();
            this.innerFlowPreHandlerFilters = new ArrayList<Filter<FlowContext,Boolean>>();
            this.outerFlowPreHandlerFilters = new ArrayList<Filter<FlowContext,Boolean>>();
        }
        this.flowPreHandlerFilters.add(filter);
        addFilter(filter, this.innerFlowPreHandlerFilters, this.outerFlowPostHandlerFilters);
    }
    
    public boolean noOuterFlowPreHandlerFilter() {
        return outerFlowPreHandlerFilters == null || outerFlowPreHandlerFilters.size() == 0;
    }
    
    public Boolean doOuterFlowPreHandlerFilter(FlowContext context, Function<FlowContext, Boolean> outerFlowPreHandlerInvoker) {
        FilterChain<FlowContext, Boolean> chain = new FilterChainImpl<>(outerFlowPreHandlerFilters, outerFlowPreHandlerInvoker);
        return chain.doFilter(context);
    }
    
    public boolean noInnerFlowPreHandlerFilter() {
        return innerFlowPreHandlerFilters == null || innerFlowPreHandlerFilters.size() == 0;
    }
    
    public Boolean doInnerFlowPreHandlerFilter(FlowContext context, Function<FlowContext, Boolean> innerFlowPreHandlerInvoker) {
        FilterChain<FlowContext, Boolean> chain = new FilterChainImpl<>(innerFlowPreHandlerFilters, innerFlowPreHandlerInvoker);
        return chain.doFilter(context);
    }

    /* FlowPostHandlerFilter */
    private List<Filter<FlowContext, Void>> innerFlowPostHandlerFilters;
    private List<Filter<FlowContext, Void>> outerFlowPostHandlerFilters;
    
    public List<Filter<FlowContext, Void>> getFlowPostHandlerFilters() {
        return flowPostHandlerFilters;
    }
    
    public List<Filter<FlowContext, Void>> getInnerFlowPostHandlerFilters() {
        return innerFlowPostHandlerFilters;
    }

    public List<Filter<FlowContext, Void>> getOuterFlowPostHandlerFilters() {
        return outerFlowPostHandlerFilters;
    }

    public void setFlowPostHandlerFilters(List<Filter<FlowContext, Void>> flowPostHandlerFilters) {
        this.flowPostHandlerFilters = flowPostHandlerFilters;
        refreshFlowPostHandlerFilters();
    }
    
    private void refreshFlowPostHandlerFilters() {
        if (flowPostHandlerFilters == null) {
            this.innerFlowPostHandlerFilters = this.outerFlowPostHandlerFilters = null;
        } else {
            this.innerFlowPostHandlerFilters = new ArrayList<Filter<FlowContext,Void>>();
            this.outerFlowPostHandlerFilters = new ArrayList<Filter<FlowContext,Void>>();
            for (Filter filter : flowPostHandlerFilters) {
                addFilter(filter, this.innerFlowPostHandlerFilters, this.outerFlowPostHandlerFilters);
            }
        }
    }
    
    public void addFlowPostHandlerFilter(Filter<FlowContext, Void> filter) {
        if (this.flowPostHandlerFilters == null) {
            this.flowPostHandlerFilters = new ArrayList<Filter<FlowContext, Void>>();
            this.innerFlowPostHandlerFilters = new ArrayList<Filter<FlowContext,Void>>();
            this.outerFlowPostHandlerFilters = new ArrayList<Filter<FlowContext,Void>>();
        }
        this.flowPostHandlerFilters.add(filter);
        addFilter(filter, this.innerFlowPostHandlerFilters, this.outerFlowPostHandlerFilters);
    }
    
    public boolean noOuterFlowPostHandlerFilter() {
        return outerFlowPostHandlerFilters == null || outerFlowPostHandlerFilters.size() == 0;
    }
    
    public Void doOuterFlowPostHandlerFilter(FlowContext context, Function<FlowContext, Void> outerFlowPostHandlerInvoker) {
        FilterChain<FlowContext, Void> chain = new FilterChainImpl<>(outerFlowPostHandlerFilters, outerFlowPostHandlerInvoker);
        return chain.doFilter(context);
    }
    
    public boolean noInnerFlowPostHandlerFilter() {
        return innerFlowPostHandlerFilters == null || innerFlowPostHandlerFilters.size() == 0;
    }
    
    public Void doInnerFlowPostHandlerFilter(FlowContext context, Function<FlowContext, Void> innerFlowPostHandlerInvoker) {
        FilterChain<FlowContext, Void> chain = new FilterChainImpl<>(innerFlowPostHandlerFilters, innerFlowPostHandlerInvoker);
        return chain.doFilter(context);
    }
    
    private void addFilter(Filter filter, List innerFilters, List outerFilters) {
        int filterOrder = filter.getOrder();
        int pos = 0;
        if (filterOrder < 0) {
            for (; pos < innerFilters.size(); pos++) {
                if (((Filter) innerFilters.get(pos)).getOrder() < filterOrder) {
                    break;
                }
            }
            innerFilters.add(pos, filter);
        } else {
            for (; pos < outerFilters.size(); pos++) {
                if (((Filter)outerFilters.get(pos)).getOrder() < filterOrder) {
                    break;
                }
            }
            outerFilters.add(pos, filter);
        }
    }

}

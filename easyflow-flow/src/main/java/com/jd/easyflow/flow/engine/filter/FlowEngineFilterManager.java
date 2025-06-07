package com.jd.easyflow.flow.engine.filter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.filter.FilterChainImpl;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public class FlowEngineFilterManager {

    protected List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> filters;

    protected List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> innerFilters;
    protected List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> outerFilters;
    
    public void init(InitContext initContext, FlowEngine flowEngine) {
        if (filters != null) {
            filters.forEach(filter -> {
                filter.init(initContext, flowEngine);
            });
            refreshFilters();
        }
    }
    
    public boolean noOuterFilter() {
        return outerFilters == null || outerFilters.size() == 0;
    }
    
    public FlowResult doOuterFilter(Pair<FlowParam, FlowEngine> p, Function<Pair<FlowParam, FlowEngine>, FlowResult> outerInvoker) {
        FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain = new FilterChainImpl<Pair<FlowParam, FlowEngine>, FlowResult>(outerFilters,
                outerInvoker);
        return chain.doFilter(p);
    }
    
    public boolean noInnerFilter() {
        return innerFilters == null || innerFilters.size() == 0;
    }
    
    public FlowResult doInnerFilter(Pair<FlowParam, FlowEngine> p, Function<Pair<FlowParam, FlowEngine>, FlowResult> innerInvoker) {
        FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain = new FilterChainImpl<Pair<FlowParam, FlowEngine>, FlowResult>(outerFilters,
                innerInvoker);
        return chain.doFilter(p);
    }
    

    public List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> filters) {
        this.filters = filters;
        refreshFilters();
    }
    
    private void refreshFilters() {
        if (filters == null) {
            this.innerFilters = this.outerFilters = null;
        } else {
            this.innerFilters = new ArrayList<Filter<Pair<FlowParam, FlowEngine>, FlowResult>>();
            this.outerFilters = new ArrayList<Filter<Pair<FlowParam, FlowEngine>, FlowResult>>();
            for (Filter filter : filters) {
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
                        if (((Filter) outerFilters.get(pos)).getOrder() < filterOrder) {
                            break;
                        }
                    }
                    outerFilters.add(pos, filter);
                }
            }
        }
    }

    public List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> getInnerFilters() {
        return innerFilters;
    }

    public List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> getOuterFilters() {
        return outerFilters;
    }

    
    
    

}

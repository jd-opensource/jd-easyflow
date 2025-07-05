package com.jd.easyflow.flow.ext.interrupt;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.InterruptNodeAction;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class ExtInterruptNodeAction extends InterruptNodeAction {

    @Override
    public void init(InitContext initContext, Object flowNode) {
        initFlowFilter(initContext, flowNode);
        initNodeFilter(initContext, flowNode);
    }
    
    private void initFlowFilter(InitContext initContext, Object flowNode) {
        List<Filter<FlowContext, FlowResult>> filters = initContext.getFlow().getFilterManager().getFilters();
        boolean contains = false;
        if (filters == null) {
            filters = new ArrayList<>();
            initContext.getFlow().getFilterManager().setFilters(filters);
        }
        for (Filter filter : filters) {
            if (filter instanceof ExtNodeThreadInterruptFlowFilter) {
                contains = true;
                break;
            }
        }
        if (!contains) {
            filters.add(0, new ExtNodeThreadInterruptFlowFilter());
            initContext.getFlow().getFilterManager().setFilters(filters);
        }
    }
    
    private void initNodeFilter(InitContext initContext, Object flowNode) {
        List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> filters = initContext.getFlow().getFilterManager().getNodeFilters();
        boolean contains = false;
        if (filters == null) {
            filters = new ArrayList<>();
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
        for (Filter filter : filters) {
            if (filter instanceof ExtNodeThreadInterruptNodeFilter) {
                contains = true;
                break;
            }
        }
        if (!contains) {
            filters.add(0, new ExtNodeThreadInterruptNodeFilter());
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
    }
    

}

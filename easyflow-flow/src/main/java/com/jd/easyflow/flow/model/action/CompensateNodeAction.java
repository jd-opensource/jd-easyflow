package com.jd.easyflow.flow.model.action;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.compensate.CompensateHelper;
import com.jd.easyflow.flow.model.action.compensate.CompensateNodeFilter;
import com.jd.easyflow.flow.model.action.compensate.CompensateNodePreHandlerFilter;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class CompensateNodeAction implements NodeAction {
    
    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        CompensateHelper.compensate(context);
        return null;
    }
    
    @Override
    public void init(InitContext initContext, Object flowNode) {
        boolean recordHistory = ! Boolean.FALSE.equals(initContext.getFlow().getProperty(FlowConstants.FLOW_PROPERTY_RECORD_HISTORY));
        if (! recordHistory) {
            throw new FlowException("CompensateNodeAction must record history");
        }
        initNodeFilter(initContext, flowNode);
        initNodePreHandlerFilter(initContext, flowNode);
    }
    
    private void initNodeFilter(InitContext initContext, Object flowNode) {
        List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> filters = initContext.getFlow().getFilterManager().getNodeFilters();
        boolean contains = false;
        if (filters == null) {
            filters = new ArrayList<>();
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
        for (Filter filter : filters) {
            if (filter instanceof CompensateNodeFilter) {
                contains = true;
                break;
            }
        }
        if (!contains) {
            filters.add(0, new CompensateNodeFilter(Integer.MAX_VALUE));
            initContext.getFlow().getFilterManager().setNodeFilters(filters);
        }
    }
    
    private void initNodePreHandlerFilter(InitContext initContext, Object flowNode) {
        List<Filter<Pair<NodeContext, FlowContext>, Boolean>> nodePreHandlerFilters = initContext.getFlow().getFilterManager().getNodePreHandlerFilters();
        boolean contains = false;
        if (nodePreHandlerFilters == null) {
            nodePreHandlerFilters = new ArrayList<>();
            initContext.getFlow().getFilterManager().setNodePreHandlerFilters(nodePreHandlerFilters);
        }
        for (Filter filter : nodePreHandlerFilters) {
            if (filter instanceof CompensateNodePreHandlerFilter) {
                contains = true;
                break;
            }
        }
        if (!contains) {
            nodePreHandlerFilters.add(0, new CompensateNodePreHandlerFilter(Integer.MAX_VALUE));
            initContext.getFlow().getFilterManager().setNodePreHandlerFilters(nodePreHandlerFilters);
        }
    }

}


package com.jd.easyflow.flow.model.action.compensate;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class CompensateFlowParseEventListener implements FlowParseEventListener {

    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            // CompensateNodeFilter
            Flow flow = event.getFlow();
            List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> nodeFilters = flow.getFilterManager().getNodeFilters();
            boolean contains = false;
            if (nodeFilters == null) {
                nodeFilters = new ArrayList<>();
                flow.getFilterManager().setNodeFilters(nodeFilters);
            }
            for (Filter filter : nodeFilters) {
                if (filter instanceof CompensateNodeFilter) {
                    contains = true;
                    break;
                }
            }
            if (!contains) {
                nodeFilters.add(0, new CompensateNodeFilter(Integer.MAX_VALUE));
                flow.getFilterManager().setNodeFilters(nodeFilters);
            }
            
            // CompensateFlowFilter
            List<Filter<FlowContext, FlowResult>> filters = flow.getFilterManager().getFilters();
            contains = false;
            if (filters == null) {
                filters = new ArrayList<>();
                flow.getFilterManager().setFilters(filters);
            }
            for (Filter filter : filters) {
                if (filter instanceof CompensateFlowFilter) {
                    contains = true;
                    break;
                }
            }
            if (!contains) {
                filters.add(0, new CompensateFlowFilter(Integer.MAX_VALUE));
                flow.getFilterManager().setFilters(filters);
            }            
            
        }
        }

    }
}

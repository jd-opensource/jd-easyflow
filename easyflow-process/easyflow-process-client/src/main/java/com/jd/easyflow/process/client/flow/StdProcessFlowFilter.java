package com.jd.easyflow.process.client.flow;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdProcessFlowFilter extends StdProcessFlowListener implements Filter<FlowContext, FlowResult> {

    @Override
    public FlowResult doFilter(FlowContext context, FilterChain<FlowContext, FlowResult> chain) {
        super.onFlowStart(context);
        try {
            FlowResult result = chain.doFilter(context);
            context.setResult(result);
            super.onFlowEnd(context);
            return context.getResult();
        } finally {
            super.onFlowComplete(context);
        }
    }

}

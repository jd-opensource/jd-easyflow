package com.jd.easyflow.flow.ext.interrupt;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.filter.impl.BaseFlowFilter;

/**
 * @author liyuliang5
 */
public class ExtNodeThreadInterruptFlowFilter extends BaseFlowFilter {
    
    private static final Logger logger = LoggerFactory.getLogger(ExtNodeThreadInterruptFlowFilter.class);
    
    public ExtNodeThreadInterruptFlowFilter() {}
    
    public ExtNodeThreadInterruptFlowFilter(int order) {
        this.order = order;
    }

    @Override
    public FlowResult doFilter(FlowContext request, FilterChain<FlowContext, FlowResult> chain) {
        Set<ExtNodeThreadHolder> holderSet = Collections.synchronizedSet(new HashSet<>());
        request.put(ExtNodeThreadInterruptHelper.CTX_NODE_THREAD_HODLER_SET, holderSet);
        try {
            return chain.doFilter(request);
        } finally {
            for (ExtNodeThreadHolder holder : holderSet) {
                synchronized (holder.lock) {
                    if (!holder.complete) {
                        if (request.isLogOn() && logger.isInfoEnabled()) {
                            logger.info(holder.nodeContext.getNodeId() + " interrupted");
                        }
                        holder.executionThread.interrupt();
                    }
                }
            }
            holderSet.clear();
        }
    }

}

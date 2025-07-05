package com.jd.easyflow.flow.model.action.compensate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.impl.BaseNodePreHandlerFilter;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public class CompensateNodePreHandlerFilter extends BaseNodePreHandlerFilter {
    
    private static final Logger logger = LoggerFactory.getLogger(CompensateNodePreHandlerFilter.class);
    
    public CompensateNodePreHandlerFilter() {
        
    }
    
    public CompensateNodePreHandlerFilter(int order) {
        this.order = order;
    }

    @Override
    public Boolean doFilter(Pair<NodeContext, FlowContext> request,
            FilterChain<Pair<NodeContext, FlowContext>, Boolean> chain) {
        Boolean result = chain.doFilter(request);
        if (CompensateHelper.isCompensating(request.getRight())) {
            if (request.getRight().isLogOn() && logger.isInfoEnabled()) {
                logger.info("Compensating, NodePreHandler return false, node:" + request.getLeft().getNodeId());
            }
            return false;
        } else {
            return result;
        }
    }

}

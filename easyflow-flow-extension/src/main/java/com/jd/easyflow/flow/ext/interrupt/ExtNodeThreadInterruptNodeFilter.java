package com.jd.easyflow.flow.ext.interrupt;

import java.util.Set;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.impl.BaseNodeFilter;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public class ExtNodeThreadInterruptNodeFilter extends BaseNodeFilter {
    
    public ExtNodeThreadInterruptNodeFilter() {}
    
    public ExtNodeThreadInterruptNodeFilter(int order) {
        this.order = order;
    }

    @Override
    public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
            FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
        ExtNodeThreadHolder holder = new ExtNodeThreadHolder();
        holder.nodeContext = request.getMiddle();
        holder.executionThread = Thread.currentThread();
        Set<ExtNodeThreadHolder> set = request.getRight().get(ExtNodeThreadInterruptHelper.CTX_NODE_THREAD_HODLER_SET);
        set.add(holder);
        try {
            return chain.doFilter(request);
        } finally {
            synchronized (holder.lock) {
                holder.complete = true;
                Thread.interrupted();
            }
        }
    }

}

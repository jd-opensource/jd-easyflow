package com.jd.easyflow.process.client.flow;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.Triple;

/**
 * 
 * @author liyuliang5
 *
 */
public class StdProcessFlowNodeFilter extends StdProcessFlowListener
        implements Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> {

    @Override
    public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
            FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
        super.onNodeStart(request.getMiddle(), request.getRight());
        NodeContext nodeContext = chain.doFilter(request);
        super.onNodeEnd(request.getMiddle(), request.getRight());
        return nodeContext;
    }

}

package com.jd.easyflow.flow.cases.posthandler;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Test node post handler filter.
 * 
 * @author liyuliang5
 *
 */
public class TestPostHandlerFilter implements Filter<Pair<NodeContext, FlowContext>, NodeContext[]> {

    private static final Logger logger = LoggerFactory.getLogger(TestPostHandlerFilter.class);

    @Override
    public NodeContext[] doFilter(Pair<NodeContext, FlowContext> request,
            FilterChain<Pair<NodeContext, FlowContext>, NodeContext[]> chain) {
        NodeContext[] result = chain.doFilter(request);
        if ("STEP1".equals(request.getLeft().getNodeId())) {
            logger.info("origin result:" + result[0].getNodeId());
            NodeContext nextNodeContext = new NodeContext();
            nextNodeContext.setNodeId("STEP3");
            // request.getLeft().setNextNodes(new NodeContext[] { nextNodeContext });
            return new NodeContext[] { nextNodeContext };
        }
        return result;
    }

}

package com.jd.easyflow.flow.cases.parser;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;

/**
 * 
 * @author liyuliang5
 */
public class TestFlowParamAndResultPrintFilter implements Filter<FlowContext, FlowResult> {

    private static final Logger logger = LoggerFactory.getLogger(TestFlowParamAndResultPrintFilter.class);

    @Override
    public FlowResult doFilter(FlowContext request, FilterChain<FlowContext, FlowResult> chain) {
        logger.info("flowId is:" + request.getParam().getFlowId());
        FlowResult result = chain.doFilter(request);
        logger.info("flow result is:" + result.getResult());
        return result;

    }
}

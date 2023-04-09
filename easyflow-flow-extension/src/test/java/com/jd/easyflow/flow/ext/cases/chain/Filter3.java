package com.jd.easyflow.flow.ext.cases.chain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;

public class Filter3 implements Filter<FlowContext, FlowResult>{
    
    private static final Logger logger = LoggerFactory.getLogger(Filter3.class);

    @Override
    public FlowResult doFilter(FlowContext request, FilterChain<FlowContext, FlowResult> chain) {
        logger.info("filter3 start");
        FlowResult result = chain.doFilter(request);
        logger.info("filter3 end");
        return result;
    }

}

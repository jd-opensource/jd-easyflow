package com.jd.easyflow.process.client.fsm;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;

/**
 * 
 * @author liyuliang5
 */
public class StdProcessFsmFilter extends StdProcessFsmListener implements Filter<FsmContext, FsmResult> {

    @Override
    public FsmResult doFilter(FsmContext context, FilterChain<FsmContext, FsmResult> chain) {
        super.onFsmStart(context);
        try {
            FsmResult result = chain.doFilter(context);
            context.setResult(result);
            super.onFsmEnd(context);
            return context.getResult();
        } finally {
            super.onFsmComplete(context);
        }
    }

}

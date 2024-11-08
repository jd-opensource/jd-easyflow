package com.jd.easyflow.process.client.fsm;

import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 */
public class StdProcessFsmManagerFilter extends StdProcessFsmListener implements Filter<Pair<FsmParam, FsmManager>, FsmResult> {

    @Override
    public FsmResult doFilter(Pair<FsmParam, FsmManager> param, FilterChain<Pair<FsmParam, FsmManager>, FsmResult> chain) {
        super.onFsmManagerStart(param.getLeft(), param.getRight());
        return chain.doFilter(param);
    }

}

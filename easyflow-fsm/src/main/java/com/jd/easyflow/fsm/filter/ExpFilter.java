package com.jd.easyflow.fsm.filter;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.el.ElEvaluator;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.util.Pair;
import com.jd.easyflow.fsm.util.Triple;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public class ExpFilter<T, R> implements Filter<T, R> {

    private ElEvaluator elEvaluator;

    private String exp;

    public ExpFilter() {

    }

    public ExpFilter(String exp) {
        this.exp = exp;
    }

    @Override
    public R doFilter(T request, FilterChain<T, R> chain) {
        Map<String, Object> data = new HashMap<>();
        data.put("request", request);
        data.put("chain", chain);
        ElEvaluator evaluator = this.elEvaluator;
        if (evaluator == null) {
            if (request instanceof FsmContext) {
                evaluator = ((FsmContext) request).getElEvaluator();
            } else if (request instanceof Pair) {
                if (((Pair) request).getRight() instanceof FsmContext) {
                    evaluator = ((FsmContext) ((Pair) request).getRight()).getElEvaluator();
                } else if (((Pair) request).getRight() instanceof FsmManager) {
                    evaluator = ((FsmManager) ((Pair) request).getRight()).getElEvaluator();
                }
            } else if (request instanceof Triple && ((Triple) request).getRight() instanceof FsmContext) {
                evaluator = ((FsmContext) ((Triple) request).getRight()).getElEvaluator();
            }
        }

        if (evaluator == null) {
            evaluator = ElFactory.get();
        }
        return evaluator.eval(exp, null, null, data);
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }

    public String getExp() {
        return exp;
    }

    public void setExp(String exp) {
        this.exp = exp;
    }

}

package com.jd.easyflow.flow.filter;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElEvaluator;
import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.util.JsonUtil;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.flow.util.Triple;

/**
 * 
 * @author liyuliang5
 *
 * @param <T>
 * @param <R>
 */
public class ExpFilter<T, R> extends BaseFilter<T, R> {

    private static final Logger logger = LoggerFactory.getLogger(ExpFilter.class);

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
        if (logger.isDebugEnabled()) {
            logger.debug("EVAL SPEL:" + exp);
        }
        ElEvaluator evaluator = this.elEvaluator;
        if (evaluator == null) {
            if (request instanceof FlowContext) {
                evaluator = ((FlowContext) request).getElEvaluator();
            } else if (request instanceof Pair) {
                if (((Pair) request).getRight() instanceof FlowContext) {
                    evaluator = ((FlowContext) ((Pair) request).getRight()).getElEvaluator();
                } else if (((Pair) request).getRight() instanceof FlowEngine) {
                    evaluator = ((FlowEngine) ((Pair) request).getRight()).getElEvaluator();
                }
            } else if (request instanceof Triple && ((Triple) request).getRight() instanceof FlowContext) {
                evaluator = ((FlowContext) ((Triple) request).getRight()).getElEvaluator();
            }
        }

        if (evaluator == null) {
            evaluator = ElFactory.get();
        }
        R result = evaluator.evalWithDefaultContext(exp, data, true);
        if (logger.isDebugEnabled()) {
            try {
                logger.debug("SPEL RESULT:" + JsonUtil.toJsonString(result));
            } catch (Throwable t) {
                logger.debug("SPEL RESULT to json string exception:" + t.getMessage());
            }
        }
        return result;
    }

    public String getExp() {
        return exp;
    }

    public void setExp(String exp) {
        this.exp = exp;
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }

}

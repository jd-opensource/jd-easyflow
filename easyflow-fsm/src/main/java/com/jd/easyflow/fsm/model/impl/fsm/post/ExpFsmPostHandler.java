package com.jd.easyflow.fsm.model.impl.fsm.post;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.FsmPostHandler;

public class ExpFsmPostHandler implements FsmPostHandler {

    private static final Logger logger = LoggerFactory.getLogger(ExpFsmPostHandler.class);

    private String exp;

    public ExpFsmPostHandler() {
    }

    public ExpFsmPostHandler(String exp) {
        this.exp = exp;
    }

    @Override
    public void postHandle(FsmContext context) {
        Object result = context.getElEvaluator().eval(exp, null, context, null);
        if (logger.isInfoEnabled()) {
            logger.info("Exp:" + exp + " result:" + result);
        }
    }

    public String getExp() {
        return exp;
    }

    public void setExp(String exp) {
        this.exp = exp;
    }

}

package com.jd.easyflow.fsm.model.impl.fsm.pre;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.model.FsmPreHandler;
import com.jd.easyflow.fsm.model.impl.fsm.post.ExpFsmPostHandler;

public class ExpFsmPreHandler implements FsmPreHandler {

    private static final Logger logger = LoggerFactory.getLogger(ExpFsmPostHandler.class);

    private String exp;

    public ExpFsmPreHandler() {
    }

    public ExpFsmPreHandler(String exp) {
        this.exp = exp;
    }

    @Override
    public boolean preHandle(FsmContext context) {
        boolean result = ElFactory.get().eval(exp, null, context, null);
        if (logger.isInfoEnabled()) {
            logger.info("Exp:" + exp + " result:" + result);
        }
        return result;
    }

    public String getExp() {
        return exp;
    }

    public void setExp(String exp) {
        this.exp = exp;
    }

}

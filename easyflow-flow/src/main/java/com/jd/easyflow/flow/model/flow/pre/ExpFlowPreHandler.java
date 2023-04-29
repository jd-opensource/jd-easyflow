package com.jd.easyflow.flow.model.flow.pre;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowPreHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFlowPreHandler implements FlowPreHandler {

    private static final Logger logger = LoggerFactory.getLogger(ExpFlowPreHandler.class);

    private String exp;

    public ExpFlowPreHandler() {
    }

    public ExpFlowPreHandler(String exp) {
        this.exp = exp;
    }

    @Override
    public boolean preHandle(FlowContext context) {
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

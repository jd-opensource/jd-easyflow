package com.jd.easyflow.flow.model.flow.post;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowPostHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpFlowPostHandler implements FlowPostHandler {

    private static final Logger logger = LoggerFactory.getLogger(ExpFlowPostHandler.class);

    private String exp;

    public ExpFlowPostHandler() {
    }

    public ExpFlowPostHandler(String exp) {
        this.exp = exp;
    }

    @Override
    public void postHandle(FlowContext context) {
        Object result = ElFactory.get().eval(exp, null, context, null);
        if (context.isLogOn() && logger.isInfoEnabled()) {
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

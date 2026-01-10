package com.jd.easyflow.processunit.client.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecPolicy;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.service.ProcessUnitExecutor;

/**
 * @author liyuliang5
 *
 */
public abstract class BaseProcessUnitExecutor implements ProcessUnitExecutor {
    
    private static final Logger log = LoggerFactory.getLogger(BaseProcessUnitExecutor.class);

    @Override
    public ExecResult execute(ExecParam param) {
        ExecContext context = new ExecContext(param);
        try {
            ExecPolicy policy = beforeCall(context);
            log.info("Return exec policy:{}", (policy == null ? null : policy.getPolicyType()));
            context.setPolicy(policy);
            call(policy, context);
        } finally {
            afterCall(context);
        }
        return context.getResult();
    }

    public void afterCall(ExecContext context) {
        throw new UnsupportedOperationException();
    }

    public void call(ExecPolicy policy, ExecContext context) {
        throw new UnsupportedOperationException();
    }

    public ExecPolicy beforeCall(ExecContext context) {
        throw new UnsupportedOperationException();
    }

}

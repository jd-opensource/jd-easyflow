package com.jd.easyflow.processunit.domain.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.domain.model.vo.ExecParam;
import com.jd.easyflow.processunit.domain.model.vo.ExecPolicy;
import com.jd.easyflow.processunit.domain.model.vo.ExecResult;
import com.jd.easyflow.processunit.domain.service.ProcessUnitExecutor;

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
            log.info("Return exec policy:{}" + (policy == null ? null : policy.getPolicyType()));
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

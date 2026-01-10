package com.jd.easyflow.processunit.client.gateway;

import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecPolicy;

/**
 * @author liyuliang5
 *
 */
public interface ProcessUnitServerGateway {

    public ExecPolicy syncBeforeCall(ExecContext context);
    
    public void syncAfterCall(ExecContext context);
}

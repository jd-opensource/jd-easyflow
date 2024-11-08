package com.jd.easyflow.process.client.task.service;

/**
 * @author liyuliang5
 *
 */
public interface TaskClientOperation<P,R> {
    
    public R operate(P param);

}

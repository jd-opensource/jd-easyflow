package com.jd.easyflow.flow.cases.runner;

import java.util.concurrent.Executors;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy;
import java.util.concurrent.TimeUnit;

import com.jd.easyflow.flow.engine.impl.ReusableThreadFlowRunner;

/**
 * 
 * @author liyuliang5
 *
 */
public class TestReusableThreadRunner extends ReusableThreadFlowRunner {

    public TestReusableThreadRunner() {
        this.executor = new ThreadPoolExecutor(0, Integer.MAX_VALUE, 60L, TimeUnit.SECONDS,
                new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new AbortPolicy());
    }

}

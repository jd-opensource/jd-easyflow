package com.jd.easyflow.flow.ext.timeout;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * 
 * @author liyuliang5
 */
public class TimeoutTemplate {
    
    private static final Logger logger = LoggerFactory.getLogger(TimeoutTemplate.class);
    
    private static final TimeoutTemplate INSTANCE = new TimeoutTemplate();
    
    public static TimeoutTemplate getInstance() {
        return INSTANCE;
    }
    
    public <T>T execute(Callable<T> callable, Callable<T> timeoutCallable, long timeoutMillis, ExecutorService executorService, boolean logOn) {
        return execute(callable, timeoutCallable, timeoutMillis, executorService, logOn, true);
    }
    
    public <T>T execute(Callable<T> callable, Callable<T> timeoutCallable, long timeoutMillis, ExecutorService executorService, boolean logOn, boolean interruptOnTimeout) {
        ExecutionThreadHolder holder = new ExecutionThreadHolder();
        Future<T> future = executorService.submit(() -> {
            if (holder.timeout) {
                if (logOn) {
                    logger.warn("caller timeout, return");
                }
                return null;
            }
            holder.executionThread = Thread.currentThread();
            try {
                T result = callable.call();
                return result;
            } finally {
                if (interruptOnTimeout) {
                    synchronized (holder.lock) {
                        holder.complete = true;
                        Thread.interrupted();
                    }
                }
            }
        });

        T result = null;
        try {
            result = future.get(timeoutMillis, TimeUnit.MILLISECONDS);
        } catch (TimeoutException e) {
            logger.error("node action timeout!");
            holder.timeout = true;
            if (interruptOnTimeout) {
                synchronized (holder.lock) {
                    if (!holder.complete && holder.executionThread != null) {
                        holder.executionThread.interrupt();
                    }
                }
            }
            try {
                result = timeoutCallable.call();
            } catch (Exception e1) {
                throw ExceptionUtil.throwException(e1);
            }
        } catch (ExecutionException e) {
            if (logOn) {
                logger.error("execution error!");
            }
            throw ExceptionUtil.throwException(e.getCause());
        } catch (InterruptedException e) {
            logger.warn("thread interrupted!");
            throw ExceptionUtil.throwException(e);
        }

        return result;
    }
    
    private static class ExecutionThreadHolder {
        volatile Thread executionThread;
        Object lock = new Object();
        volatile boolean complete = false;
        volatile boolean timeout = false;
    }


}

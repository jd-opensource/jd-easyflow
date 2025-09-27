package com.jd.easyflow.flow.engine.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.ExceptionUtil;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * Multiple thread runner.
 * 
 * @author liyuliang5
 * @date 2021/07/26
 */
public class MultipleThreadFlowRunner extends BaseFlowRunner {

    private static final Logger logger = LoggerFactory.getLogger(MultipleThreadFlowRunner.class);

    protected static long startId = System.currentTimeMillis();

    protected Executor executor;
    
    protected long timeout = 0;
    
    protected boolean throwExceptionOnTimeout = false;
    
    public MultipleThreadFlowRunner() {
        
    }
    
    public MultipleThreadFlowRunner(Executor executor, long timeout) {
        this.executor = executor;
        this.timeout = timeout;
    }
    
    public MultipleThreadFlowRunner(Executor executor, long timeout, boolean throwExceptionOnTimeout) {
        this.executor = executor;
        this.timeout = timeout;
        this.throwExceptionOnTimeout = throwExceptionOnTimeout;
    }

    @Override
    public void runNodes(FlowContextImpl context) {
        String runId = startId++ + "";
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("Start running flow node, runId:" + runId);
        }
        Lock lock = createLock(context);
        AtomicInteger counter = new AtomicInteger();
        scheduleNodes(context, counter, lock, runId);
        try {
            if (timeout == 0) {
                lock.lockInterruptibly();
            } else {
                boolean result = lock.tryLock(timeout, TimeUnit.MILLISECONDS);
                context.put(FlowConstants.FLOW_CTX_MULTI_AWAIT_RESULT, result);
                if (result == false) {
                    context.setInterrupted();
                    if (throwExceptionOnTimeout) {
                        throw new FlowException("flow execution timeout, runId:" + runId + ", flowId:" + context.getFlowId());
                    }
                }
            }
        } catch (InterruptedException e) {
            throw new FlowException(e);
        }
        List<NodeContext> exceptions = context.get(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS);
        // Default behavior is throwing first exception.
        if (exceptions != null && exceptions.size() > 0) {
            if (context.isLogOn()) {
                logger.error("Flow execute exception");
            }
            Throwable t = exceptions.get(0).get(FlowConstants.NODE_CTX_MULTI_EXCEPTION);
            throw ExceptionUtil.throwException(t);
        }
    }
    
    protected Lock createLock(FlowContextImpl context) {
        return new CountDownLatchLock();
    }
    
    protected void scheduleNodes(FlowContextImpl context, AtomicInteger counter, Lock lock,
            String runId) {
        addTaskIfExists(context, counter, lock, runId);
    }

    /**
     * Add task.
     * 
     * @param context
     * @param executor
     * @param counter
     * @param lock
     */
    private void addTaskIfExists(FlowContextImpl context, AtomicInteger counter, Lock lock,
            String runId) {
        NodeContext currentNode;
        while ((currentNode = context.getNextNode()) != null) {
            final NodeContext finalCurrentNode = currentNode;
            counter.addAndGet(1);
            executor.execute(() -> {
                try {
                    if (context.isLogOn() && logger.isInfoEnabled()) {
                        logger.info("Start execute flow node:" + finalCurrentNode.getNodeId() + ", runId:" + runId);
                    }
                    NodeContext[] nextNodes = runOneNode(finalCurrentNode, context);
                    if (nextNodes != null) {
                        context.addNodes(nextNodes);
                    }
                    if (context.isInterrupted()) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("Flow state is interrupted");
                        }
                        lock.unlock();
                        return;
                    }
                    addTaskIfExists(context, counter, lock, runId);
                    int count = counter.addAndGet(-1);
                    if (count == 0) {
                        lock.unlock();
                    }
                } catch (Throwable t) { // NOSONAR
                    addException(context, finalCurrentNode, t);
                    if (context.isInterrupted()) {
                        if (context.isLogOn() && logger.isInfoEnabled()) {
                            logger.info("Flow state is interrupted");
                        }
                        lock.unlock();
                        return;
                    }
                    int count = counter.addAndGet(-1);
                    if (count == 0) {
                        lock.unlock();
                    }
                }
            });
        }
    }

    /**
     * Add exception.
     * 
     * @param context
     * @param nodeContext
     * @param t
     */
    protected void addException(FlowContext context, NodeContext nodeContext, Throwable t) {
        synchronized (context) {
            List<NodeContext> exceptionNodes = context.get(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS);
            if (exceptionNodes == null) {
                exceptionNodes = new ArrayList<NodeContext>();
                context.put(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS, exceptionNodes);
            }
            nodeContext.put(FlowConstants.NODE_CTX_MULTI_EXCEPTION, t);
            exceptionNodes.add(nodeContext);
        }
    }

    protected void printStackTrace() {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < stack.length; i++) {
            builder.append(stack[i].toString() + "\n");
        }
        logger.error("Flow execute exception,\n " + builder);
    }

    public Executor getExecutor() {
        return executor;
    }

    public void setExecutor(Executor executor) {
        this.executor = executor;
    }

    public long getTimeout() {
        return timeout;
    }

    public void setTimeout(long timeout) {
        this.timeout = timeout;
    }

    public boolean isThrowExceptionOnTimeout() {
        return throwExceptionOnTimeout;
    }

    public void setThrowExceptionOnTimeout(boolean throwExceptionOnTimeout) {
        this.throwExceptionOnTimeout = throwExceptionOnTimeout;
    }
    
    private static class CountDownLatchLock implements Lock {
        
        private CountDownLatch latch = new CountDownLatch(1);


        @Override
        public void lockInterruptibly() throws InterruptedException {
            latch.await();
        }
        
        @Override
        public boolean tryLock(long time, TimeUnit unit) throws InterruptedException {
            return latch.await(time, unit);
        }
        
        @Override
        public void unlock() {
            latch.countDown();
        }

        @Override
        public void lock() { throw new UnsupportedOperationException(); }

        @Override
        public boolean tryLock(){ throw new UnsupportedOperationException(); }

        @Override
        public Condition newCondition(){ throw new UnsupportedOperationException(); }
        
    }
    
}

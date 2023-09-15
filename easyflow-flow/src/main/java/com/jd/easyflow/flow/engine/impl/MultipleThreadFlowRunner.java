package com.jd.easyflow.flow.engine.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicInteger;

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

    @Override
    public void runNodes(FlowContextImpl context) {
        String runId = startId++ + "";
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("Start running flow node, runId:" + runId);
        }
        CountDownLatch lock = new CountDownLatch(1);
        AtomicInteger counter = new AtomicInteger();
        scheduleNodes(context, counter, lock, runId);
        try {
            lock.await();
        } catch (InterruptedException e) {
            throw new FlowException(e);
        }
        List<NodeContext> exceptions = context.get(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS);
        // Default behavior is throwing first exception.
        if (exceptions != null && exceptions.size() > 0) {
            printStackTrace();
            Throwable t = exceptions.get(0).get(FlowConstants.NODE_CTX_MULTI_EXCEPTION);
            throw ExceptionUtil.throwException(t);
        }
    }

    protected void scheduleNodes(FlowContextImpl context, AtomicInteger counter, CountDownLatch lock,
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
    private void addTaskIfExists(FlowContextImpl context, AtomicInteger counter, CountDownLatch lock,
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
                        lock.countDown();
                        return;
                    }
                    addTaskIfExists(context, counter, lock, runId);
                    int count = counter.addAndGet(-1);
                    if (count == 0) {
                        lock.countDown();
                    }
                } catch (Throwable t) { // NOSONAR
                    addException(context, finalCurrentNode, t);
                    int count = counter.addAndGet(-1);
                    if (count == 0) {
                        lock.countDown();
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
    protected synchronized void addException(FlowContext context, NodeContext nodeContext, Throwable t) {
        List<NodeContext> exceptionNodes = context.get(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS);
        if (exceptionNodes == null) {
            exceptionNodes = new ArrayList<NodeContext>();
            context.put(FlowConstants.FLOW_CTX_MULTI_EXCEPTIONS, exceptionNodes);
        }
        nodeContext.put(FlowConstants.NODE_CTX_MULTI_EXCEPTION, t);
        exceptionNodes.add(nodeContext);
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

}

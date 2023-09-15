package com.jd.easyflow.flow.engine.impl;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.model.NodeContext;

/**
 * Reuse thread first.
 * 
 * @author liyuliang5
 *
 */
public class ReusableThreadFlowRunner extends MultipleThreadFlowRunner {

    private static final Logger logger = LoggerFactory.getLogger(ReusableThreadFlowRunner.class);

    @Override
    protected void scheduleNodes(FlowContextImpl context, AtomicInteger counter, CountDownLatch lock, String runId) {
        List<NodeContext> startNodes = context.getStartNodes();
        if (startNodes.size() == 0) {
            lock.countDown();
            return;
        }
        counter.addAndGet(startNodes.size());
        runNodes(startNodes.toArray(new NodeContext[startNodes.size()]), context, counter, lock, runId);
    }

    private void runNodes(NodeContext[] nodes, FlowContextImpl context, AtomicInteger counter, CountDownLatch lock,
            String runId) {
        while (true) {
            if (nodes == null || nodes.length == 0) {
                return;
            }
            if (nodes.length > 1) {
                for (int i = 1; i < nodes.length; i++) {
                    final NodeContext finalNode = nodes[i];
                    executor.execute(() -> {
                        runNodes(new NodeContext[] { finalNode }, context, counter, lock, runId);
                    });
                }
            }
            NodeContext[] nextNodes = doRunOneNode(nodes[0], context, counter, lock, runId);
            nodes = nextNodes;
        }
    }

    private NodeContext[] doRunOneNode(NodeContext node, FlowContextImpl context, AtomicInteger counter,
            CountDownLatch lock, String runId) {
        NodeContext[] nextNodes = null;
        try {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Start execute flow node:" + node.getNodeId() + ", runId:" + runId);
            }
            nextNodes = super.runOneNode(node, context);
        } catch (Throwable t) { // NOSONAR
            addException(context, node, t);
        }
        if (nextNodes != null && nextNodes.length > 0) {
            counter.addAndGet(nextNodes.length);
        }
        int count = counter.addAndGet(-1);
        if (count == 0) {
            lock.countDown();
        }
        if (context.isInterrupted()) {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("Flow state is interrupted");
            }
            lock.countDown();
            nextNodes = null;
        }
        return nextNodes;
    }
}

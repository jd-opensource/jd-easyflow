package com.jd.easyflow.flow.engine.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.model.NodeContext;

/**
 * Single thread executor.
 * 
 * @author liyuliang5
 * @date 2021/07/25
 */
public class SingleThreadFlowRunner extends BaseFlowRunner {

    public static final Logger logger = LoggerFactory.getLogger(SingleThreadFlowRunner.class);

    /**
     * 
     * DFS.
     * 
     * @param context
     */
    @Override
    public void runNodes(FlowContextImpl context) {
        NodeContext currentNode;
        // Loop execute.
        while ((currentNode = context.getNextNode()) != null) {
            if (context.isInterrupted()) {
                if (context.isLogOn() && logger.isInfoEnabled()) {
                    logger.info("Flow interrupted!");
                }
                break;
            }
            NodeContext[] nextNodes = runOneNode(currentNode, context);
            if (nextNodes != null) {
                context.addNodes(nextNodes);
            }
        }
    }
}

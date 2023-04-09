package com.jd.easyflow.flow.util;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowUtil {

    /**
     * get node
     * @param nodeContext
     * @param context
     * @return
     */
    public static FlowNode node(NodeContext nodeContext, FlowContext context) {
        return context.getFlow().getNode(nodeContext.getNodeId());
    }

    /**
     * get node property
     * @param <T>
     * @param key
     * @param nodeContext
     * @param context
     * @return
     */
    public static <T> T nodeProperty(String key, NodeContext nodeContext, FlowContext context) {
        return (T) node(nodeContext, context).getProperty(key);
    }

}

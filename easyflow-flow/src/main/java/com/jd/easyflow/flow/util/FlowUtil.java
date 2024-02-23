package com.jd.easyflow.flow.util;

import java.util.ArrayList;
import java.util.List;

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
    
    
    /**
     * 
     * @param nodes
     * @return
     */
    public static String[] nodeIds(FlowNode[] nodes) {
        if (nodes == null) {
            return null;
        }
        String[] nodeIds = new String[nodes.length];
        for (int i = 0; i < nodes.length; i++) {
            nodeIds[i] = nodes[i].getId();
        }
        return nodeIds;
    }
    
    /**
     * 
     * @param nodes
     * @return
     */
    public static List<String> nodeIdsOfNodeList(List<FlowNode> nodes) {
        if (nodes == null) {
            return null;
        }
        List<String> nodeIds = new ArrayList<>(nodes.size());
        for (int i = 0; i < nodes.size(); i++) {
            nodeIds.add(nodes.get(i).getId());
        }
        return nodeIds;
    }
    
    /**
     * 
     * @param nodes
     * @return
     */
    public static String[] nodeIds(NodeContext[] nodes) {
        if (nodes == null) {
            return null;
        }
        String[] nodeIds = new String[nodes.length];
        for (int i = 0; i < nodes.length; i++) {
            nodeIds[i] = nodes[i].getNodeId();
        }
        return nodeIds;
    }
    
    /**
     * 
     * @param nodes
     * @return
     */
    public static List<String> nodeIdsOfNodeContextList(List<NodeContext> nodes) {
        if (nodes == null) {
            return null;
        }
        List<String> nodeIds = new ArrayList<>(nodes.size());
        for (int i = 0; i < nodes.size(); i++) {
            nodeIds.add(nodes.get(i).getNodeId());
        }
        return nodeIds;
    }
    
    public static String nsKey(String key) {
        return FlowConstants.NS_SEP + key;
    }
    
    public static String nsKey(String ns, String key) {
        return ns + FlowConstants.NS_SEP + key;
    }

}

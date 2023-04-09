package com.jd.easyflow.flow.engine.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.event.FlowEventTrigger;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Flow Context.
 * 
 * @author liyuliang5
 *
 */
public class FlowContextImpl implements FlowContext {
    /**
     * Flow id.
     */
    private String flowId;
    /**
     * Flow definition.
     */
    private Flow flow;
    /**
     * Flow param.
     */
    private FlowParam param;
    /**
     * Flow result.
     */
    private FlowResult result;
    /**
     * Event trigger.
     */
    private FlowEventTrigger eventTrigger;
    /**
     * Start nodes.
     */
    private List<NodeContext> startNodes;
    /**
     * End nodes.
     */
    private List<NodeContext> endNodes;
    /**
     * Node context list waiting execute.
     */
    private Stack<NodeContext> nodeStack = new Stack<NodeContext>();

    /**
     * Common data map.
     */
    private Map<String, Object> data = new ConcurrentHashMap<String, Object>();
    /**
     * Flow engine.
     */
    private FlowEngine flowEngine;

    /**
     * interrupted
     */
    private volatile boolean interrupted = false;

    @Override
    public void put(String key, Object value) {
        if (value == null) {
            data.remove(value);
        } else {
            data.put(key, value);
        }
    }

    @Override
    public <T> T get(String key) {
        return (T) data.get(key);
    }

    @Override
    public void remove(String key) {
        data.remove(key);
    }

    public synchronized void addNodes(NodeContext[] nodes) {
        for (int i = 0; i < nodes.length; i++) {
            nodeStack.add(nodes[nodes.length - 1 - i]);
        }
    }

    /**
     * Get next node.
     */
    public synchronized NodeContext getNextNode() {
        if (nodeStack == null) {
            return null;
        }
        if (nodeStack.isEmpty()) {
            return null;
        }
        return nodeStack.pop();
    }

    @Override
    public FlowParam getParam() {
        return param;
    }

    @Override
    public void setParam(FlowParam param) {
        this.param = param;
    }

    @Override
    public FlowResult getResult() {
        return result;
    }

    @Override
    public void setResult(FlowResult result) {
        this.result = result;
    }

    @Override
    public String getFlowId() {
        return flowId;
    }

    @Override
    public void setFlowId(String flowId) {
        this.flowId = flowId;
    }

    @Override
    public Flow getFlow() {
        return flow;
    }

    @Override
    public void setFlow(Flow flow) {
        this.flow = flow;
    }

    public FlowEventTrigger getEventTrigger() {
        return eventTrigger;
    }

    public void setEventTrigger(FlowEventTrigger eventTrigger) {
        this.eventTrigger = eventTrigger;
    }

    @Override
    public Map<String, Object> getData() {
        return data;
    }

    @Override
    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    @Override
    public FlowEngine getFlowEngine() {
        return flowEngine;
    }

    public void setFlowEngine(FlowEngine flowEngine) {
        this.flowEngine = flowEngine;
    }

    @Override
    public List<NodeContext> getStartNodes() {
        return startNodes;
    }

    @Override
    public void setStartNodes(List<NodeContext> startNodes) {
        this.startNodes = startNodes;
    }

    @Override
    public List<NodeContext> getEndNodes() {
        return endNodes;
    }

    public void setEndNodes(List<NodeContext> endNodes) {
        this.endNodes = endNodes;
    }

    public synchronized void addEndNode(NodeContext node) {
        if (endNodes == null) {
            endNodes = new ArrayList();
        }
        endNodes.add(node);
    }

    public boolean isInterrupted() {
        return interrupted;
    }

    public void setInterrupted() {
        this.interrupted = true;
    }

}

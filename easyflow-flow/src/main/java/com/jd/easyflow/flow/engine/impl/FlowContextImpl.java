package com.jd.easyflow.flow.engine.impl;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.el.ElEvaluator;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;

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
    private Deque<NodeContext> nodeStack = new ArrayDeque<NodeContext>();

    /**
     * Common data map.
     */
    private Map<String, Object> data = new ConcurrentHashMap<String, Object>();

    /**
     * business context
     */
    private Object context;
    
    /**
     * Flow engine.
     */
    private FlowEngine flowEngine;

    /**
     * interrupted
     */
    private volatile boolean interrupted = false;
    
    private Boolean preResult;
    
    private Boolean logFlag;
    
    private ElEvaluator elEvaluator;
    
    private Boolean recordHistory;
    
    private FlowContext parentContext;
    
    private NodeContext parentNodeContext;

    @Override
    public void put(String key, Object value) {
        if (value == null) {
            data.remove(key);
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
            nodeStack.push(nodes[nodes.length - 1 - i]);
        }
    }

    /**
     * Get next node.
     */
    public synchronized NodeContext getNextNode() {
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

    public Boolean getPreResult() {
        return preResult;
    }

    public void setPreResult(Boolean preResult) {
        this.preResult = preResult;
    }

    public Boolean getLogFlag() {
        return logFlag;
    }

    public void setLogFlag(Boolean logFlag) {
        this.logFlag = logFlag;
    }
    
    public boolean isLogOn() {
        return logFlag == null || this.logFlag.booleanValue();
    }

    public <T>T getContext() {
        return (T) context;
    }

    public void setContext(Object context) {
        this.context = context;
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }

    @Override
    public boolean isRecordHistory() {
        if (recordHistory == null) {
            recordHistory = ! Boolean.FALSE.equals(flow.getProperty(FlowConstants.FLOW_PROPERTY_RECORD_HISTORY));
        }
        return recordHistory;
    }

    public Boolean getRecordHistory() {
        return recordHistory;
    }

    public void setRecordHistory(Boolean recordHistory) {
        this.recordHistory = recordHistory;
    }

    @Override
    public FlowContext getParentContext() {
        return parentContext;
    }

    public void setParentContext(FlowContext parentContext) {
        this.parentContext = parentContext;
    }

    @Override
    public NodeContext getParentNodeContext() {
        return parentNodeContext;
    }

    public void setParentNodeContext(NodeContext parentNodeContext) {
        this.parentNodeContext = parentNodeContext;
    }
    
}

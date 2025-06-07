package com.jd.easyflow.flow.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.engine.event.FlowEventTrigger;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.filter.FlowFilterManager;
import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * 
 * @author liyuliang5
 *
 */
public class Flow implements FlowLifeCycle {

    public static final String DOLLAR = "$";

    private String id;

    private String name;

    private String[] startNodeIds;

    private List<FlowNode> nodeList;

    private Map<String, Object> properties = new ConcurrentHashMap<>();

    private Map<String, FlowNode> nodeMap;
    /**
     * relation of node id and index.
     */
    private Map<String, Integer> nodeIndexMap;
    
    private FlowPreHandler preHandler;
    
    private FlowPostHandler postHandler;

    private FlowEventTrigger eventTrigger = new FlowEventTrigger();
    
    private FlowFilterManager filterManager = new FlowFilterManager();

    private FlowRunner runner;

    @JsonIgnore
    private FlowParser flowParser;
    
    private Boolean logFlag;

    @Override
    public void init(InitContext initContext, Object parent) {
        if (preHandler != null) {
            preHandler.init(initContext, this);
        }
        if (nodeList != null) {
            for (FlowNode flowNode : nodeList) {
                flowNode.init(initContext, this);
            }
        }
        if (postHandler != null) {
            postHandler.init(initContext, this);
        }
        
        eventTrigger.init(initContext, this);
        filterManager.init(initContext, this);
        if (runner != null) {
            runner.init(initContext, this);
        }
        
    }
    
    @Override
    public void destroy() {
        if (preHandler != null) {
            preHandler.destroy();
        }
        if (nodeList != null) {
            for (FlowNode flowNode : nodeList) {
                flowNode.destroy();
            }
        }
        if (postHandler != null) {
            postHandler.destroy();
        }
        
        eventTrigger.destroy();
        filterManager.destry();
        if (runner != null) {
            runner.destroy();
        }
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public List<FlowNode> getNodeList() {
        return nodeList;
    }

    public void initNodeList(List<FlowNode> nodeList) {
        for (FlowNode node : nodeList) {
            addNode(node);
        }
    }

    public Map<String, Object> getProperties() {
        return properties;
    }
    
    public void setProperties(Map<String, Object> properties) {
        this.properties.clear();
        putProperties(properties);
    }

    public void putProperties(Map<String, Object> properties) {
        if (properties == null) {
            return;
        }
        for (Entry<String, Object>  entry : properties.entrySet()) {
            if (entry.getValue() == null) {
                this.properties.remove(entry.getKey());
            } else {
                this.properties.put(entry.getKey(), entry.getValue());
            }
        }
    }

    public Map<String, FlowNode> getNodeMap() {
        return nodeMap;
    }

    public void setProperty(String key, Object value) {
        if (value == null) {
            properties.remove(key);
        } else {
            properties.put(key, value);
        }
    }
    

    public <T> T getProperty(String key) {
        return (T) properties.get(key);
    }

    public void addNode(FlowNode node) {
        if (node.getId().startsWith(DOLLAR)) {
            throw new IllegalArgumentException("Node ID CANNOT start with $");
        }
        if (this.nodeList == null) {
            this.nodeList = new ArrayList<>();
        }
        if (this.nodeMap == null) {
            this.nodeMap = new HashMap<>();
        }
        if (this.nodeMap.containsKey(node.getId())) {
            throw new FlowException("Node:" + node.getId() + " duplicate");
        }
        if (this.nodeIndexMap == null) {
            this.nodeIndexMap = new HashMap<>();
        }
        this.nodeList.add(node);
        this.nodeMap.put(node.getId(), node);
        this.nodeIndexMap.put(node.getId(), this.nodeList.size() - 1);
    }
    
    public void setNodeList(List<FlowNode> nodeList) {
        if (nodeList == null) {
            this.nodeList = null;
            this.nodeMap = null;
            this.nodeIndexMap = null;
        } else {
            this.nodeList = nodeList;
            this.nodeMap = new HashMap<>();
            this.nodeIndexMap = new HashMap<>();
            for (FlowNode node : nodeList) {
                this.nodeMap.put(node.getId(), node);
                this.nodeIndexMap.put(node.getId(), this.nodeList.size() - 1);
            }
        }
    }

    public FlowNode getNode(String nodeId) {
        return this.nodeMap.get(nodeId);
    }

    public int getNodeIndex(String nodeId) {
        return this.nodeIndexMap.get(nodeId);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public FlowEventTrigger getEventTrigger() {
        return eventTrigger;
    }

    public void setEventTrigger(FlowEventTrigger eventTrigger) {
        this.eventTrigger = eventTrigger;
    }

    public void triggerEvent(String eventType, FlowContext context) {
        this.eventTrigger.triggerEvent(eventType, context);
    }

    public void triggerEvent(String eventType, Object eventData, FlowContext context, boolean catchThrowable) {
        this.eventTrigger.triggerEvent(eventType, eventData, context, catchThrowable);
    }

    public String[] getStartNodeIds() {
        return startNodeIds;
    }

    public void setStartNodeIds(String[] startNodeIds) {
        this.startNodeIds = startNodeIds;
    }
    
    public FlowRunner getRunner() {
        return runner;
    }

    public void setRunner(FlowRunner runner) {
        this.runner = runner;
    }

    public String stringify() {
        if (flowParser == null) {
            return null;
        }
        return flowParser.stringify(this);
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }


    public FlowPreHandler getPreHandler() {
        return preHandler;
    }

    public void setPreHandler(FlowPreHandler preHandler) {
        this.preHandler = preHandler;
    }

    public FlowPostHandler getPostHandler() {
        return postHandler;
    }

    public void setPostHandler(FlowPostHandler postHandler) {
        this.postHandler = postHandler;
    }

    public Boolean getLogFlag() {
        return logFlag;
    }

    public void setLogFlag(Boolean logFlag) {
        this.logFlag = logFlag;
    }

    public FlowFilterManager getFilterManager() {
        return filterManager;
    }

    public void setFilterManager(FlowFilterManager filterManager) {
        this.filterManager = filterManager;
    }
    
}

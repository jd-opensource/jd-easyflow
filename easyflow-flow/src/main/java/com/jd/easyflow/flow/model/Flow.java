package com.jd.easyflow.flow.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.engine.event.FlowEventTrigger;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.parser.FlowParser;

/**
 * 
 * @author liyuliang5
 *
 */
public class Flow {

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

    private FlowRunner runner;

    private List<Filter<FlowContext, FlowResult>> filters;

    private List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> nodeFilters;

    /**
     * nodeAction is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, Object>> nodeActionFilters;
    /**
     * nodePreHandler is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, Boolean>> nodePreHandlerFilters;
    /**
     * nodePostHandler is not standard model. define here for performance.
     */
    private List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> nodePostHandlerFilters;

    private FlowParser flowParser;
    
    private Boolean logFlag;

    public void init(InitContext initContext) {
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
        this.properties = properties;
    }

    public void putProperties(Map<String, Object> properties) {
        if (properties != null) {
            this.properties.putAll(properties);
        }
    }

    public Map<String, FlowNode> getNodeMap() {
        return nodeMap;
    }

    public void setNodeMap(Map<String, FlowNode> nodeMap) {
        this.nodeMap = nodeMap;
    }

    public void setProperty(String key, Object value) {
        properties.put(key, value);
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

    public List<Filter<FlowContext, FlowResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<FlowContext, FlowResult>> filters) {
        this.filters = filters;
    }

    public void addFilter(Filter<FlowContext, FlowResult> filter) {
        if (this.filters == null) {
            this.filters = new ArrayList<Filter<FlowContext, FlowResult>>();
        }
        this.filters.add(filter);
    }

    public List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> getNodeFilters() {
        return nodeFilters;
    }

    public void setNodeFilters(List<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>> nodeFilters) {
        this.nodeFilters = nodeFilters;
    }

    public void addNodeFilter(Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> filter) {
        if (this.nodeFilters == null) {
            this.nodeFilters = new ArrayList<Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext>>();
        }
        this.nodeFilters.add(filter);
    }

    public List<Filter<Pair<NodeContext, FlowContext>, Object>> getNodeActionFilters() {
        return nodeActionFilters;
    }

    public void addNodeActionFilter(Filter<Pair<NodeContext, FlowContext>, Object> filter) {
        if (this.nodeActionFilters == null) {
            this.nodeActionFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Object>>();
        }
        this.nodeActionFilters.add(filter);
    }

    public void addNodePreHandlerFilter(Filter<Pair<NodeContext, FlowContext>, Boolean> filter) {
        if (this.nodePreHandlerFilters == null) {
            this.nodePreHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, Boolean>>();
        }
        this.nodePreHandlerFilters.add(filter);
    }

    public void addNodePostHandlerFilter(Filter<Pair<NodeContext, FlowContext>, NodeContext[]> filter) {
        if (this.nodePostHandlerFilters == null) {
            this.nodePostHandlerFilters = new ArrayList<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>>();
        }
        this.nodePostHandlerFilters.add(filter);
    }

    public FlowRunner getRunner() {
        return runner;
    }

    public void setRunner(FlowRunner runner) {
        this.runner = runner;
    }

    public String stringify() {
        return flowParser.stringify(this);
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

    public void setNodeList(List<FlowNode> nodeList) {
        this.nodeList = nodeList;
    }

    public void setNodeActionFilters(List<Filter<Pair<NodeContext, FlowContext>, Object>> nodeActionFilters) {
        this.nodeActionFilters = nodeActionFilters;
    }

    public List<Filter<Pair<NodeContext, FlowContext>, Boolean>> getNodePreHandlerFilters() {
        return nodePreHandlerFilters;
    }

    public void setNodePreHandlerFilters(List<Filter<Pair<NodeContext, FlowContext>, Boolean>> nodePreHandlerFilters) {
        this.nodePreHandlerFilters = nodePreHandlerFilters;
    }

    public List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> getNodePostHandlerFilters() {
        return nodePostHandlerFilters;
    }

    public void setNodePostHandlerFilters(
            List<Filter<Pair<NodeContext, FlowContext>, NodeContext[]>> nodePostHandlerFilters) {
        this.nodePostHandlerFilters = nodePostHandlerFilters;
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


}

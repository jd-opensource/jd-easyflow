package com.jd.easyflow.flow.engine.impl;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.FlowRunner;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.engine.event.FlowEventTrigger;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.parser.FlowParser;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.JsonUtil;
import com.jd.easyflow.flow.util.SpelHelper;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowEngineImpl implements FlowEngine, SmartLifecycle {

    public static final Logger logger = LoggerFactory.getLogger(FlowEngineImpl.class);

    protected Map<String, Flow> flowMap = new ConcurrentHashMap<>();

    protected Map<String, String> flowDefinitionMap = new ConcurrentHashMap<String, String>();

    private FlowEventTrigger eventTrigger = new FlowEventTrigger();

    private List<FlowEventListener> listeners;

    private List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> filters;

    private String flowPath;

    private volatile boolean inited;

    @Autowired
    private ApplicationContext applicationContext;

    private FlowRunner defaultFlowRunner = new SingleThreadFlowRunner();

    /**
     * Default is json definition parser.
     */
    private FlowParser flowParser = new FlowParserImpl();

    private  int phase = Integer.MIN_VALUE;

    private volatile boolean isRunning = false;

    public void init() {
        if (inited) {
            return;
        }
        if (applicationContext != null) {
            SpelHelper.setApplicationContext(applicationContext);
        }
        loadFlow();
        if (listeners != null) {
            listeners.forEach(listener -> eventTrigger.addListener(listener));
        }
        inited = true;
    }

    protected void loadFlow() {
        if (flowPath == null) {
            return;
        }
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        String[] flowPaths = flowPath.split(",");
        for (String path : flowPaths) {
            try {
                resources = resolver.getResources(path.trim());
                for (Resource resource : resources) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Start parsing definition files:" + resource.getURI());
                    }
                    try (InputStream is = resource.getInputStream()) {
                        String flowDefinition = IOUtils.toString(is);
                        List<Flow> flowList = flowParser.parse(flowDefinition);
                        flowDefinitionMap.put(flowList.get(0).getId(), flowDefinition);
                        flowList.forEach(flow -> {
                            if (flowMap.containsKey(flow.getId())) {
                                throw new FlowException("Flow " + flow.getId() + " exists");
                            }
                            flowMap.put(flow.getId(), flow);
                        });
                    }
                }
            } catch (IOException e) {
                throw new RuntimeException("Flow definition file parse exception", e);
            }
        }
    }

    /**
     * Start flow engine, exeucte flow.
     */
    @Override
    public FlowResult execute(FlowParam param) {
        if (! inited) {
            throw new FlowException("Flow engine is not inited. flowId:" + param.getFlowId());
        }
        if (logger.isInfoEnabled()) {
            logger.info("START EXECUTE FLOW, flowId:" + param.getFlowId() + " nodeIds:"
                    + Arrays.toString(param.getNodeIds()));
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Flow param:" + JsonUtil.toJsonString(param));
        }
        if (filters == null || filters.size() == 0) {
            return invokeFlowEngine(param);
        } else {
            FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain = new FilterChain<Pair<FlowParam, FlowEngine>, FlowResult>(filters,
                    p -> invokeFlowEngine(p.getLeft()));
            return chain.doFilter(Pair.of(param, this));
        }
    }

    protected FlowResult invokeFlowEngine(FlowParam param) {
        Map<String, Object> data = new HashMap<>();
        data.put("param", param);
        data.put("flowEngine", this);
        eventTrigger.triggerEvent(FlowEventTypes.FLOW_ENGINE_START, data, null, false);
        try {
            FlowResult result = executeFlow(param);
            eventTrigger.triggerEvent(FlowEventTypes.FLOW_ENGINE_END, data, null, false);
            return result;
        } finally {
            eventTrigger.triggerEvent(FlowEventTypes.FLOW_ENGINE_COMPLETE, data, null, true);
        }
    }

    protected FlowResult executeFlow(FlowParam param) {
        // init flow context
        FlowContext context = initContext(param);
        // find flow definition
        Flow flow = findFlow(context);
        if (logger.isInfoEnabled()) {
            logger.info("EXECUTE FLOW, flowId:" + flow.getId());
        }
        if (flow.getFilters() == null || flow.getFilters().size() == 0) {
            return invokeFlow(context);
        } else {
            FilterChain<FlowContext, FlowResult> chain = new FilterChain<FlowContext, FlowResult>(flow.getFilters(),
                    p -> invokeFlow(p));
            return chain.doFilter(context);
        }
    }

    protected FlowResult invokeFlow(FlowContext context) {
        Flow flow = context.getFlow();
        Throwable throwable = null;
        try {
            flow.triggerEvent(FlowEventTypes.FLOW_START, context);
            init(context);
            run(context);
            flow.triggerEvent(FlowEventTypes.FLOW_END, context);
            return wrapResult(context);
        } catch (Throwable t) { // NOSONAR
            throwable = t;
            logger.error(t.getMessage(), t);
            throw t;
        } finally {
            flow.triggerEvent(FlowEventTypes.FLOW_COMPLETE, throwable, context, true);
        }
    }

    protected FlowContext initContext(FlowParam param) {
        FlowContext context = param.getContext() != null ? param.getContext() : new FlowContextImpl();
        if (context.getParam() == null) {
            context.setParam(param);
        }
        if (context.getResult() == null) {
            FlowResult result = new FlowResult();
            context.setResult(result);
            result.setContext(context);
        }
        if (context.getFlowId() == null) {
            context.setFlowId(param.getFlowId());
        }
        context.setFlowEngine(this);
        return context;
    }

    protected void run(FlowContext context) {
        FlowRunner flowRunner = context.getFlow().getRunner();
        if (flowRunner == null) {
            flowRunner = defaultFlowRunner;
        }
        flowRunner.run(context);
    }

    /**
     * 
     * Find flow definition.
     *
     * @return
     */
    protected Flow findFlow(FlowContext context) {
        Flow flow = context.getFlow();
        if (flow == null) {
            flow = getFlow(context.getFlowId());
            context.setFlow(flow);
        }
        // Exists scenario changing flow id
        context.setFlowId(flow.getId());
        return flow;
    }

    @Override
    public Flow getFlow(String flowId) {
        return flowMap.get(flowId);
    }

    /**
     * 
     * Init flow context.
     *
     * @param param
     */
    protected void init(FlowContext context) {
        context.getFlow().triggerEvent(FlowEventTypes.INIT_START, context);
        String[] nodeIds = context.getParam().getNodeIds();
        // If nodeIds is null, using startNodeIds；if is empty array，run empty flow
        // instance.
        if (nodeIds == null) {
            nodeIds = context.getFlow().getStartNodeIds();
        }
        NodeContext[] nodes = new NodeContext[nodeIds.length];
        for (int i = 0; i < nodeIds.length; i++) {
            nodes[i] = new NodeContext(nodeIds[i]);
        }
        context.addNodes(nodes);
        context.setStartNodes(Arrays.asList(nodes));
        context.getFlow().triggerEvent(FlowEventTypes.INIT_END, context);
    }

    public void addFlow(Flow flow) {
        flowMap.put(flow.getId(), flow);
    }

    private FlowResult wrapResult(FlowContext context) {
        FlowResult result = context.getResult();
        return result;
    }

    public String getFlowPath() {
        return flowPath;
    }

    public void setFlowPath(String flowPath) {
        this.flowPath = flowPath;
    }


    public Map<String, Flow> getFlowMap() {
        return flowMap;
    }

    public void setFlowMap(Map<String, Flow> flowMap) {
        this.flowMap = flowMap;
    }

    public Map<String, String> getFlowDefinitionMap() {
        return flowDefinitionMap;
    }

    public void setFlowDefinitionMap(Map<String, String> flowDefinitionMap) {
        this.flowDefinitionMap = flowDefinitionMap;
    }

    public List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<Pair<FlowParam, FlowEngine>, FlowResult>> filters) {
        this.filters = filters;
    }

    public FlowEventTrigger getEventTrigger() {
        return eventTrigger;
    }

    public void setEventTrigger(FlowEventTrigger eventTrigger) {
        this.eventTrigger = eventTrigger;
    }

    public List<FlowEventListener> getListeners() {
        return listeners;
    }

    public void setListeners(List<FlowEventListener> listeners) {
        this.listeners = listeners;
    }

    public FlowRunner getDefaultFlowRunner() {
        return defaultFlowRunner;
    }

    public void setDefaultFlowRunner(FlowRunner defaultFlowRunner) {
        this.defaultFlowRunner = defaultFlowRunner;
    }

    @Override
    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

    public boolean isInited() {
        return inited;
    }

    public void setInited(boolean inited) {
        this.inited = inited;
    }

    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }


    @Override
    public void start() {
        init();
        isRunning = true;
    }

    @Override
    public void stop() {
        isRunning = false;
    }

    @Override
    public boolean isRunning() {
        return isRunning;
    }
    @Override
    public int getPhase() {
        return phase;
    }

    public void setPhase(int phase) {
        this.phase = phase;
    }
}

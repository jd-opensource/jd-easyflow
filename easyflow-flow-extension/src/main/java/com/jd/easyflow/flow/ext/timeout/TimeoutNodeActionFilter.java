package com.jd.easyflow.flow.ext.timeout;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowUtil;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 */
public class TimeoutNodeActionFilter implements Filter<Pair<NodeContext, FlowContext>, Object> {

    private static final Logger logger = LoggerFactory.getLogger(TimeoutNodeActionFilter.class);

    protected ExecutorService defaultExecutorService;
    
    protected Map<String, ExecutorService> executorServiceMap = new ConcurrentHashMap<String, ExecutorService>();
    

    private TimeoutTemplate timeoutTemplate = new TimeoutTemplate();

    public TimeoutNodeActionFilter() {

    }

    public TimeoutNodeActionFilter(ExecutorService defaultExecutorService) {
        this.defaultExecutorService = defaultExecutorService;
    }

    @Override
    public void init(InitContext initContext, Object parent) {
        if (this.defaultExecutorService == null) {
            this.defaultExecutorService = Executors.newCachedThreadPool();
        }
    }

    @Override
    public Object doFilter(Pair<NodeContext, FlowContext> request,
            FilterChain<Pair<NodeContext, FlowContext>, Object> chain) {
        NodeContext nodeContext = request.getLeft();
        FlowContext context = request.getRight();
        Map<String, Object> timeoutConfig = FlowUtil.nodeProperty("timeout", nodeContext, context);
        boolean enableTimeout = timeoutConfig != null && !Boolean.FALSE.equals(timeoutConfig.get("enable"));
        if (!enableTimeout) {
            // no timeout configuration.
            return chain.doFilter(request);
        } else {
            // has timeout configuration.
            String executorServiceKey = (String) timeoutConfig.get("executorServiceKey");
            Integer timeoutMillis = null;
            String timeoutMillisExp = (String) timeoutConfig.get("timeoutMillisExp");
            if (timeoutMillisExp != null) {
                timeoutMillis = (int) context.getElEvaluator().eval(timeoutMillisExp, nodeContext, context, timeoutConfig);
            }
            if (timeoutMillis == null) {
                timeoutMillis = (Integer) timeoutConfig.get("timeoutMillis");
            }
            if (timeoutMillis == null) {
                throw new FlowException("timeoutMills can not be null, check config");
            }
            boolean interruptOnTimeout = ! Boolean.FALSE.equals(timeoutConfig.get("interruptOnTimeout"));
            ExecutorService executorService = getExecutorService(executorServiceKey, nodeContext, context);
            Object actionResult = timeoutTemplate.execute(() -> {
                // normal method.
                return chain.doFilter(request);
            }, () -> {
                // timeout method.
                String onTimeoutExp = (String) timeoutConfig.get("onTimeoutExp");
                Map<String, Object> timeoutContext = new HashMap<String, Object>();
                timeoutContext.put("timeoutConfig", timeoutConfig);
                return context.getElEvaluator().eval(onTimeoutExp, nodeContext, context, timeoutContext);
            }, timeoutMillis, executorService, context.isLogOn(), interruptOnTimeout);

            return actionResult;
        }

    }

    public ExecutorService getExecutorService(String executorServiceKey, NodeContext nodeContext, FlowContext flowContext) {
        if (executorServiceKey == null) {
            return defaultExecutorService;
        }
        ExecutorService executorService = executorServiceMap.computeIfAbsent(executorServiceKey, key -> {
            return Executors.newCachedThreadPool();
        });
        return executorService;
    }

    public ExecutorService getDefaultExecutorService() {
        return defaultExecutorService;
    }

    public void setDefaultExecutorService(ExecutorService defaultExecutorService) {
        this.defaultExecutorService = defaultExecutorService;
    }

    public Map<String, ExecutorService> getExecutorServiceMap() {
        return executorServiceMap;
    }

    public void setExecutorServiceMap(Map<String, ExecutorService> executorServiceMap) {
        this.executorServiceMap = executorServiceMap;
    }

    public TimeoutTemplate getTimeoutTemplate() {
        return timeoutTemplate;
    }

    public void setTimeoutTemplate(TimeoutTemplate timeoutTemplate) {
        this.timeoutTemplate = timeoutTemplate;
    }
    
    

}

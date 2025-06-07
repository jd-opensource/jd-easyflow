package com.jd.easyflow.flow.ext.timeout;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.filter.BaseFilter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
public class TimeoutFlowFilter extends BaseFilter<FlowContext, FlowResult> {
    

    private static final Logger logger = LoggerFactory.getLogger(TimeoutFlowFilter.class);

    protected ExecutorService defaultExecutorService;
    
    protected Map<String, ExecutorService> executorServiceMap = new ConcurrentHashMap<String, ExecutorService>();

    private TimeoutTemplate timeoutTemplate = new TimeoutTemplate();

    public TimeoutFlowFilter() {

    }

    public TimeoutFlowFilter(ExecutorService defaultExecutorService) {
        this.defaultExecutorService = defaultExecutorService;
    }

    @Override
    public void init(InitContext initContext, Object parent) {
        if (this.defaultExecutorService == null) {
            this.defaultExecutorService = Executors.newCachedThreadPool();
        }
    }

    @Override
    public FlowResult doFilter(FlowContext context,
            FilterChain<FlowContext, FlowResult> chain) {
        Map<String, Object> timeoutConfig = context.getFlow().getProperty("timeout");
        boolean enableTimeout = timeoutConfig != null && !Boolean.FALSE.equals(timeoutConfig.get("enable"));
        if (!enableTimeout) {
            // no timeout configuration.
            return chain.doFilter(context);
        } else {
            // has timeout configuration.
            String executorServiceKey = (String) timeoutConfig.get("executorServiceKey");
            Integer timeoutMillis = null;
            String timeoutMillisExp = (String) timeoutConfig.get("timeoutMillisExp");
            if (timeoutMillisExp != null) {
                timeoutMillis = (int) context.getElEvaluator().eval(timeoutMillisExp, null, context, timeoutConfig);
            }
            if (timeoutMillis == null) {
                timeoutMillis = (Integer) timeoutConfig.get("timeoutMillis");
            }
            if (timeoutMillis == null) {
                throw new FlowException("timeoutMills can not be null, check config");
            }
            boolean interruptOnTimeout = ! Boolean.FALSE.equals(timeoutConfig.get("interruptOnTimeout"));
            ExecutorService executorService = getExecutorService(executorServiceKey, null, context);
            FlowResult result = timeoutTemplate.execute(() -> {
                // normal method.
                return chain.doFilter(context);
            }, () -> {
                // timeout method.
                String onTimeoutExp = (String) timeoutConfig.get("onTimeoutExp");
                Map<String, Object> timeoutContext = new HashMap<String, Object>();
                timeoutContext.put("timeoutConfig", timeoutConfig);
                Object evalResult = context.getElEvaluator().eval(onTimeoutExp, null, context, timeoutContext);
                if (! (evalResult instanceof FlowResult)) {
                    throw new FlowException("eval result should be FlowResult type");
                }
                return (FlowResult) evalResult;
            }, timeoutMillis, executorService, context.isLogOn(), interruptOnTimeout);

            return result;
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

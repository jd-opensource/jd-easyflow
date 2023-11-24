package com.jd.easyflow.fsm;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.event.FsmEventTrigger;
import com.jd.easyflow.fsm.exception.FsmException;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.fsm.util.FsmEventTypes;
import com.jd.easyflow.fsm.util.FsmIOUtil;
import com.jd.easyflow.fsm.util.SpelHelper;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmManager implements SmartLifecycle {

    public static final Logger logger = LoggerFactory.getLogger(FsmManager.class);

    protected Map<String, Fsm> fsmMap = new ConcurrentHashMap<>();

    protected Map<String, String> fsmDefinitionMap = new ConcurrentHashMap<String, String>();

    private String fsmPath;

    private FsmEventTrigger eventTrigger = new FsmEventTrigger();

    private List<FsmEventListener> listeners;

    @Autowired
    private ApplicationContext applicationContext;

    private volatile boolean inited = false;

    private List<Filter<Pair<FsmParam, FsmManager>, FsmResult>> filters;

    private  int phase = Integer.MIN_VALUE;
    
    private boolean autoStartup = true;

    private volatile boolean isRunning = false;

    public void init() {
        if (inited) {
            return;
        }
        if (applicationContext != null) {
            SpelHelper.setApplicationContext(applicationContext);
        }
        loadFsm();
        if (listeners != null) {
            listeners.forEach(listener -> eventTrigger.addListener(listener));
        }
        eventTrigger.init(null, null);
        if (filters != null) {
            filters.forEach(filter -> {
                filter.init(null, null);
            });
        }
        inited = true;
    }
    
    public void destroy () {
        if (fsmMap != null) {
            for (Entry<String, Fsm> entry : fsmMap.entrySet()) {
                entry.getValue().destroy();
            }
        }
        eventTrigger.destroy();
        if (filters != null) {
            filters.forEach(filter -> {
                filter.destroy();
            });
        }
    }

    /**
     * 
     * Loan fsm.
     *
     */
    protected void loadFsm() {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        try {
            resources = resolver.getResources(fsmPath);
            for (Resource resource : resources) {
                logger.info("Start parse fsm definition file:" + resource.getURI());
                try (InputStream is = resource.getInputStream()) {
                    String fsmDefinition = FsmIOUtil.toString(is);
                    Fsm fsm = FsmParser.parse(fsmDefinition);
                    if (fsmDefinitionMap.containsKey(fsm.getId())) {
                        throw new FsmException();
                    }
                    fsmDefinitionMap.put(fsm.getId(), fsmDefinition);
                    fsmMap.put(fsm.getId(), fsm);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Fsm definition file load exception", e);
        }
    }

    public void add(Fsm fsm) {
        if (!fsmMap.containsKey(fsm.getId())) {
            fsmMap.put(fsm.getId(), fsm);
        }
    }

    public Fsm getFsm(String id) {
        return fsmMap.get(id);
    }

    public FsmResult run(FsmParam param) {
        if (! inited) {
            throw new FsmException("Fsm is not inited. fsmId:" + param.getFsmId());
        }
        if (logger.isInfoEnabled()) {
            logger.info("FSM MANAGER RUN. fsmId: " + param.getFsmId() + " event:" + param.getEventId() + " currentStateId:"
                    + param.getCurrentStateId() + " opType:" + param.getOpType());
        }
        if (filters == null || filters.size() == 0) {
            return invokeFsm(param);
        } else {
            FilterChain<Pair<FsmParam, FsmManager>, FsmResult> chain = new FilterChain<Pair<FsmParam, FsmManager>, FsmResult>(filters, p -> invokeFsm(p.getLeft()));
            return chain.doFilter(Pair.of(param, this));
        }
    }

    protected FsmResult invokeFsm(FsmParam param) {
        Map<String, Object> data = new HashMap<>();
        data.put("param", param);
        data.put("fsmManager", this);
        eventTrigger.triggerEvent(FsmEventTypes.FSM_MANAGER_START, data, null, false);
        try {
            Fsm fsm = getFsm(param.getFsmId());
            if (fsm == null) {
                throw new RuntimeException("FSM:" + param.getFsmId() + " not exists");
            }
            FsmResult result = fsm.run(param);
            data.put("result", result);
            eventTrigger.triggerEvent(FsmEventTypes.FSM_MANAGER_END, data, null, false);
            return result;
        } finally {
            eventTrigger.triggerEvent(FsmEventTypes.FSM_MANAGER_COMPLETE, data, null, true);
        }
    }

    public String getFsmPath() {
        return fsmPath;
    }

    public void setFsmPath(String fsmPath) {
        this.fsmPath = fsmPath;
    }

    public List<FsmEventListener> getListeners() {
        return listeners;
    }

    public void setListeners(List<FsmEventListener> listeners) {
        this.listeners = listeners;
    }

    public Map<String, Fsm> getFsmMap() {
        return fsmMap;
    }

    public void setFsmMap(Map<String, Fsm> fsmMap) {
        this.fsmMap = fsmMap;
    }

    public FsmEventTrigger getEventTrigger() {
        return eventTrigger;
    }

    public void setEventTrigger(FsmEventTrigger eventTrigger) {
        this.eventTrigger = eventTrigger;
    }

    public Map<String, String> getFsmDefinitionMap() {
        return fsmDefinitionMap;
    }

    public void setFsmDefinitionMap(Map<String, String> fsmDefinitionMap) {
        this.fsmDefinitionMap = fsmDefinitionMap;
    }



    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public boolean isInited() {
        return inited;
    }

    public void setInited(boolean inited) {
        this.inited = inited;
    }

    public List<Filter<Pair<FsmParam, FsmManager>, FsmResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<Pair<FsmParam, FsmManager>, FsmResult>> filters) {
        this.filters = filters;
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
    public boolean isAutoStartup() {
        return autoStartup;
    }
    
    @Override
    public void stop(Runnable callback) {
        stop();
        callback.run();
    }
    
    @Override
    public int getPhase() {
        return phase;
    }


    public void setPhase(int phase) {
        this.phase = phase;
    }

    public void setAutoStartup(boolean autoStartup) {
        this.autoStartup = autoStartup;
    }


    
    
}

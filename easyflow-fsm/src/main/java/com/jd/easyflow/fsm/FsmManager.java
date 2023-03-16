package com.jd.easyflow.fsm;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.io.IOUtils;
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

    private List<Filter<FsmParam, FsmResult>> filters;

    private  int phase = Integer.MIN_VALUE;

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
        inited = true;
    }

    /**
     * 
     * Loan fsm.
     *
     */
    protected void loadFsm() {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        InputStream is = null;
        try {
            resources = resolver.getResources(fsmPath);
            for (Resource resource : resources) {
                try {
                    logger.info("Start parse fsm definition file:" + resource.getURI());
                    is = resource.getInputStream();
                    String fsmDefinition = IOUtils.toString(is);
                    Fsm fsm = FsmParser.parse(fsmDefinition);
                    if (fsmDefinitionMap.containsKey(fsm.getId())) {
                        throw new FsmException();
                    }
                    fsmDefinitionMap.put(fsm.getId(), fsmDefinition);
                    fsmMap.put(fsm.getId(), fsm);
                } finally {
                    if (is != null) {
                        IOUtils.closeQuietly(is);
                    }
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Fsm definition file load exception", e);
        } finally {
            if (is != null) {
                IOUtils.closeQuietly(is);
            }
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
            FilterChain<FsmParam, FsmResult> chain = new FilterChain<FsmParam, FsmResult>(filters, p -> invokeFsm(p));
            return chain.doFilter(param);
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

    public List<Filter<FsmParam, FsmResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<FsmParam, FsmResult>> filters) {
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
    public int getPhase() {
        return phase;
    }


    public void setPhase(int phase) {
        this.phase = phase;
    }
}

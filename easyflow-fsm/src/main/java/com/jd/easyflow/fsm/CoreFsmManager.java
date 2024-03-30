package com.jd.easyflow.fsm;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.el.ElEvaluator;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.event.FsmEventTrigger;
import com.jd.easyflow.fsm.exception.FsmException;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.fsm.util.FsmEventTypes;
import com.jd.easyflow.fsm.util.FsmIOUtil;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class CoreFsmManager {

    public static final Logger logger = LoggerFactory.getLogger(CoreFsmManager.class);

    protected Map<String, Fsm> fsmMap = new ConcurrentHashMap<>();

    protected Map<String, String> fsmDefinitionMap = new ConcurrentHashMap<String, String>();

    protected String fsmPath;

    protected FsmEventTrigger eventTrigger = new FsmEventTrigger();

    protected List<FsmEventListener> listeners;


    protected volatile boolean inited = false;

    protected List<Filter<Pair<FsmParam, CoreFsmManager>, FsmResult>> filters;
    
    protected Map<String, Object> properties = new ConcurrentHashMap<String, Object>();
    
    protected ElEvaluator elEvaluator;

    public void init() {
        if (inited) {
            return;
        }
        if (elEvaluator == null) {
            elEvaluator = ElFactory.get();
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
    protected abstract void loadFsm();
    
    protected void loadFsmInputStream(InputStream inputstream) throws IOException {
        String fsmDefinition = FsmIOUtil.toString(inputstream);
        Fsm fsm = FsmParser.parse(fsmDefinition, true, elEvaluator);
        if (fsmDefinitionMap.containsKey(fsm.getId())) {
            throw new FsmException();
        }
        fsmDefinitionMap.put(fsm.getId(), fsmDefinition);
        fsmMap.put(fsm.getId(), fsm);
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
        boolean logFlag = param.getContext() != null ? param.getContext().isLogOn() : (param.getLogFlag() == null || param.getLogFlag().booleanValue());
        if (logFlag && logger.isInfoEnabled()) {
            logger.info("FSM MANAGER RUN. fsmId: " + param.getFsmId() + " event:" + param.getEventId() + " currentStateId:"
                    + param.getCurrentStateId() + " opType:" + param.getOpType());
        }
        param.put(FsmConstants.PARAM_KEY_EL_EVALUATOR, this.getElEvaluator());
        if (filters == null || filters.size() == 0) {
            return invokeFsm(param);
        } else {
            FilterChain<Pair<FsmParam, CoreFsmManager>, FsmResult> chain = new FilterChain<Pair<FsmParam, CoreFsmManager>, FsmResult>(filters, p -> invokeFsm(p.getLeft()));
            return chain.doFilter(Pair.of(param, this));
        }
    }

    protected FsmResult invokeFsm(FsmParam param) {
        // No fsm manager listener scenario.
        if (eventTrigger.getListenerList() == null || eventTrigger.getListenerList().size() == 0) {
            Fsm fsm = getFsm(param.getFsmId());
            if (fsm == null) {
                throw new RuntimeException("FSM:" + param.getFsmId() + " not exists");
            }
            FsmResult result = fsm.run(param);
            return result;
        }
        // Has fsm manager listener scenario.
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


    public boolean isInited() {
        return inited;
    }

    public void setInited(boolean inited) {
        this.inited = inited;
    }

    public List<Filter<Pair<FsmParam, CoreFsmManager>, FsmResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<Pair<FsmParam, CoreFsmManager>, FsmResult>> filters) {
        this.filters = filters;
    }


    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }
    
    public <T>T getProperty(String key) {
        return (T) properties.get(key);
    }
    
    public void setProperty(String key, Object value) {
        properties.put(key, value);
    }

    public ElEvaluator getElEvaluator() {
        return elEvaluator;
    }

    public void setElEvaluator(ElEvaluator elEvaluator) {
        this.elEvaluator = elEvaluator;
    }
    
    


    
    
}

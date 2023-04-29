package com.jd.easyflow.fsm;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.event.FsmEventTrigger;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.filter.FilterChain;
import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.FsmPostHandler;
import com.jd.easyflow.fsm.model.FsmPreHandler;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.fsm.util.FsmEventTypes;
import com.jd.easyflow.fsm.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class Fsm {

    public static final Logger logger = LoggerFactory.getLogger(Fsm.class);

    public static final String DOLLAR = "$";

    private String id;

    private String name;

    private FsmPreHandler preHandler;

    private FsmPostHandler postHandler;

    private List<State> stateList = new ArrayList<State>();

    private String startStateId;

    private Map<String, State> stateMap = new HashMap<String, State>();

    private Map<String, Integer> stateIndexMap = new HashMap<>();

    private List<Event> eventList = new ArrayList<Event>();

    private Map<String, Event> eventMap = new HashMap<String, Event>();

    private List<Transition> transitionList = new ArrayList<Transition>();

    private Map<String, Transition> transitionMap = new HashMap<String, Transition>();

    private FsmEventTrigger eventTrigger = new FsmEventTrigger();

    private Map<String, Object> properties;

    private List<Filter<FsmContext, FsmResult>> filters;

    private List<Filter<Triple<Transition, TransitionContext, FsmContext>, Void>> transitionFilters;

    private List<Filter<Pair<TransitionContext, FsmContext>, Void>> transitionActionFilters;

    /**
     * 执行状态机时，外层负责查找执行哪个状态机
     * 
     * @param event
     * @param context
     */
    public FsmResult run(FsmParam param) {
        if (logger.isInfoEnabled()) {
            logger.info("FSM START,fsmId: " + param.getFsmId() + " event:" + param.getEventId() + " currentStateId:"
                    + param.getCurrentStateId() + " opType:" + param.getOpType());
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Param:" + JsonUtil.toJsonString(param));
        }
        FsmContext context = initContext(param);
        if (filters == null || filters.size() == 0) {
            return invokeFsm(context);
        } else {
            FilterChain<FsmContext, FsmResult> chain = new FilterChain<FsmContext, FsmResult>(filters,
                    p -> invokeFsm(p));
            return chain.doFilter(context);
        }
    }

    protected FsmResult invokeFsm(FsmContext context) {
        Throwable throwable = null;
        try {
            eventTrigger.triggerEvent(FsmEventTypes.FSM_START, context);
            // init start state
            initStartState(context);
            if (this.preHandler != null) {
                boolean preResult = this.preHandler.preHandle(context);
                context.setPreResult(preResult);
                if (!preResult) {
                    logger.info("pre result false");
                    eventTrigger.triggerEvent(FsmEventTypes.FSM_END, context);
                    return wrapResult(context);
                }
            }
            while (true) {
                if (context.isInterrupted()) {
                    if (logger.isInfoEnabled()) {
                        logger.info("fsm interrupted");
                        break;
                    }
                }

                State currentState = context.getCurrentState();
                Event event = context.getCurrentEvent();
                if (logger.isInfoEnabled()) {
                    logger.info("Current state:" + currentState.getId() + ", Current event:"
                            + (event == null ? null : event.getId()));
                }
                if (event == null) {
                    break;
                }
                String transitionKey = createTransitionKey(currentState, event);
                Transition transition = transitionMap.get(transitionKey);
                if (transition == null) {
                    logger.warn("No transition found, currentState:" + currentState.getId() + " currentEvent:"
                            + event.getId() + ",EXIT");
                    break;
                }
                beforeTransition(context);
                TransitionContext transitionContext = new TransitionContext();
                transitionContext.setTransition(transition);
                if (!Boolean.FALSE.equals(this.getProperty(FsmConstants.FSM_PROPERTY_RECORD_HISTORY))) {
                    context.addTransition(transitionContext);
                }
                try {
                    runTransition(transition, transitionContext, context);
                } catch (Throwable t) { // NOSONAR
                    transitionContext.setThrowable(t);
                    throw t;
                }
                afterTransition(context);

            }
            if (this.postHandler != null) {
                this.postHandler.postHandle(context);
            }
            eventTrigger.triggerEvent(FsmEventTypes.FSM_END, context);
            return wrapResult(context);
        } catch (Throwable t) { // NOSONAR
            throwable = t;
            logger.error(t.getMessage(), t);
            throw t;
        } finally {
            eventTrigger.triggerEvent(FsmEventTypes.FSM_COMPLETE, throwable, context, true);
        }
    }

    private void runTransition(Transition transition, TransitionContext transitionContext, FsmContext context) {
        if (this.transitionFilters == null || this.transitionFilters.size() == 0) {
            invokeTransition(transition, transitionContext, context);
            return;
        }
        FilterChain<Triple<Transition, TransitionContext, FsmContext>, Void> chain = new FilterChain<Triple<Transition, TransitionContext, FsmContext>, Void>(
                this.transitionFilters, p -> {
                    invokeTransition(transition, transitionContext, context);
                    return null;
                });
        chain.doFilter(Triple.of(transition, transitionContext, context));
    }

    private void invokeTransition(Transition transition, TransitionContext transitionContext, FsmContext context) {
        Throwable tstThrowable;
        try {
            eventTrigger.triggerEvent(FsmEventTypes.TST_START, transitionContext, context, false);
            transition.execute(transitionContext, context);
            eventTrigger.triggerEvent(FsmEventTypes.TST_END, transitionContext, context, false);
        } catch (Throwable t) { // NOSONAR
            tstThrowable = t;
            transitionContext.setThrowable(tstThrowable);
            logger.error("Transition Exception");
            throw t;
        } finally {
            eventTrigger.triggerEvent(FsmEventTypes.TST_COMPLETE, transitionContext, context, true);
        }
    }

    /**
     * 
     * Init Context，Event and state should in context.
     *
     * @param param
     * @return
     */
    public FsmContext initContext(FsmParam param) {
        FsmContext context = param.getContext() != null ? param.getContext() : new FsmContext();
        FsmResult result = new FsmResult();
        context.setParam(param);
        context.setResult(result);
        result.setContext(context);
        context.setFsm(this);
        if (param.getCurrentStateId() != null) {
            State state = getState(param.getCurrentStateId());
            if (state == null) {
                logger.warn("State:" + param.getCurrentStateId() + " not exists");
            }
            context.setCurrentState(state);
        }
        if (param.getEventId() != null) {
            Event event = getEvent(param.getEventId());
            if (event == null) {
                logger.warn("Event:" + param.getEventId() + " not exists");
            }
            context.setCurrentEvent(event);
        }
        return context;
    }

    private State initStartState(FsmContext context) {
        State currentState = context.getCurrentState();
        if (currentState == null) {
            currentState = getState(startStateId);
            context.setCurrentState(currentState);
        }
        return currentState;
    }

    public FsmResult wrapResult(FsmContext context) {
        FsmResult result = context.getResult();
        result.setInstance(context.getStateInstance());
        result.setInstanceId(context.getStateInstanceId());
        result.setState(context.getCurrentState());
        result.setResult(context.getFirstTransitionActionResult());

        result.setTransitionExecuted(context.isTransitionExecuted());
        result.setFirstTransition(context.isFirstTransition());

        result.setFirstTransitionState(context.getFirstTransitionState());
        result.setFirstTransitionEvent(context.getFirstTransitionEvent());
        result.setFirstTransitionResut(context.getFirstTransitionActionResult());

        result.setLastTransitionState(context.getTransitionState());
        result.setLastTransitionEvent(context.getTransitionEvent());
        result.setLastTransitionResult(context.getTransitionActionResult());

        return result;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public List<State> getStateList() {
        return stateList;
    }

    public void initStateList(List<State> stateList) {
        for (State state : stateList) {
            addState(state);
        }
    }

    public List<Event> getEventList() {
        return eventList;
    }

    public void setEventList(List<Event> eventList) {
        this.eventList = eventList;
    }

    public void addState(State state) {
        if (state.getId().startsWith(DOLLAR)) {
            throw new IllegalArgumentException("State ID CANNOT start with $");
        }
        if (!stateMap.containsKey(state.getId())) {
            stateMap.put(state.getId(), state);
            stateList.add(state);
            stateIndexMap.put(state.getId(), stateList.size() - 1);
        }
    }

    public State getState(String stateId) {
        return stateMap.get(stateId);
    }

    public void addEvent(Event event) {
        if (!eventMap.containsKey(event.getId())) {
            eventMap.put(event.getId(), event);
            eventList.add(event);
        }
    }

    public Event getEvent(String eventId) {
        return eventMap.get(eventId);
    }

    public void addTransition(Transition transition) {
        String fromId = transition.getFromId();
        String eventId = transition.getEventId();
        if (fromId == null) {
            throw new RuntimeException("fromId can not be empty");
        }
        if (eventId == null) {
            throw new RuntimeException("eventId  can not be empty");
        }
        transitionList.add(transition);
        transitionMap.put(createTransitionKey(fromId, eventId), transition);
    }

    /**
     * 
     * Process before transition.
     *
     * @param context
     */
    public void beforeTransition(FsmContext context) {
        if (!context.isTransitionExecuted() && context.isFirstTransition()) {
            context.setFirstTransitionState(context.getCurrentState());
            context.setFirstTransitionEvent(context.getCurrentEvent());
        }
        // execute on next transition
        if (context.isTransitionExecuted()) {
            context.setFirstTransition(false);
        }
        context.setTransitionState(context.getCurrentState());
        context.setTransitionEvent(context.getCurrentEvent());
        context.setTransitionActionResult(null);
        context.setTransitionPostState(null);
    }

    /**
     * 
     * Process after transition.
     *
     * @param context
     */
    public void afterTransition(FsmContext context) {
        context.setTransitionExecuted(true);
    }

    private String createTransitionKey(State from, Event event) {
        return "state_" + from.getId() + "_event_" + event.getId();
    }

    private String createTransitionKey(String fromId, String eventId) {
        return "state_" + fromId + "_event_" + eventId;
    }

    public void addListener(FsmEventListener listener) {
        eventTrigger.addListener(listener);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Transition> getTransitionList() {
        return transitionList;
    }

    public void setTransitionList(List<Transition> transitionList) {
        this.transitionList = transitionList;
    }

    public String getStartStateId() {
        return startStateId;
    }

    public void setStartStateId(String startStateId) {
        this.startStateId = startStateId;
    }

    public FsmEventTrigger getEventTrigger() {
        return eventTrigger;
    }

    public void setEventTrigger(FsmEventTrigger eventTrigger) {
        this.eventTrigger = eventTrigger;
    }

    public void setProperty(String key, Object value) {
        if (this.properties == null) {
            properties = new HashMap<>();
        }
        properties.put(key, value);
    }

    public <T> T getProperty(String key) {
        if (properties == null) {
            return null;
        }
        return (T) properties.get(key);
    }

    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    public List<Filter<FsmContext, FsmResult>> getFilters() {
        return filters;
    }

    public void setFilters(List<Filter<FsmContext, FsmResult>> filters) {
        this.filters = filters;
    }

    public void addFilter(Filter<FsmContext, FsmResult> filter) {
        if (this.filters == null) {
            this.filters = new ArrayList<Filter<FsmContext, FsmResult>>();
        }
        this.filters.add(filter);
    }

    public List<Filter<Pair<TransitionContext, FsmContext>, Void>> getTransitionActionFilters() {
        return transitionActionFilters;
    }

    public void setTransitionActionFilters(
            List<Filter<Pair<TransitionContext, FsmContext>, Void>> transitionActionFilters) {
        this.transitionActionFilters = transitionActionFilters;
    }

    public void addTransitionActionFilter(Filter<Pair<TransitionContext, FsmContext>, Void> filter) {
        if (this.transitionActionFilters == null) {
            this.transitionActionFilters = new ArrayList<Filter<Pair<TransitionContext, FsmContext>, Void>>();
        }
        this.transitionActionFilters.add(filter);
    }

    public List<Filter<Triple<Transition, TransitionContext, FsmContext>, Void>> getTransitionFilters() {
        return transitionFilters;
    }

    public void setTransitionFilters(
            List<Filter<Triple<Transition, TransitionContext, FsmContext>, Void>> transitionFilters) {
        this.transitionFilters = transitionFilters;
    }

    public void addTransitionFilter(Filter<Triple<Transition, TransitionContext, FsmContext>, Void> transitionFilter) {
        if (this.transitionFilters == null) {
            this.transitionFilters = new ArrayList<Filter<Triple<Transition, TransitionContext, FsmContext>, Void>>();
        }
        this.transitionFilters.add(transitionFilter);
    }

    public int getStateIndex(String stateId) {
        return stateIndexMap.get(stateId);
    }

    public FsmPreHandler getPreHandler() {
        return preHandler;
    }

    public void setPreHandler(FsmPreHandler preHandler) {
        this.preHandler = preHandler;
    }

    public FsmPostHandler getPostHandler() {
        return postHandler;
    }

    public void setPostHandler(FsmPostHandler postHandler) {
        this.postHandler = postHandler;
    }

}

package com.jd.easyflow.fsm.cases.fsmmanager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.FsmLifeCycle;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * This is only a POC Fsm.
 * @author liyuliang5
 */
public abstract class PocFsm implements FsmLifeCycle {

    private String id;

    private List<State> stateList = new ArrayList<State>();

    private String startStateId;

    private Map<String, State> stateMap = new HashMap<String, State>();

    private Map<String, Integer> stateIndexMap = new HashMap<>();

    private List<Event> eventList = new ArrayList<Event>();

    private Map<String, Event> eventMap = new HashMap<String, Event>();

    private List<Transition> transitionList = new ArrayList<Transition>();

    private Map<String, Transition> transitionMap = new HashMap<String, Transition>();

    /**
     * 
     * 
     * @param event
     * @param context
     */
    public FsmResult run(FsmParam param) {
        // init context
        FsmContext context = new FsmContext();
        FsmResult result = new FsmResult();
        context.setParam(param);
        context.setResult(result);
        result.setContext(context);
        
        // init start state and event
        State currentState = getState(param.getCurrentStateId());
        context.setCurrentState(currentState);
        Event currentEvent = getEvent(param.getEventId());
        context.setCurrentEvent(currentEvent);

        // loop run
        while (true) {
            currentState = context.getCurrentState();
            Event event = context.getCurrentEvent();
            if (event == null) {
                break;
            }
            String transitionKey = createTransitionKey(currentState, event);
            Transition transition = transitionMap.get(transitionKey);
            if (transition == null) {
                break;
            }

            context.setTransitionState(context.getCurrentState());
            context.setTransitionEvent(context.getCurrentEvent());
            context.setTransitionActionResult(null);
            context.setTransitionPostState(null);

            TransitionContext transitionContext = new TransitionContext();
            transitionContext.setTransition(transition);
            transition.execute(transitionContext, context);
        }

        // return result
        result.setResult(context.getFirstTransitionActionResult());
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

}

package com.jd.easyflow.fsm.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.model.Event;
import com.jd.easyflow.fsm.model.FsmPostHandler;
import com.jd.easyflow.fsm.model.FsmPreHandler;
import com.jd.easyflow.fsm.model.InitContext;
import com.jd.easyflow.fsm.model.PostHandleResult;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.TransitionPreHandler;
import com.jd.easyflow.fsm.model.builder.TransitionBuilder;
import com.jd.easyflow.fsm.model.impl.EventImpl;
import com.jd.easyflow.fsm.model.impl.StateImpl;
import com.jd.easyflow.fsm.util.Pair;
import com.jd.easyflow.fsm.util.Triple;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmBuilder {

    private Fsm fsm;

    private FsmBuilder() {
        // NOOP
    }

    public static FsmBuilder create(String id) {
        return create(id, null);
    }

    public static FsmBuilder create(String id, String name) {
        FsmBuilder builder = new FsmBuilder();
        Fsm fsm = new Fsm();
        fsm.setId(id);
        fsm.setName(name);
        builder.fsm = fsm;
        return builder;
    }

    public FsmBuilder state(State state) {
        fsm.addState(state);
        return this;
    }
    
    public FsmBuilder startState(State state) {
        fsm.addState(state);
        fsm.setStartStateId(state.getId());
        return this;
    }
    
    public FsmBuilder startState(String stateId) {
        return startState(addState(stateId));
    }
    
    

    public FsmBuilder state(String id, String name) {
        return state(id, name, false);
    }
    
    public FsmBuilder state(String id, String name, boolean start) {
        StateImpl state = new StateImpl(id, name);
        fsm.addState(state);
        if (start) {
            fsm.setStartStateId(id);
        }
        return this;
    }

    public FsmBuilder states(State[] states) {
        for (State state : states) {
            fsm.addState(state);
        }
        return this;
    }

    public FsmBuilder event(Event event) {
        fsm.addEvent(event);
        return this;
    }

    public FsmBuilder event(String id, String name) {
        EventImpl event = new EventImpl(id, name);
        fsm.addEvent(event);
        return this;
    }

    public FsmBuilder events(Event[] events) {
        for (Event event : events) {
            fsm.addEvent(event);
        }
        return this;
    }

    public FsmBuilder transition(String fromId, String eventId, String toId) {
        State from = addState(fromId);
        Event event = addEvent(eventId);
        State to = addState(toId);
        transition(from, event, to);
        return this;
    }

    public FsmBuilder transition(State from, Event event, State to) {
        TransitionBuilder builder = TransitionBuilder.create();
        builder.fromId(from.getId()).eventId(event.getId()).to(to)
                .toIdList(to == null ? null : Arrays.asList(to.getId()));
        Transition transition = builder.build();
        fsm.addState(from);
        if (to != null) {
            fsm.addState(to);
        }
        fsm.addEvent(event);
        fsm.addTransition(transition);
        return this;
    }

    public FsmBuilder transition(Transition transition) {
        addState(transition.getFromId());
        addEvent(transition.getEventId());
        if (transition.getToIdList() != null) {
            for (String toId : transition.getToIdList()) {
                addState(toId);
            }
        }
        fsm.addTransition(transition);
        return this;
    }

    public FsmBuilder transition(String fromId, String eventId, List<String> toIdList, TransitionPreHandler preHandler,
            TransitionAction action, TransitionPostHandler postHandler) {
        State from = addState(fromId);
        Event event = addEvent(eventId);

        List<State> toList = new ArrayList<>();
        if (toIdList != null) {
            for (String toId : toIdList) {
                State to = addState(toId);
                toList.add(to);
            }
        }
        transition(from, event, toList, preHandler, action, postHandler);
        return this;
    }

    public FsmBuilder transition(State from, Event event, List<State> toList, TransitionPreHandler preHandler,
            TransitionAction action, TransitionPostHandler postHandler) {
        fsm.addState(from);
        fsm.addEvent(event);
        for (State to : toList) {
            fsm.addState(to);
        }

        List<String> toIdList = new ArrayList<>();
        for (State to : toList) {
            toIdList.add(to.getId());
        }
        TransitionBuilder builder = TransitionBuilder.create();
        builder.fromId(from.getId()).eventId(event.getId()).toIdList(toIdList).preHandler(preHandler).action(action)
                .postHandler(postHandler);
        Transition transition = builder.build();
        fsm.addTransition(transition);
        return this;
    }
    
    public FsmBuilder listener(FsmEventListener listener) {
        fsm.addListener(listener);
        return this;
    }
    
    public FsmBuilder filter(Filter<FsmContext, FsmResult> filter) {
        fsm.addFilter(filter);
        return this;
    }
    
    public FsmBuilder transitionFilter(Filter<Triple<Transition, TransitionContext, FsmContext>, Void> filter) {
        fsm.addTransitionFilter(filter);
        return this;
    }
    
    public FsmBuilder transitionActionFilter(Filter<Pair<TransitionContext, FsmContext>, Object> filter) {
        fsm.addTransitionActionFilter(filter);
        return this;
    }
    
    public FsmBuilder transitionPreHandlerFilter(Filter<Pair<TransitionContext, FsmContext>, Boolean> filter) {
        fsm.addTransitionPreHandlerFilter(filter);
        return this;
    }
    
    public FsmBuilder transitionPostHandlerFilter(Filter<Pair<TransitionContext, FsmContext>, PostHandleResult> filter) {
        fsm.addTransitionPostHandlerFilter(filter);
        return this;
    }
    
    public FsmBuilder properties(Map<String, Object> properties) {
        fsm.setProperties(properties);
        return this;
    }
    
    public FsmBuilder putProperties(Map<String, Object> properties) {
        fsm.putProperties(properties);
        return this;
    }
    
    public FsmBuilder fsmPreHandler(FsmPreHandler preHandler) {
        fsm.setPreHandler(preHandler);
        return this;
    }
    
    public FsmBuilder fsmPostHandler(FsmPostHandler postHandler) {
        fsm.setPostHandler(postHandler);
        return this;
    }
    
    public FsmBuilder logFag(Boolean logFlag) {
        fsm.setLogFlag(logFlag);;
        return this;
    }
    
    public Fsm build() {
        return fsm;
    }
    
    public Fsm buildAndInit() {
        InitContext initContext = new InitContext();
        initContext.setParseEl(true);
        initContext.setFsmDefinitionMap(null);
        initContext.setFsm(fsm);
        fsm.init(initContext, null);
        return fsm;
    }
    
    private State addState(String stateId) {
        if (stateId == null) {
            return null;
        }
        State state = fsm.getState(stateId);
        if (state == null) {
            state = new StateImpl(stateId);
            fsm.addState(state);
        }
        return state;
    }
    
    private Event addEvent(String eventId) {
        Event event = fsm.getEvent(eventId);
        if (event == null) {
            event = new EventImpl(eventId);
            fsm.addEvent(event);
        }
        return event;
    }

}

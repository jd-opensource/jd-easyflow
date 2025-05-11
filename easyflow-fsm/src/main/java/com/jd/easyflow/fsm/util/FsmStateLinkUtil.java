package com.jd.easyflow.fsm.util;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.TransitionImpl;
import com.jd.easyflow.fsm.model.impl.post.ConditionalTransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.post.FixedTransitionPostHandler;

/**
 * @author liyuliang5
 */
public class FsmStateLinkUtil {

    private static final Logger logger = LoggerFactory.getLogger(FsmStateLinkUtil.class);

    private static final String STATE_PROP_NEXT_STATES_CACHE = "_$nextStatesCache";
    private static final String STATE_PROP_PREVIOUS_STATES_CACHE = "_$previousStatesCache";

    public static final String STATE_ID_ALL = "$*";
    public static final String STATE_ID_UNKNOWN = "$?";
    private static final String IDX_VAR_PREFIX = "$";


    public static List<String> getNextStates(String stateId, Fsm fsm) {
        State state = fsm.getState(stateId);
        return getNextStates(state, fsm);
    }

    public static List<String> getNextStates(State state, Fsm fsm) {
        List<String> nextStates = state.getProperty(STATE_PROP_NEXT_STATES_CACHE);
        if (nextStates != null) {
            return nextStates;
        }
        nextStates = state.getProperty(FsmConstants.STATE_PROP_NEXT_STATES);
        if (nextStates == null) {
            nextStates = new ArrayList<String>();
            String stateId = state.getId();
            for (Transition transition : fsm.getTransitionList()) {
                if (!transition.getFromId().equals(stateId)) {
                    continue;
                }
                if (transition instanceof TransitionImpl) {
                    TransitionImpl transitionImpl = (TransitionImpl) transition;
                    TransitionPostHandler postHandler = transitionImpl.getPostHandler();
                    if (postHandler == null) {
                        // NOOP
                    } else if (postHandler instanceof FixedTransitionPostHandler) {
                        FixedTransitionPostHandler fixed = (FixedTransitionPostHandler) postHandler;
                        nextStates.addAll(parseTo(fixed.getTo(), stateId, fsm));
                    } else if (postHandler instanceof ConditionalTransitionPostHandler) {
                        ConditionalTransitionPostHandler conditional = (ConditionalTransitionPostHandler) postHandler;
                        List<Map<String, Object>> branchList = conditional.getBranchList();
                        for (Map<String, Object> branch : branchList) {
                            Object next = branch.get("to");
                            List<String> toList = parseTo(next, stateId, fsm);
                            nextStates.addAll(toList);
                        }
                    } else {
                        nextStates.add(STATE_ID_UNKNOWN);
                    }
                } else {
                    nextStates.add(STATE_ID_UNKNOWN);
                }
            }
        }
        if (nextStates.size() > 1) {
            Set<String> nextStateIdsSet = new LinkedHashSet<String>();
            nextStateIdsSet.addAll(nextStates);
            nextStates = new ArrayList<String>(nextStateIdsSet);
        }
        if (nextStates.contains(STATE_ID_UNKNOWN)) {
            if (logger.isInfoEnabled()) {
                logger.info(fsm.getId() + ":" + state.getId() + " next states contains unkown");
            }
        }
        state.setProperty(STATE_PROP_NEXT_STATES_CACHE, nextStates);
        return nextStates;

    }

    public static List<String> getPreviousStates(String stateId, Fsm fsm) {
        State state = fsm.getState(stateId);
        return getPreviousStates(state, fsm);
    }

    public static List<String> getPreviousStates(State state, Fsm fsm) {
        List<String> previousStateIds = state.getProperty(STATE_PROP_PREVIOUS_STATES_CACHE);
        if (previousStateIds != null) {
            return previousStateIds;
        }
        String stateId = state.getId();
        previousStateIds = new ArrayList<String>();
        boolean containsUnknown = false;
        for (State fsmState : fsm.getStateList()) {
            List<String> nextStateIds = getNextStates(fsmState, fsm);
            for (String nextStateId : nextStateIds) {
                if (STATE_ID_UNKNOWN.equals(nextStateId)) {
                    containsUnknown = true;
                } else if (stateId.equals(nextStateId) || STATE_ID_ALL.equals(nextStateId)) {
                    previousStateIds.add(fsmState.getId());
                    break;
                }
            }
        }
        if (containsUnknown) {
            previousStateIds.add(STATE_ID_UNKNOWN);
        }
        state.setProperty(STATE_PROP_PREVIOUS_STATES_CACHE, previousStateIds);
        return previousStateIds;
    }
    
    private static List<String> parseTo(Object to, String stateId, Fsm fsm) {
        List<String> result = new ArrayList<String>();
        // String type
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                result.add(toStr);
            } else {
                toStr = parseIndexVar(toStr, stateId, fsm);
                result.add(toStr);
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            String toStr = fsm.getStateList().get(toIdx).getId();
            result.add(toStr);
            // List type
        } else {
            result.add(STATE_ID_UNKNOWN);
        }
        return result;
    }
    
    private static String parseIndexVar(String var, String stateId, Fsm fsm) {
        int index = -1;
        switch (var) {
        case "$first": {
            index = 0;
            break;
        }
        case "$last": {
            index = fsm.getStateList().size() - 1;
            break;
        }
        case "$previous": {
            index = fsm.getStateIndex(stateId) - 1;
            break;
        }
        case "$next": {
            index = fsm.getStateIndex(stateId) + 1;
            break;
        }
        default: {
            return STATE_ID_UNKNOWN;
        }
        }
        return fsm.getStateList().get(index).getId();
    }

}

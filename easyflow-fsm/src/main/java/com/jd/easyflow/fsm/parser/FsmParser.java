package com.jd.easyflow.fsm.parser;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.builder.FsmBuilder;
import com.jd.easyflow.fsm.el.ElFactory;
import com.jd.easyflow.fsm.event.ExpFsmEventListener;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.filter.ExpFilter;
import com.jd.easyflow.fsm.filter.Filter;
import com.jd.easyflow.fsm.model.Transition;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionPostHandler;
import com.jd.easyflow.fsm.model.TransitionPreHandler;
import com.jd.easyflow.fsm.model.builder.TransitionBuilder;
import com.jd.easyflow.fsm.model.definition.DefConstants;
import com.jd.easyflow.fsm.model.impl.StateImpl;
import com.jd.easyflow.fsm.model.impl.action.ExpTransitionAction;
import com.jd.easyflow.fsm.model.impl.post.ConditionalTransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.post.ExpTransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.post.FixedTransitionPostHandler;
import com.jd.easyflow.fsm.model.impl.pre.ExpTransitionPreHandler;
import com.jd.easyflow.fsm.util.JsonUtil;

/**
 * 
 * Fsm Parser.
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class FsmParser {

    private static final Logger logger = LoggerFactory.getLogger(FsmParser.class);

    private static final String FSM_STRING_KEY = "_fsm_string";
    
    public static Fsm parse(String data) {
        return parse(data, true);
    }

    /**
     * Parse string definition to java model.
     * 
     * @param data
     * @return
     */
    public static Fsm parse(String data, boolean parseEl) {
        Map<String, Object> map = JsonUtil.parseObject(data, Map.class);
        String fsmId = (String) map.get(DefConstants.COMMON_PROP_ID);
        String fsmName = (String) map.get(DefConstants.COMMON_PROP_NAME);
        FsmBuilder builder = FsmBuilder.create(fsmId, fsmName);
        // Parse property
        Map<String, Object> properties = (Map<String, Object>) map.get(DefConstants.COMMON_PROP_PROPERTIES);
        builder.properties(properties);
        // Parse state
        List<Map<String, Object>> states = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_STATES);
        if (states != null) {
            for (Map<String, Object> state : states) {
                boolean start = Boolean.TRUE.equals(state.get(DefConstants.STATE_PROP_START));
                StateImpl stateInfo = new StateImpl((String) state.get(DefConstants.COMMON_PROP_ID), (String) state.get(DefConstants.COMMON_PROP_NAME),
                        (Map<String, Object>) state.get(DefConstants.COMMON_PROP_PROPERTIES));
                if (start) {
                    builder.startState(stateInfo);
                } else {
                    builder.state(stateInfo);
                }
            }
        }
        // Parse event
        List<Map<String, Object>> events = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_EVENTS);
        if (events != null) {
            for (Map<String, Object> event : events) {
                builder.event((String) event.get(DefConstants.COMMON_PROP_ID), (String) event.get(DefConstants.COMMON_PROP_NAME));
            }
        }
        // Parse transition
        List<Map<String, Object>> transitions = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_TRANSITIONS);
        for (Map<String, Object> transition : transitions) {
            // create exp is unsupported.
            TransitionBuilder transBuilder = TransitionBuilder.create();
            TransitionPreHandler preHandler = parsePre(transition.get(DefConstants.TST_PROP_PRE), parseEl);
            TransitionAction transAction = parseAction(transition.get(DefConstants.TST_PROP_ACTION), parseEl);
            TransitionPostHandler postHandler = parsePost(transition.get(DefConstants.TST_PROP_POST), parseEl);
            List<String> toList = (List<String>) transition.get(DefConstants.TST_PROP_TOLIST);

            Object from = transition.get(DefConstants.TST_PROP_FROM);
            List<String> froms = from instanceof String ? Arrays.asList((String) from) : (List<String>) from;
            Object event = transition.get(DefConstants.TST_PROP_EVENT);
            List<String> evts = event instanceof String ? Arrays.asList((String) event) : (List<String>) event;
            for (String fromId : froms) {
                for (String eventId : evts) {
                    Transition trans = transBuilder.fromId(fromId).eventId(eventId).toIdList(toList)
                            .preHandler(preHandler).action(transAction).postHandler(postHandler).build();
                    builder.transition(trans);
                }
            }

        }
        // Listener
        parseListeners(map, builder, parseEl);
        // Filter
       parseFilters(map, builder, parseEl);
        // Transition Filter.
        parseTransitionFilters(map, builder, parseEl);
        // Transition Action Filter.
        parseTransitionActionFilters(map, builder, parseEl);

        Fsm fsm = builder.build();
        fsm.setProperty(FSM_STRING_KEY, data);

        return fsm;
    }
    
    /**
     * Listener.
     * @param map
     * @param builder
     * @param parseEl
     */
    private static void parseListeners(Map<String, Object> map, FsmBuilder builder, boolean parseEl) {
        List<Map<String, Object>> listeners = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_LISTENERS);
        if (listeners != null) {
            for (Object listenerObj : listeners) {
                if (listenerObj instanceof String) {
                    ExpFsmEventListener expFsmEventListener = new ExpFsmEventListener((String) listenerObj);
                    builder.listener(expFsmEventListener);
                } else {
                    Map<String, Object> listener = (Map<String, Object>) listenerObj;
                    String type = (String) listener.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type) || listener.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) listener.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            FsmEventListener eventListener = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            builder.listener(eventListener);
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Filter.
     * @param map
     * @param builder
     * @param parseEl
     */
    private static void parseFilters(Map<String, Object> map, FsmBuilder builder, boolean parseEl) {
        List<Map<String, Object>> filters = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_FILTERS);
        if (filters != null) {
            for (Object filterObj : filters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    builder.filter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type) || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Filter fsmFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            builder.filter(fsmFilter);
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Transition Filter.
     * @param map
     * @param builder
     * @param parseEl
     */
    private static void parseTransitionFilters(Map<String, Object> map, FsmBuilder builder, boolean parseEl) {
        List<Map<String, Object>> transitionFilters = (List<Map<String, Object>>) map.get(DefConstants.FSM_PROP_TRANSITION_FILTERS);
        if (transitionFilters != null) {
            for (Object filterObj : transitionFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    builder.transitionFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type) || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Filter fsmFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            builder.transitionFilter(fsmFilter);
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Transition action filter.
     * @param map
     * @param builder
     * @param parseEl
     */
    private static void parseTransitionActionFilters(Map<String, Object> map, FsmBuilder builder, boolean parseEl) {
        List<Map<String, Object>> transitionActionFilters = (List<Map<String, Object>>) map
                .get(DefConstants.FSM_PROP_TRANSITION_ACTION_FILTERS);
        if (transitionActionFilters != null) {
            for (Object filterObj : transitionActionFilters) {
                if (filterObj instanceof String) {
                    ExpFilter expFilter = new ExpFilter<>((String) filterObj);
                    builder.transitionActionFilter(expFilter);
                } else {
                    Map<String, Object> filter = (Map<String, Object>) filterObj;
                    String type = (String) filter.get(DefConstants.COMMON_PROP_TYPE);
                    if (DefConstants.COMMON_PROP_CREATE.equals(type) || filter.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
                        if (parseEl) {
                            String exp = (String) filter.get(DefConstants.COMMON_PROP_CREATE_EXP);
                            Filter fsmFilter = ElFactory.get().evalWithDefaultContext(exp, null, false);
                            builder.transitionActionFilter(fsmFilter);
                        }
                    }
                }
            }
        }
    }

    public static TransitionPreHandler parsePre(Object preObj, boolean parseEl) {
        if (preObj == null) {
            return null;
        }
        if (preObj instanceof String) {
            ExpTransitionPreHandler transAction = new ExpTransitionPreHandler();
            transAction.setExp((String) preObj);
            return transAction;
        }
        Map<String, Object> pre = (Map<String, Object>) preObj;
        String type = (String) pre.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (!parseEl) {
                return null;
            }
                String exp = (String) pre.get(DefConstants.COMMON_PROP_CREATE_EXP);
                TransitionPreHandler handler = ElFactory.get().evalWithDefaultContext(exp, null, false);
                return handler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || pre.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpTransitionPreHandler transAction = new ExpTransitionPreHandler();
            String exp = (String) pre.get(DefConstants.COMMON_PROP_EXP);
            transAction.setExp(exp);
            return transAction;
        }
        throw new IllegalArgumentException("Param illegal:" + pre);

    }

    public static TransitionAction parseAction(Object actionObj, boolean parseEl) {
        if (actionObj == null) {
            return null;
        }
        if (actionObj instanceof String) {
            ExpTransitionAction transAction = new ExpTransitionAction();
            transAction.setExp((String) actionObj);
            return transAction;
        }
        Map<String, Object> action = (Map<String, Object>) actionObj;
        String type = (String) action.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || action.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (! parseEl) {
                return null;
            }
            String exp = (String) action.get(DefConstants.COMMON_PROP_CREATE_EXP);
            TransitionAction nodeAction = ElFactory.get().evalWithDefaultContext(exp, null, false);
            return nodeAction;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || action.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpTransitionAction transAction = new ExpTransitionAction();
            String exp = (String) action.get(DefConstants.COMMON_PROP_EXP);
            transAction.setExp(exp);
            return transAction;
        }
        throw new IllegalArgumentException("Param illegal:" + action);
    }

    public static TransitionPostHandler parsePost(Object postObj, boolean parseEl) {
        if (postObj == null) {
            return null;
        }
        if (postObj instanceof String) {
            ExpTransitionPostHandler postHandler = new ExpTransitionPostHandler();
            postHandler.setExp((String) postObj);
            return postHandler;
        }
        Map<String, Object> post = (Map<String, Object>) postObj;
        String type = (String) post.get(DefConstants.COMMON_PROP_TYPE);
        if (DefConstants.COMMON_PROP_CREATE.equals(type) || post.containsKey(DefConstants.COMMON_PROP_CREATE_EXP)) {
            if (! parseEl) {
                return null;
            }
            String exp = (String) post.get(DefConstants.COMMON_PROP_CREATE_EXP);
            TransitionPostHandler postHandler = ElFactory.get().evalWithDefaultContext(exp, null, false);
            return postHandler;
        } else if (DefConstants.COMMON_PROP_EXP.equals(type) || post.containsKey(DefConstants.COMMON_PROP_EXP)) {
            ExpTransitionPostHandler postHandler = new ExpTransitionPostHandler();
            String exp = (String) post.get(DefConstants.COMMON_PROP_EXP);
            postHandler.setExp(exp);
            return postHandler;
        } else if (DefConstants.TST_POST_TYPE_CONDITION.equals(type) || post.containsKey(DefConstants.TST_POST_PROP_CONDITIONS) || post.containsKey(DefConstants.TST_POST_PROP_WHEN)) {
            List<Map<String, Object>> conditionList = null;
            if (post.containsKey(DefConstants.TST_POST_PROP_CONDITIONS)) {
                conditionList = (List<Map<String, Object>>) post.get(DefConstants.TST_POST_PROP_CONDITIONS);
            } else {
                conditionList = Arrays.asList(post);
            }
            ConditionalTransitionPostHandler postHandler = new ConditionalTransitionPostHandler(conditionList);
            return postHandler;

        } else if (DefConstants.TST_POST_TYPE_FIXED.equals(type) || post.containsKey(DefConstants.TST_POST_PROP_TO)) {
            Object nextState = (Object) post.get(DefConstants.TST_POST_PROP_TO);
            FixedTransitionPostHandler postHandler = new FixedTransitionPostHandler(nextState);
            return postHandler;
        }
        throw new IllegalArgumentException("Param illegal:" + post);
    }

    /**
     * 
     * Java model to string.
     *
     * @param flow
     * @return
     */
    public static String stringify(Fsm fsm) {
        if (fsm.getProperty(FSM_STRING_KEY) != null) {
            return fsm.getProperty(FSM_STRING_KEY);
        }
        logger.warn("Original fsm string definition not found，unsupported now. fsmId:" + fsm.getId());
        return null;
    }
}

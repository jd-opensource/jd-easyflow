package com.jd.easyflow.fsm.listener;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.fsm.event.FsmEvent;
import com.jd.easyflow.fsm.event.FsmEventListener;
import com.jd.easyflow.fsm.util.FsmConstants;
import com.jd.easyflow.fsm.util.FsmEventTypes;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmCommonListener implements FsmEventListener {
    
    @Override
    public Pair<String, Integer>[] getAcceptedEvents() {
        return new Pair[] {Pair.of(FsmEventTypes.FSM_START, FsmConstants.EVENT_ORDER_START),
                Pair.of(FsmEventTypes.TST_END, -FsmConstants.EVENT_ORDER_START)};
    }

    @Override
    public void on(FsmEvent event) {
        switch (event.getType()) {
        // If there is no event after FSM start, default to enter event when creating instance.
        // TODO: Add enter and exit event is better.
        case FsmEventTypes.FSM_START : {
            if (event.getContext().getCurrentEvent() == null) {
                event.getContext().setCurrentEvent(FsmConstants.COMMON_ENTER_EVENT);
            }
            break;
        }
        // If there is next state after transition, but no event, we set to enter event.
        case FsmEventTypes.TST_END: {
            if ( event.getContext().getCurrentEvent() == null && event.getContext().getTransitionPostState() != null) {
                event.getContext().setCurrentEvent(FsmConstants.COMMON_ENTER_EVENT);
            }
            break;
        }
        default : {
            // NOOP
        }
        }

    }

}

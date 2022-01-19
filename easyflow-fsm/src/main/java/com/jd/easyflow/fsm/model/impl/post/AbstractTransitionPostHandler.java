package com.jd.easyflow.fsm.model.impl.post;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionPostHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class AbstractTransitionPostHandler implements TransitionPostHandler {

    private static final String IDX_VAR_PREFIX = "$";

    protected String parseToStateId(Object to, TransitionContext transitionContext, FsmContext fsmContext) {
        // String type
        if (to instanceof String) {
            String toStr = (String) to;
            if (!toStr.startsWith(IDX_VAR_PREFIX)) {
                return toStr;
            } else {
                return parseIndexVar(toStr, transitionContext, fsmContext);
            }
        } else if (to instanceof Integer) {
            int toIdx = (Integer) to;
            return fsmContext.getFsm().getStateList().get(toIdx).getId();
        } else {
            throw new UnsupportedOperationException("Unsupported type" + to.getClass());
        }
    }

    private String parseIndexVar(String var, TransitionContext transitionContext, FsmContext fsmContext) {
        int index = -1;
        Fsm fsm = fsmContext.getFsm();
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
            index = fsm.getStateIndex(fsmContext.getCurrentState().getId()) - 1;
            break;
        }
        case "$next": {
            index = fsm.getStateIndex(fsmContext.getCurrentState().getId()) + 1;
            break;
        }
        default: {
            throw new UnsupportedOperationException("Unsupported var:" + var);
        }
        }
        return fsm.getStateList().get(index).getId();
    }
}

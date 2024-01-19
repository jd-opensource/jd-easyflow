package com.jd.easyflow.fsm.model.impl.action;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.model.TransitionExecutor;

/**
 * Used for condition create expression.
 * @author liyuliang5
 */
public class ActionResultEl implements TransitionExecutor<Boolean> {

    public static final String OP_EQ = "eq";
    public static final String OP_NEQ = "neq";

    private String operator;

    private Object value;

    public ActionResultEl() {

    }

    public ActionResultEl(String operator, Object value) {
        this.operator = operator;
        this.value = value;
    }

    public static ActionResultEl create(String operator, Object value) {
        ActionResultEl el = new ActionResultEl();
        el.operator = operator;
        el.value = value;
        return el;
    }

    @Override
    public Boolean execute(TransitionContext transitionContext, FsmContext context) {
        Object actionResult = transitionContext.getActionResult();
        switch (operator) {
        case OP_EQ:
            return actionResult == value || (actionResult != null && actionResult.equals(value));
        case OP_NEQ:
            return actionResult != value && (actionResult == null || !actionResult.equals(value));

        default: {
            throw new UnsupportedOperationException("Unsupported operator " + operator);
        }
        }
    }

}

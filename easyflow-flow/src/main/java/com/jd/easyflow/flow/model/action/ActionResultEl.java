package com.jd.easyflow.flow.model.action;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;

/**
 * Used for condition create expression.
 * @author liyuliang5
 */
public class ActionResultEl implements NodeExecutor<Boolean> {

    public static final String OP_EQ = "==";
    public static final String OP_NEQ = "!=";

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
    public Boolean execute(NodeContext nodeContext, FlowContext context) {
        Object actionResult = nodeContext.getActionResult();
        switch (operator) {
        case OP_EQ: 
            return actionResult == value || (actionResult != null && actionResult.equals(value));
        case OP_NEQ:
            return actionResult != value && (actionResult == null || ! actionResult.equals(value));

        default: {
            throw new UnsupportedOperationException("Unsupported operator " + operator);
        }
        }
    }

}

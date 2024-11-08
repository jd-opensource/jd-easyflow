package com.jd.easyflow.action;

import java.util.function.Function;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class ActionAspect {
    
    private static final Logger log = LoggerFactory.getLogger(ActionAspect.class);

    private Function<Object, ActionInfo<Object[], Object>> function;

    public Object process(ProceedingJoinPoint point) throws Throwable {
        Action action = getAction(point);
        if (log.isDebugEnabled()) {
            log.debug("Request:" + action.code() + " " + action.name());
        }
        ActionInfo<Object[], Object> actionInfo = new ActionInfo<>();
        actionInfo.setParam(point.getArgs());
        actionInfo.put(ActionConstants.CTX_JOIN_POINT, point);
        actionInfo.setActionCode(action.code());
        function.apply(actionInfo);
        return actionInfo.getResult();
    }

    private Action getAction(ProceedingJoinPoint point) throws NoSuchMethodException, SecurityException {
        Action action = ((MethodSignature) point.getSignature()).getMethod().getAnnotation(Action.class);
        return action;
    }


    public Function<Object, ActionInfo<Object[], Object>> getFunction() {
        return function;
    }

    public void setFunction(Function<Object, ActionInfo<Object[], Object>> function) {
        this.function = function;
    }

}

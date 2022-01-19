package com.jd.easyflow.fsm.el;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionContext;
import com.jd.easyflow.fsm.util.JsonUtil;
import com.jd.easyflow.fsm.util.SpelHelper;

/**
 * 
 * @author liyuliang5
 *
 */
public class SpelEvaluator implements ElEvaluator {

    private static final Logger logger = LoggerFactory.getLogger(SpelEvaluator.class);

    @Override
    public <T> T evalWithDefaultContext(String exp, Object root, boolean cache) {
        return SpelHelper.evalWithDefaultContext(exp, root, cache);
    }

    @Override
    public <T> T eval(String exp, TransitionContext transitionContext, FsmContext fsmContext,
            Map<String, Object> data) {
        if (logger.isInfoEnabled()) {
            logger.info("EVAL SPEL:" + exp);
        }
        Map<String, Object> root = new HashMap<>();
        if (fsmContext != null) {
            root.put("context", fsmContext);
            root.put("param", fsmContext.getParam());
            if (fsmContext.getParam() != null && fsmContext.getParam().getParam() != null) {
                root.put("bizParam", fsmContext.getParam().getParam());
            }
            if (fsmContext.getParam() != null && fsmContext.getParam().getDataMap() !=  null) {
                root.put("paramData", fsmContext.getParam().getDataMap());
            }
            root.put("result", fsmContext.getResult());
            if (fsmContext.getResult() != null && fsmContext.getResult().getResult() != null) {
                root.put("bizResult", fsmContext.getResult().getResult());
            }
        }
        if (fsmContext != null) {
            root.put("transitionContext", transitionContext);
            root.put("actionResult", transitionContext == null ? null : transitionContext.getActionResult());
        }
        if (data != null) {
            root.putAll(data);
        }
        Object result = SpelHelper.evalWithDefaultContext(exp, root, true);
        if (logger.isInfoEnabled()) {
            logger.info("SPEL RESULT:" + JsonUtil.toJsonString(result));
        }
        return (T) result;
    }

}

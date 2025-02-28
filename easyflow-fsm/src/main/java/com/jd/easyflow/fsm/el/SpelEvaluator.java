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

    // Normal implementation.
    public static final int ROOT_TYPE_HASH_MAP = 0;
    // Performance is higher than HashMap
    public static final int ROOT_TYPE_ROOT_MAP = 1;

    private int rootType = ROOT_TYPE_ROOT_MAP;
    
    private boolean cache = true;

    @Override
    public <T> T evalWithDefaultContext(String exp, Object root, boolean cache) {
        try {
            return SpelHelper.evalWithDefaultContext(exp, root, cache);
        } catch (Exception e) {
            if (logger.isErrorEnabled()) {
                logger.error("eval spel exception, exp:" + exp + "," + e.getMessage());
            }
            throw e;
        }
    }

    @Override
    public <T> T eval(String exp, TransitionContext transitionContext, FsmContext fsmContext,
            Map<String, Object> data) {
        if ((fsmContext == null || fsmContext.isLogOn()) && logger.isInfoEnabled()) {
            logger.info("EVAL SPEL:" + exp);
        }
        Object root = null;
        switch (rootType) {
        case ROOT_TYPE_HASH_MAP:
            root = buildHashMapRoot(transitionContext, fsmContext, data);
            break;
        case ROOT_TYPE_ROOT_MAP:
            root = buildRootMapRoot(transitionContext, fsmContext, data);
            break;
        }

        Object result = null;
        try {
            result =  SpelHelper.evalWithDefaultContext(exp, root, cache);
        } catch (Exception e) {
            if ((fsmContext == null || fsmContext.isLogOn()) && logger.isErrorEnabled()) {
                logger.error("EVAL SPEL EXCEPTION, exp:" + exp + "," + e.getMessage());
            } 
            throw e;
        }
        if ((fsmContext == null || fsmContext.isLogOn()) && logger.isInfoEnabled()) {
            try {
                logger.info("SPEL RESULT:" + JsonUtil.toJsonString(result));
            } catch (Throwable t) {
                logger.info("spel result to json string exception:" + t.getMessage());
            }
        }
        return (T) result;
    }

    private Object buildHashMapRoot(TransitionContext transitionContext, FsmContext fsmContext,
            Map<String, Object> data) {
        Map<String, Object> root = new HashMap<>();
        if (fsmContext != null) {
            root.put("context", fsmContext);
            if (fsmContext.getContext() != null) {
                root.put("bizContext", fsmContext.getContext());
            }
            root.put("param", fsmContext.getParam());
            if (fsmContext.getParam() != null && fsmContext.getParam().getParam() != null) {
                root.put("bizParam", fsmContext.getParam().getParam());
            }
            if (fsmContext.getParam() != null && fsmContext.getParam().getDataMap() != null) {
                root.put("paramData", fsmContext.getParam().getDataMap());
            }
            root.put("result", fsmContext.getResult());
            if (fsmContext.getResult() != null && fsmContext.getResult().getResult() != null) {
                root.put("bizResult", fsmContext.getResult().getResult());
            }
        }
        if (transitionContext != null) {
            root.put("transitionContext", transitionContext);
            root.put("actionResult", transitionContext.getActionResult());
            if (transitionContext.getTransitionContext() != null) {
                root.put("transitionBizContext", transitionContext.getTransitionContext());
            }
        }
        if (data != null) {
            root.putAll(data);
        }
        return root;
    }

    private Object buildRootMapRoot(TransitionContext transitionContext, FsmContext fsmContext,
            Map<String, Object> data) {
        ElRootMap root = new ElRootMap();
        if (fsmContext != null) {
            root.context = fsmContext;
            if (fsmContext.getContext() != null) {
                root.bizContext = fsmContext.getContext();
            }
            root.param = fsmContext.getParam();
            if (fsmContext.getParam() != null && fsmContext.getParam().getParam() != null) {
                root.bizParam = fsmContext.getParam().getParam();
            }
            if (fsmContext.getParam() != null && fsmContext.getParam().getDataMap() != null) {
                root.paramData = fsmContext.getParam().getDataMap();
            }
            root.result = fsmContext.getResult();
            if (fsmContext.getResult() != null && fsmContext.getResult().getResult() != null) {
                root.bizResult = fsmContext.getResult().getResult();
            }
        }
        if (transitionContext != null) {
            root.transitionContext = transitionContext;
            root.actionResult = transitionContext.getActionResult();
            if (transitionContext.getTransitionContext() != null) {
                root.transitionBizContext = transitionContext.getTransitionContext();
            }
        }
        if (data != null) {
            root.data = data;
        }
        return root;
    }

    public int getRootType() {
        return rootType;
    }

    public void setRootType(int rootType) {
        this.rootType = rootType;
    }

    public boolean isCache() {
        return cache;
    }

    public void setCache(boolean cache) {
        this.cache = cache;
    }
    
    

}

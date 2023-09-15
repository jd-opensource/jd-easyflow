package com.jd.easyflow.flow.el;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.JsonUtil;
import com.jd.easyflow.flow.util.SpelHelper;

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
    public <T>T eval(String exp, NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data) {
        if (flowContext.isLogOn() && logger.isInfoEnabled()) {
            logger.info("EVAL SPEL:" + exp);
        }
        Map<String, Object> root = new HashMap<>();
        if (nodeContext != null) {
            root.put("nodeContext", nodeContext);
            root.put("actionResult", nodeContext == null ? null : nodeContext.getActionResult());
        }
        if (flowContext != null) {
            root.put("context", flowContext);
            root.put("param", flowContext.getParam());
            if (flowContext.getParam() != null && flowContext.getParam().getParam() != null) {
                root.put("bizParam", flowContext.getParam().getParam());
            }
            if (flowContext.getParam() != null && flowContext.getParam().getDataMap() !=  null) {
                root.put("paramData", flowContext.getParam().getDataMap());
            }
            root.put("result", flowContext.getResult());
            if (flowContext.getResult() != null && flowContext.getResult().getResult() != null) {
                root.put("bizResult", flowContext.getResult().getResult());
            }            
        }
        if (data != null) {
            root.putAll(data);
        }
        Object result = SpelHelper.evalWithDefaultContext(exp, root, true);
        if (flowContext.isLogOn() && logger.isInfoEnabled()) {
            logger.info("SPEL RESULT:" + JsonUtil.toJsonString(result));
        }
        return (T) result;
    }

}

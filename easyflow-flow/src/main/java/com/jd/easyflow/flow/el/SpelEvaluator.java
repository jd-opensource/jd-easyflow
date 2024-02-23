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

    // Normal implementation.
    public static final int ROOT_TYPE_HASH_MAP = 0;
    // Performance is higher than HashMap
    public static final int ROOT_TYPE_ROOT_MAP = 1;

    private int rootType = ROOT_TYPE_ROOT_MAP;

    private boolean cache = true;

    @Override
    public <T> T evalWithDefaultContext(String exp, Object root, boolean cache) {
        return SpelHelper.evalWithDefaultContext(exp, root, cache);
    }

    @Override
    public <T> T eval(String exp, NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data) {
        if (flowContext.isLogOn() && logger.isInfoEnabled()) {
            logger.info("EVAL SPEL:" + exp);
        }
        Object root = null;
        switch (rootType) {
        case ROOT_TYPE_HASH_MAP:
            root = buildHashMapRoot(nodeContext, flowContext, data);
            break;
        case ROOT_TYPE_ROOT_MAP:
            root = buildRootMapRoot(nodeContext, flowContext, data);
            break;
        }

        Object result = SpelHelper.evalWithDefaultContext(exp, root, cache);
        if (flowContext.isLogOn() && logger.isInfoEnabled()) {
            logger.info("SPEL RESULT:" + JsonUtil.toJsonString(result));
        }
        return (T) result;
    }

    private Object buildHashMapRoot(NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data) {
        Map<String, Object> root = new HashMap<>();
        if (nodeContext != null) {
            root.put("nodeContext", nodeContext);
            root.put("actionResult", nodeContext.getActionResult());
            if (nodeContext.getNodeContext() != null) {
                root.put("nodeBizContext", nodeContext.getNodeContext());
            }
        }
        if (flowContext != null) {
            root.put("context", flowContext);
            if (flowContext.getContext() != null) {
                root.put("bizContext", flowContext.getContext());
            }
            root.put("param", flowContext.getParam());
            if (flowContext.getParam() != null && flowContext.getParam().getParam() != null) {
                root.put("bizParam", flowContext.getParam().getParam());
            }
            if (flowContext.getParam() != null && flowContext.getParam().getDataMap() != null) {
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
        return root;
    }

    private Object buildRootMapRoot(NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data) {
        ElRootMap root = new ElRootMap();
        if (nodeContext != null) {
            root.nodeContext = nodeContext;
            root.actionResult = nodeContext.getActionResult();
            if (nodeContext.getNodeContext() != null) {
                root.nodeBizContext = nodeContext.getNodeContext();
            }
        }
        if (flowContext != null) {
            root.context = flowContext;
            if (flowContext.getContext() != null) {
                root.bizContext = flowContext.getContext();
            }
            root.param = flowContext.getParam();
            if (flowContext.getParam() != null && flowContext.getParam().getParam() != null) {
                root.bizParam = flowContext.getParam().getParam();
            }
            if (flowContext.getParam() != null && flowContext.getParam().getDataMap() != null) {
                root.paramData = flowContext.getParam().getDataMap();
            }
            root.result = flowContext.getResult();
            if (flowContext.getResult() != null && flowContext.getResult().getResult() != null) {
                root.bizResult = flowContext.getResult().getResult();
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

package com.jd.easyflow.flow.el;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class SpelEvaluator implements ElEvaluator, ApplicationContextAware {

    private static final Logger logger = LoggerFactory.getLogger(SpelEvaluator.class);

    // Normal implementation.
    public static final int ROOT_TYPE_HASH_MAP = 0;
    // Performance is higher than HashMap
    public static final int ROOT_TYPE_ROOT_MAP = 1;

    private int rootType = ROOT_TYPE_ROOT_MAP;

    private boolean cache = true;
    

    private StandardEvaluationContext context = new StandardEvaluationContext();

    private Map<String, Expression> cacheMap = new ConcurrentHashMap();

    private ExpressionParser parser = new SpelExpressionParser();

    private ApplicationContext applicationContext;
    
    {
        context.addPropertyAccessor(new MapAccessor());
    }

    @Override
    public <T> T evalWithDefaultContext(String exp, Object root, boolean cache) {
        try {
            Expression expression;
            if (cache) {
                expression = cacheMap.get(exp);
                if (expression == null) {
                    expression = parser.parseExpression(exp);
                    cacheMap.put(exp, expression);
                }
            } else {
                expression = parser.parseExpression(exp);
            }

            Object value = expression.getValue(context, root);
            return (T) value;
        } catch (Exception e) {
            if (logger.isErrorEnabled()) {
                logger.error("Eval spel exception, exp:" + exp + "," + e.getMessage());
            }
            throw e;
        }
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
        Object result = null;
        try {
            result = evalWithDefaultContext(exp, root, cache);
        } catch (Exception e) {
            if (flowContext.isLogOn() && logger.isErrorEnabled()) {
                logger.error("EVAL SPEL EXCEPTION, EXP:" + exp + "," + e.getMessage());
            }
            throw e;
        }
        if (flowContext.isLogOn() && logger.isInfoEnabled()) {
            try {
                logger.info("SPEL RESULT:" + JsonUtil.toJsonString(result));
            } catch (Throwable t) {
                logger.info("spel result to json string exception:" + t.getMessage());
            }
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
                root.put("resultData", flowContext.getResult().getDataMap());
            }
        }
        if (data != null) {
            root.putAll(data);
        }
        return root;
    }

    private Object buildRootMapRoot(NodeContext nodeContext, FlowContext flowContext, Map<String, Object> data) {
        ElRootMap root = new ElRootMap();
        root.nodeContext = nodeContext;
        root.context = flowContext;
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
    
    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
        context.setBeanResolver(new BeanFactoryResolver(applicationContext));
    }

    public StandardEvaluationContext getContext() {
        return context;
    }

    public void setContext(StandardEvaluationContext context) {
        this.context = context;
    }

    public ExpressionParser getParser() {
        return parser;
    }

    public void setParser(ExpressionParser parser) {
        this.parser = parser;
    }
    
    


}

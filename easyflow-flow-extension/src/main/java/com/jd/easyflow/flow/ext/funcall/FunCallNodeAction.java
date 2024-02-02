package com.jd.easyflow.flow.ext.funcall;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.util.ClassUtils;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * Function call node action.
 * 
 * @author liyuliang5
 *
 */
public class FunCallNodeAction implements NodeAction, ApplicationContextAware {

    private static final Logger logger = LoggerFactory.getLogger(FunCallNodeAction.class);

    private Object instance;

    private Method method;

    private List<Map<String, Object>> paramList;

    private List<Map<String, Object>> resultList;

    private String nodeId;

    private ApplicationContext applicationContext;

    public void create(Map<String, Object> actionConf, FlowNode node) throws Exception {
        FunCallNodeAction nodeAction = new FunCallNodeAction();
        nodeAction.applicationContext = this.applicationContext;
        nodeAction.init(actionConf, node);
    }

    public FunCallNodeAction() {
        // NOOP
    }

    public FunCallNodeAction(Map<String, Object> actionConf, FlowNode node) throws Exception {
        init(actionConf, node);
    }

    public void init(Map<String, Object> actionConf, FlowNode node) throws Exception {
        paramList = (List<Map<String, Object>>) actionConf.get("param");
        resultList = (List<Map<String, Object>>) actionConf.get("result");

        String classMethod = (String) actionConf.get("classMethod");
        String methodName = null;
        Class clazz = null;
        if (StringUtils.isNotEmpty(classMethod)) {
            String[] info = classMethod.split("::");
            String clazzName = info[0];
            methodName = info[1];
            clazz = Class.forName(clazzName);
            instance = clazz.newInstance();
        }
        if (instance == null) {
            String beanMethod = (String) actionConf.get("beanMethod");
            if (StringUtils.isEmpty(beanMethod)) {
                String[] info = classMethod.split("::");
                methodName = info[1];
                instance = applicationContext.getBean(methodName);
                clazz = instance.getClass();
            }
        }

        method = ClassUtils.getMethod(clazz, methodName, Map.class);
        nodeId = node.getId();
    }

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        Map<String, Object> inputMap = new HashMap<>();
        Map<String, Object> nodeActionMap = context.get(FunCallConstants.CTX_NODE_ACTION_DATA_MAP);
        if (paramList != null) {
            for (Map<String, Object> param : paramList) {
                String key = (String) param.get("key");
                String valueExp = (String) param.get("value");
                Map<String, Object> contextMap = new HashMap<>();
                contextMap.put("node", nodeActionMap);
                String value = context.getElEvaluator().eval(valueExp, nodeContext, context, contextMap);
                inputMap.put(key, value);
            }
        }

        try {
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("function input:" + inputMap);
            }
            Map<String, Object> outputMap = (Map<String, Object>) method.invoke(instance, inputMap);
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("function output:" + outputMap);
            }

            Map<String, Object> resultMap = outputMap;
            if (resultList != null) {
                resultMap = new HashMap<>();
                Map<String, Object> contextMap = new HashMap<>();
                contextMap.put("node", context.get(FunCallConstants.CTX_NODE_ACTION_DATA_MAP));
                for (Map<String, Object> result : resultList) {
                    String key = (String) result.get("key");
                    String valueExp = (String) result.get("value");
                    Object value = null;
                    if (StringUtils.isNotEmpty(valueExp)) {
                        value = context.getElEvaluator().eval(valueExp, null, context, contextMap);
                    } else {
                        value = result.get(key);
                    }
                    String scope = (String) result.get("scope");
                    if (FunCallConstants.RESULT_SCOPE_NODE.equals(scope)) {
                        nodeContext.put(key, value);
                    } else if (FunCallConstants.RESULT_SCOPE_FLOW.equals(scope)) {
                        context.put(key, value);
                    } else {
                        result.put(key, value);
                    }
                }
            }
            if (context.isLogOn() && logger.isInfoEnabled()) {
                logger.info("function result:" + resultMap);
            }
            Map<String, Object> info = new HashMap<>();
            info.put("res", outputMap);
            nodeActionMap.put(nodeId, info);

        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new FlowException(e);
        }
        return null;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;

    }

}

package com.jd.easyflow.flow.cases.action;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ClassUtils;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class MockClassMethodAction implements NodeAction {

    private static final Logger logger = LoggerFactory.getLogger(MockClassMethodAction.class);

    private Object instance;

    private Method method;

    private List<Map<String, Object>> paramList;

    private List<Map<String, Object>> resultList;
    
    private String nodeId;

    public MockClassMethodAction(Map<String, Object> actionConf, FlowNode node) throws Exception {
        String classMethod = (String) actionConf.get("classMethod");
        paramList = (List<Map<String, Object>>) actionConf.get("param");
        resultList = (List<Map<String, Object>>) actionConf.get("result");

        String[] info = classMethod.split("::");
        String clazzName = info[0];
        String methodName = info[1];
        Class clazz = Class.forName(clazzName);
        method = ClassUtils.getMethod(clazz, methodName, Map.class);
        instance = clazz.newInstance();
        
        nodeId = node.getId();
    }

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        Map<String, Object> paramMap = new HashMap<>();
        Map<String, Object> nodeActionMap = context.get("nodeActionInfoMap");
        for (Map<String, Object> param : paramList) {
            String key = (String) param.get("key");
            String valueExp = (String) param.get("value");
            Map<String, Object> contextMap = new HashMap<>();
            contextMap.put("node", nodeActionMap);
            String value = context.getElEvaluator().eval(valueExp, nodeContext, context, contextMap);
            paramMap.put(key, value);
        }
        try {
            logger.info("入参:" + paramMap);
            Map<String, Object> resultMap = (Map<String, Object>) method.invoke(instance, paramMap);
            logger.info("结果:" + resultMap);
            Map<String, Object> outputMap = new HashMap<>();
            for (Map<String, Object> result : resultList) {
                String key = (String) result.get("key");
                outputMap.put(key, result.get("key"));
            }
            Map<String, Object> info = new HashMap<>();
            info.put("res", outputMap);
            nodeActionMap.put(nodeId, info);
            
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new FlowException(e);
        }
        return null;
    }

}

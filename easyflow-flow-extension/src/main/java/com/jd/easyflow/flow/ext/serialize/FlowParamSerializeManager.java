package com.jd.easyflow.flow.ext.serialize;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public class FlowParamSerializeManager {


    private static FlowParamSerializeManager INSTANCE = new FlowParamSerializeManager();
    
    private Map<String, FlowParamSerializer> serializerMap = new ConcurrentHashMap<>();

    public static FlowParamSerializeManager getInstance() {
        return INSTANCE;
    }
    
    public FlowParamSerializer getSerializer(String className) {
        return serializerMap.computeIfAbsent(className, key -> {
            try {
                return (FlowParamSerializer) Class.forName(className).newInstance();
            } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
                throw ExceptionUtil.throwException(e);
            }
        });
    }
    
    public String serialize(String className, FlowParam param, Map<String, Object> config) {
        FlowParamSerializer serializer = getSerializer(className);
        String s = serializer.serialize(param, config);
        return s;
    }
    
    public FlowParam deserialize(String className, String s, Map<String, Object> config) {
        FlowParamSerializer serializer = getSerializer(className);
        FlowParam param = serializer.deserialize(s, config);
        return param;
    }
}

package com.jd.easyflow.process.client.common;

import java.util.Map;

import com.jd.easyflow.process.client.runtime.StdNode;
import com.jd.easyflow.process.client.runtime.StdNodeContext;
import com.jd.easyflow.process.client.runtime.StdProcess;
import com.jd.easyflow.process.client.runtime.StdProcessContext;

/**
 * 
 * @author liyuliang5
 * 
 */
public class PropertiesUtil {

    public static <T> T get(Map<String, Object> properties, String key) {
        if (properties == null) {
            return null;
        }
        return (T) properties.get(key);
    }

    public static void put(Map<String, Object> properties, String key, Object value) {
        if (value == null) {
            properties.remove(key);
        } else {
            properties.put(key, value);
        }
    }

    /**
     * 
     * @param <T>
     * @param key
     * @param propertiesList
     * @return
     */
    public static <T> T get(String key, Map<String, Object>... propertiesList) {
        if (propertiesList == null || propertiesList.length == 0) {
            return null;
        }
        for (Map<String, Object> properties : propertiesList) {
            if (properties != null) {
                Object value = properties.get(key);
                if (value != null) {
                    return (T) value;
                }
            }
        }
        return null;
    }

    /**
     * @param <T>
     * @param key
     * @param context
     * @return
     */
    public static <T> T getProcessProperty(String key, StdProcessContext context) {
        return get(key, context.getProcessProperties(), context.getProcessParamProperties(),
                context.getProcess() == null ? null : ((StdProcess) context.getProcess()).getProcessProperties());
    }

    /**
     * @param <T>
     * @param key
     * @param context
     * @return
     */
    public static <T> T getNodeProperty(String key, StdNodeContext context) {
        return get(key, context.getProcessProperties(), context.getProcessParamProperties(),
                context.getNode() == null ? null : ((StdNode) context.getNode()).getProcessProperties());
    }

    /**
     * @param <T>
     * @param key
     * @param nodeContext
     * @param processContext
     * @return
     */
    public static <T> T getProperty(String key, StdNodeContext nodeContext, StdProcessContext processContext) {
        Object nodeValue = getNodeProperty(key, nodeContext);
        if (nodeValue != null) {
            return (T) nodeValue;
        }
        return (T) getProcessProperty(key, processContext);
    }

}

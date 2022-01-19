package com.jd.easyflow.flow.bpmn.converter.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class ConvertUtil {

    public static Map<String, Object> getMapValue(Map<String, Object> obj, String key) {
        Map<String, Object> value = (Map<String, Object>) obj.get(key);
        if (value == null) {
            value = new HashMap<>();
            obj.put(key, value);
        }
        return value;
    }
    
    public static List<Object> getListValue(Map<String, Object> obj, String key) {
        List<Object> value = (List<Object>) obj.get(key);
        if (value == null) {
            value = new ArrayList<>();
            obj.put(key, value);
        }
        return value;
    }
}

package com.jd.easyflow.flow.cases.action;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class MockActionClass {

    public Map<String, Object> method1(Map<String, Object> param) {
        Map<String, Object> result = new HashMap<>();
        result.put("r1", "hello " + param.get("A"));
        result.put("r2", "hello r2");
        return result;
    }
    
    public Map<String, Object> method3(Map<String, Object> param) {
        Map<String, Object> result = new HashMap<>();
        result.put("r3", "hello " + param.get("A"));
        return result;
    }

}

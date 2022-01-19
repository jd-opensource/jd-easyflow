package com.jd.easyflow.flow.el;

import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public class ElFactory {
    
    private static Map<String, ElEvaluator> evaluatorMap;
    
    private static ElEvaluator defaultEvaluator = new SpelEvaluator();
    
    public static ElEvaluator get() {
        return defaultEvaluator;
    }

    public static ElEvaluator get(String type) {
        return evaluatorMap.get(type);
    }
    
    public static void setDefaultEvaluator(ElEvaluator evaluator) {
        defaultEvaluator = evaluator;
    }
}

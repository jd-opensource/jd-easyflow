package com.jd.easyflow.flow.el;

/**
 * 
 * @author liyuliang5
 *
 */
public class ElFactory {
    
    private static ElEvaluator defaultEvaluator = new SpelEvaluator();
    
    public static ElEvaluator get() {
        return defaultEvaluator;
    }
    
    public static void setDefaultEvaluator(ElEvaluator evaluator) {
        defaultEvaluator = evaluator;
    }
      
    public void setDefault(ElEvaluator evaluator) {
        ElFactory.defaultEvaluator = evaluator;
    }
}

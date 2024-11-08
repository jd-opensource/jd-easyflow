package com.jd.easyflow.el;

import com.jd.easyflow.spel.SpelHelper;

/**
 * 
 * @author liyuliang5
 *
 */
public class SpelEvaluator implements ElEvaluator {
    
    @Override
    public <T> T evalWithDefaultContext(String exp, Object root, boolean cache) {
        return SpelHelper.evalWithDefaultContext(exp, root, cache);
    }
    
    @Override
    public String evalTemplateWithDefaultContext(String template, Object context, boolean cache) {
        return SpelHelper.evalTemplateWithDefaultContext(template, context, cache);
    }

}

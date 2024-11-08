package com.jd.easyflow.el;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ElEvaluator {

    /**
     * Evaluate with default context.
     * 
     * @param <T>
     * @param exp
     * @param root
     * @param cache
     * @return
     */
    <T> T evalWithDefaultContext(String exp, Object root, boolean cache);

    default String evalTemplateWithDefaultContext(String template, Object root, boolean cache) {
        return evalWithDefaultContext(template, root, cache);
    }

}

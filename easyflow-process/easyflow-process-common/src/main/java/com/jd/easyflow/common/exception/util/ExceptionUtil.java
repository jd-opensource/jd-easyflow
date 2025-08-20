package com.jd.easyflow.common.exception.util;

/**
 * @author liyuliang5
 */
public class ExceptionUtil {

    /**
     * @param t
     * @return
     */
    public static RuntimeException throwException(Throwable t) {
        if (t == null) {
            throw new NullPointerException("Exception is null");
        }
        return throw0(t);
    }

    private static <T extends Throwable> T throw0(Throwable t) throws T {
        throw (T) t;
    }
    
}

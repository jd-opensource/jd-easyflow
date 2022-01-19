package com.jd.easyflow.flow.util;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExceptionUtil {

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

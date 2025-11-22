package com.jd.easyflow.integration.all.admin.sample;

/**
 * @author liyuliang5
 */
public class SampleUserHolder {

    private static ThreadLocal<String> userThreadLocal = new ThreadLocal<String>();
    
    public static void set(String user) {
        userThreadLocal.set(user);
    }
    
    public static String get() {
        return userThreadLocal.get();
    }
    
    public static void remove() {
        userThreadLocal.remove();
    }

}

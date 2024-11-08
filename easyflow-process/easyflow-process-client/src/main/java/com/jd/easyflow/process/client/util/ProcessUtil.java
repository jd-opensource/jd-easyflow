package com.jd.easyflow.process.client.util;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUtil {

    public static String nsKey(String key) {
        return ProcessConstants.NS_SEP + key;
    }
    
    public static String nsKey(String ns, String key) {
        return ns + ProcessConstants.NS_SEP + key;
    }
    
}

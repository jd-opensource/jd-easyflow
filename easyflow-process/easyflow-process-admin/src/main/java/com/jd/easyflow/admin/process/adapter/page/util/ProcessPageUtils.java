package com.jd.easyflow.admin.process.adapter.page.util;

import org.apache.commons.lang3.StringUtils;

/**
 * @author liyuliang5
 */
public class ProcessPageUtils {
    /**
     * @param val
     * @return "null" or val
     */
    public static String handleAddDefaultValue(String val){
        if(StringUtils.isBlank(val)){
            return "null";
        }
        return val;
    }
}

package com.jd.easyflow.process.client.util;

import java.util.Map;

import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class ElUtil {

    /**
     * @param json
     * @return
     */
    public static Object json2Map(String json) {
        return JSON.parseObject(json, Map.class);
    }
    
    
}

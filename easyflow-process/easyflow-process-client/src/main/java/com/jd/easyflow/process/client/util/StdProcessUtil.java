package com.jd.easyflow.process.client.util;

import java.util.Map;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 */
public class StdProcessUtil {

    public static String getNodeInstanceVar(String key, ProcessNodeInstanceDTO processNodeInstance) {
        String varsString = processNodeInstance.getVars();
        if (varsString == null) {
            return null;
        }
        Map<String, String> vars = JSON.parseObject(varsString, Map.class);
        return vars.get(key);
     }
}

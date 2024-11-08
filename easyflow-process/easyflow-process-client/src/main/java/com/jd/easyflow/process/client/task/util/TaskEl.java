package com.jd.easyflow.process.client.task.util;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.process.client.task.TaskConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class TaskEl {

    /**
     * 
     * @param param
     * @param key
     * @return
     */
    public static Object bizData(FlowParam param, String key) {
        String executeData = param.getParam(TaskConstants.PARAM_TASK_EXECUTE_DATA);
        Map<String, Object> map = JSON.parseObject(executeData, Map.class);
        if (map == null) {
            return null;
        }
        return map.get(key);
    }

}

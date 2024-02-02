package com.jd.easyflow.flow.ext.funcall;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowPreHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class FunCallFlowPreHandler implements FlowPreHandler {

    private List<Map<String, Object>> paramList;

    public FunCallFlowPreHandler(Map<String, Object> conf) {
        paramList = (List<Map<String, Object>>) conf.get("param");
    }

    @Override
    public boolean preHandle(FlowContext context) {
        // 节点的执行信息
        Map<String, Object> nodeActionDataMap = context.get(FunCallConstants.CTX_NODE_ACTION_DATA_MAP);
        if (nodeActionDataMap == null) {
            nodeActionDataMap = new ConcurrentHashMap<>();
            context.put(FunCallConstants.CTX_NODE_ACTION_DATA_MAP, nodeActionDataMap);
        }

        if (paramList != null) {
            Map<String, Object> paramMap = new HashMap<>();
            for (Map<String, Object> param : paramList) {
                String key = (String) param.get("key");
                String valueExp = (String) param.get("value");
                Object value = context.getElEvaluator().eval(valueExp, null, context, null);
                paramMap.put(key, value);
            }
            context.getParam().setParam(paramMap);
        }
        return true;
    }

}

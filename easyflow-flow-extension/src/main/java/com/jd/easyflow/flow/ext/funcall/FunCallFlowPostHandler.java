package com.jd.easyflow.flow.ext.funcall;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowPostHandler;

/**
 * 
 * @author liyuliang5
 *
 */
public class FunCallFlowPostHandler implements FlowPostHandler {

    private List<Map<String, Object>> resultList;

    public FunCallFlowPostHandler(Map<String, Object> conf) {
        resultList = (List<Map<String, Object>>) conf.get("result");
    }

    @Override
    public void postHandle(FlowContext context) {
        if (resultList != null) {
            Map<String, Object> outputMap = new HashMap<>();
            Map<String, Object> contextMap = new HashMap<>();
            contextMap.put("node", context.get(FunCallConstants.CTX_NODE_ACTION_DATA_MAP));
            for (Map<String, Object> result : resultList) {
                String key = (String) result.get("key");
                String valueExp = (String) result.get("value");
                Object value = ElFactory.get().eval(valueExp, null, context, contextMap);
                outputMap.put(key, value);
            }
            context.getResult().setResult(outputMap);
        }

    }

}

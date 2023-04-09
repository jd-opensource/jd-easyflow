package com.jd.easyflow.flow.ext.funcall;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.tuple.Pair;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.util.FlowEventTypes;

/**
 * Function call event listener.
 * 
 * @author liyuliang5
 *
 */
public class FunCallEventListener implements FlowEventListener {

    private Pair<String, Integer>[] acceptTypes = new Pair[] { Pair.of(FlowEventTypes.FLOW_START, 0),
            Pair.of(FlowEventTypes.FLOW_END, 0), Pair.of(FlowEventTypes.FLOW_COMPLETE, 0) };

    private List<Map<String, Object>> paramList;

    private List<Map<String, Object>> resultList;

    public FunCallEventListener(Map<String, Object> conf) {
        paramList = (List<Map<String, Object>>) conf.get("param");
        resultList = (List<Map<String, Object>>) conf.get("result");
    }

    public Pair<String, Integer>[] getAcceptedEvents() {
        return acceptTypes;
    }

    @Override
    public void on(FlowEvent flowEvent) {
        FlowContext context = flowEvent.getContext();
        switch (flowEvent.getType()) {
        case FlowEventTypes.FLOW_START: {
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
                    Object value = ElFactory.get().eval(valueExp, null, context, null);
                    paramMap.put(key, value);
                }
                context.getParam().setParam(paramMap);
            }
            break;
        }
        case FlowEventTypes.FLOW_END: {
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
            break;
        }
        case FlowEventTypes.FLOW_COMPLETE: {
            // 最终处理（含异常）
            break;
        }

        }

    }

    public Pair<String, Integer>[] getAcceptTypes() {
        return acceptTypes;
    }

    public void setAcceptTypes(Pair<String, Integer>[] acceptTypes) {
        this.acceptTypes = acceptTypes;
    }

}

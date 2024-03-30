package com.jd.easyflow.flow.cases.action;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.event.FlowEvent;
import com.jd.easyflow.flow.engine.event.FlowEventListener;
import com.jd.easyflow.flow.util.FlowEventTypes;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public class MockInputOutputEventListener implements FlowEventListener {

    private List<Map<String, Object>> paramList;

    private List<Map<String, Object>> resultList;

    public MockInputOutputEventListener(Map<String, Object> conf) {
        paramList = (List<Map<String, Object>>) conf.get("param");
        resultList = (List<Map<String, Object>>) conf.get("result");
    }

    public Pair<String, Integer>[] getAcceptedEvents() {
        return new Pair[] { Pair.of(FlowEventTypes.FLOW_START, 0), Pair.of(FlowEventTypes.FLOW_END, 0),
                Pair.of(FlowEventTypes.FLOW_COMPLETE, 0) };
    }

    @Override
    public void on(FlowEvent flowEvent) {
        FlowContext context = flowEvent.getContext();
        switch (flowEvent.getType()) {
        case FlowEventTypes.FLOW_START: {
            // 节点的执行信息
            context.put("nodeActionInfoMap", new ConcurrentHashMap<String, Map>());

            Map<String, Object> paramMap = new HashMap<>();
            Map<String, Object> nodeActionMap = context.get("nodeActionInfoMap");
            Map<String, Object> contextMap = new HashMap<>();
            contextMap.put("node", nodeActionMap);
            for (Map<String, Object> param : paramList) {
                String key = (String) param.get("key");
                String valueExp = (String) param.get("value");
                String value = context.getElEvaluator().eval(valueExp, null, context, contextMap);
                paramMap.put(key, value);
            }
            context.getParam().setParam(paramMap);
            break;
        }
        case FlowEventTypes.FLOW_END: {
            Map<String, Object> outputMap = new HashMap<>();
            Map<String, Object> contextMap = new HashMap<>();
            contextMap.put("node", context.get("nodeActionInfoMap"));
            for (Map<String, Object> result : resultList) {
                String key = (String) result.get("key");
                String valueExp = (String) result.get("value");
                String value = context.getElEvaluator().eval(valueExp, null, context, contextMap);
                outputMap.put(key, value);
            }
            context.getResult().setResult(outputMap);
            break;
        }
        case FlowEventTypes.FLOW_COMPLETE: {
            // 最终处理（含异常）
            break;
        }

        }

    }

}

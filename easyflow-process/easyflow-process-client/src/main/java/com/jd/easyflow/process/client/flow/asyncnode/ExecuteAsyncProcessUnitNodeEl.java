package com.jd.easyflow.process.client.flow.asyncnode;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.ext.serialize.FlowParamSerializeManager;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 */
public class ExecuteAsyncProcessUnitNodeEl {
    
    private FlowEngine flowEngine;

    public ExecResult execute(ExecParam param) {
        Map<String, Object> contentMap = JSON.parseObject(param.getRequestContent(), Map.class);
        String flowParamStr = (String) contentMap.get("flowParamStr");
        String flowParamSerializeClas = (String) contentMap.get("flowParamSerializerClass");
        Map<String, Object> serializeConfig = (Map<String, Object>) contentMap.get("flowParamSerializeConfig");
        FlowParam flowParam = FlowParamSerializeManager.getInstance().deserialize(flowParamSerializeClas, flowParamStr, serializeConfig);
        FlowResult flowResult = flowEngine.execute(flowParam);
        ExecResult result = new ExecResult();
        result.setResult(ProcessUnitConstants.RESULT_SUCCESS);
        return result;
    }

    public FlowEngine getFlowEngine() {
        return flowEngine;
    }

    public void setFlowEngine(FlowEngine flowEngine) {
        this.flowEngine = flowEngine;
    }
    
    
}

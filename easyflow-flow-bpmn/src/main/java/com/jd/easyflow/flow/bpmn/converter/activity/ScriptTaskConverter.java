package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.HashMap;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.ScriptTask;
import org.apache.commons.lang3.StringUtils;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Script Task Converter.
 * @author liyuliang5
 *
 */
public class ScriptTaskConverter extends BaseFlowNodeConverter {
    
    private static final String EXP_FORMAT = "exp";
    //private static final String CREATE_EXP_FORMAT = "createExp";

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        ScriptTask scriptTask = (ScriptTask) flowNode;
        String format = scriptTask.getScriptFormat();
        String script = scriptTask.getScript();
        if (StringUtils.isNotEmpty(script)) {
            if (format == null) {
                format = EXP_FORMAT;
            }
            Map<String, Object> action = new HashMap<>();
            action.put(format, script);
            node.put(DefConstants.NODE_PROP_ACTION, action);
        }
        return node;
    }
}

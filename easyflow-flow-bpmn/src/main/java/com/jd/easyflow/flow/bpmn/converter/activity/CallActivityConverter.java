package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.HashMap;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.CallActivity;
import org.activiti.bpmn.model.FlowNode;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.util.FlowStringUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class CallActivityConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        CallActivity callActivity = (CallActivity) flowNode;
        String flowId = callActivity.getCalledElement();
        if (node.get(DefConstants.NODE_PROP_ACTION) == null) {
            Map<String, Object> action = new HashMap<>();
            action.put(DefConstants.COMMON_PROP_FLOW_ID, flowId);
            node.put(DefConstants.NODE_PROP_ACTION, action);
        } else {
            if (FlowStringUtil.isNotEmpty(flowId)) {
                Map<String, Object> properties = ConvertUtil.getMapValue(node, DefConstants.COMMON_PROP_PROPERTIES);
                properties.put(DefConstants.COMMON_PROP_FLOW_ID, flowId);
            }
        }
        return node;
    }
}


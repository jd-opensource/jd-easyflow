package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.HashMap;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.model.action.EventNodeAction;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Receive Task Converter.
 * 
 * @author liyuliang5
 *
 */
public class ReceiveTaskConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        if (node.get(DefConstants.NODE_PROP_ACTION) == null) {
            Map<String, Object> action = new HashMap<>();
            action.put(DefConstants.COMMON_PROP_TYPE, DefConstants.NODE_ACTION_TYPE_EVENT);
            node.put(DefConstants.NODE_PROP_ACTION, action);
        }
        return node;
    }
}
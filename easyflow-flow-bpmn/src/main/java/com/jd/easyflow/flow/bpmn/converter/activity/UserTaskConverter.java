package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.HashMap;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.model.action.EventNodeAction;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * User Task Converter.
 * @author liyuliang5
 *
 */
public class UserTaskConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        Map<String, Object> action = new HashMap<>();
        action.put(DefConstants.COMMON_PROP_CREATE_EXP, "new " + EventNodeAction.class.getName() + "()");
        node.put(DefConstants.NODE_PROP_ACTION, action);
        return node;
    }
}
package com.jd.easyflow.flow.bpmn.converter.event;

import java.util.HashMap;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.model.action.EventNodeAction;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Intermediate Catch Event Converter.
 * 
 * @author liyuliang5
 *
 */
public class IntermediateCatchEventConverter extends BaseFlowNodeConverter {

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
package com.jd.easyflow.flow.bpmn.converter.event;

import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Start Event Converter.
 * @author liyuliang5
 *
 */
public class StartEventConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        node.put(DefConstants.NODE_PROP_START, true);
        return node;
    }
}

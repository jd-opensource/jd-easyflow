package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

/**
 * Transaction converter
 * @author liyuliang5
 *
 */
public class TransactionConverter extends SubProcessConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        return node;
    }
}

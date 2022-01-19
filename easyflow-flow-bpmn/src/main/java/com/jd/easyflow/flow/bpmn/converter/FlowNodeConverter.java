package com.jd.easyflow.flow.bpmn.converter;

import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;

/**
 * 
 * @author liyuliang5
 *
 */
public interface FlowNodeConverter {
    
    /**
     * Do convert.
     * @param flowNode
     * @param bpmnModel
     * @param flowDef
     * @return
     */
    Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef);
}

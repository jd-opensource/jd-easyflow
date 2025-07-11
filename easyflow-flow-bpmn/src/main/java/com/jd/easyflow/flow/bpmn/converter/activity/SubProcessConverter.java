package com.jd.easyflow.flow.bpmn.converter.activity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.ExtensionElement;
import org.activiti.bpmn.model.FlowElement;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.SequenceFlow;
import org.activiti.bpmn.model.SubProcess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.flow.bpmn.converter.FlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.BpmnXmlConstants;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class SubProcessConverter extends BaseFlowNodeConverter {
    
    private static final Logger logger = LoggerFactory.getLogger(SubProcessConverter.class);


    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        SubProcess subProcess = (SubProcess) flowNode;
        Map<String, Object> subFlowDef = new HashMap<>();
        Map<String, List<ExtensionElement>> extensionElementMap = flowNode.getExtensionElements();
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.FLOW)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.FLOW).get(0);
            String elementText = element.getElementText();
            subFlowDef = JsonUtil.parseObject(elementText, Map.class);
        }
        if (subFlowDef.get(DefConstants.COMMON_PROP_ID) == null) {
            throw new FlowException("flow id of sub process is necessary!");
        }
        convertSubProcess(subProcess, bpmnModel, subFlowDef);
        
        if (node.get(DefConstants.NODE_PROP_ACTION) == null) {
            Map<String, Object> action = new HashMap<>();
            action.put(DefConstants.COMMON_PROP_FLOW, subFlowDef);
            node.put(DefConstants.NODE_PROP_ACTION, action);
        } else {
            if (subFlowDef != null && ! subFlowDef.isEmpty()) {
                // put to properties, customized use.
                Map<String, Object> properties = ConvertUtil.getMapValue(node, DefConstants.COMMON_PROP_PROPERTIES);
                properties.put(DefConstants.COMMON_PROP_FLOW, subFlowDef);
            }
        }
        return node;
    }
    
    /**
     * Convert process definition.
     * 
     * @param process
     * @param bpmnModel
     * @param flowDef
     */
    private static void convertSubProcess(SubProcess process, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        // Process flow element, convert Gateway,Event,Activity to Node.
        List<Map<String, Object>> nodeList = (List<Map<String, Object>>) flowDef.get(DefConstants.FLOW_PROP_NODES);
        if (nodeList != null) {
            return;
        }
        nodeList = new ArrayList<>();
        flowDef.put(DefConstants.FLOW_PROP_NODES, nodeList);
        for (FlowElement flowElement : process.getFlowElements()) {
            // Flow element
            if (flowElement instanceof FlowNode) {
                FlowNodeConverter nodeConverter = BpmnConverter.getFlowNodeConverterMap().get(flowElement.getClass());
                if (nodeConverter == null) {
                    throw new FlowException("Unsupported BPMN elmenet:ID" + flowElement.getId() + " TYPE:"
                            + flowElement.getClass().getCanonicalName());
                }
                Map<String, Object> node = nodeConverter.convert((FlowNode) flowElement, bpmnModel, flowDef);
                nodeList.add(node);
                // Sequence flow
            } else if (flowElement instanceof SequenceFlow) {
                continue;
            } else {
                throw new FlowException("Unsupported BPMN element:ID" + flowElement.getId() + " TYPE:"
                        + flowElement.getClass().getCanonicalName());
            }
        }
    }
}


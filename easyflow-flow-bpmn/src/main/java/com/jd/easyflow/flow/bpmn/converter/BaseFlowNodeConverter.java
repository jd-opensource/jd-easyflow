package com.jd.easyflow.flow.bpmn.converter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.activiti.bpmn.model.Activity;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.ExtensionElement;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.Gateway;
import org.activiti.bpmn.model.SequenceFlow;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.util.BpmnXmlConstants;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class BaseFlowNodeConverter implements FlowNodeConverter {
    
    private static final Logger logger = LoggerFactory.getLogger(BaseFlowNodeConverter.class);
    
    private static final String CONDITION_TYPE_CREATE_EXP = "createExp";
    private static final String CONDITION_TYPE_CREATE_EXP_PREFIX = CONDITION_TYPE_CREATE_EXP + ":";
    
    

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = new HashMap<>();
        // ID and Name
        node.put(DefConstants.COMMON_PROP_ID, flowNode.getId());
        if (StringUtils.isNotEmpty(flowNode.getName())) {
            node.put(DefConstants.COMMON_PROP_NAME, flowNode.getName());
        }
        // Properties
        Map<String, Object> properties = null;
        Map<String, List<ExtensionElement>> extensionElementMap = flowNode.getExtensionElements();
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.PROPERTIES)) {
            properties = ConvertUtil.getMapValue(node, DefConstants.COMMON_PROP_PROPERTIES);
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.PROPERTIES).get(0);
            String elementText = element.getElementText();
            try {
                Map<String, Object> map = JsonUtil.parseObject(elementText, Map.class);
                properties.putAll(map);
            } catch (Exception e) {
                throw new FlowException("Property JSON parse error, Node:" + flowNode.getId() + ", Property:" + elementText + "." + e.getMessage(), e);
            }
        }
        // Start
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.START)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.START).get(0);
            String elementText = element.getElementText();
            node.put(DefConstants.NODE_PROP_START, JsonUtil.parseObject(elementText, Boolean.class));
        }
        // Pre
        // self first
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.PRE)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.PRE).get(0);
            String elementText = element.getElementText();
            node.put(DefConstants.NODE_PROP_PRE, JsonUtil.parseObject(elementText, Map.class));
        }           
        // Action
        // self first
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.ACTION)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.ACTION).get(0);
            String elementText = element.getElementText();
            node.put(DefConstants.NODE_PROP_ACTION, JsonUtil.parseObject(elementText, Map.class));
        }        
        // Post
        // self first.
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.POST)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.POST).get(0);
            String elementText = element.getElementText();
            node.put(DefConstants.NODE_PROP_POST, JsonUtil.parseObject(elementText, Map.class));
        }  else {
            String defaultFlow = null;
            if (flowNode instanceof Activity) {
                defaultFlow = ((Activity) flowNode).getDefaultFlow();
            } else if (flowNode instanceof Gateway) {
                defaultFlow = ((Gateway) flowNode).getDefaultFlow();
            }
            List<SequenceFlow> sequenceFlowList = flowNode.getOutgoingFlows();
            if (sequenceFlowList.size() == 1) {
                Map<String, Object> post = new HashMap<>();
                node.put(DefConstants.NODE_PROP_POST, post);
                SequenceFlow sequenceFlow = sequenceFlowList.get(0);
                String conditionExp = sequenceFlow.getConditionExpression();
                if (StringUtils.isNotEmpty(conditionExp)) {
                    conditionExp = conditionExp.trim();
                    if (conditionExp.startsWith(CONDITION_TYPE_CREATE_EXP_PREFIX)) {
                        Map<String, Object> condition = new HashMap<String, Object>();
                        condition.put(CONDITION_TYPE_CREATE_EXP, conditionExp.substring(CONDITION_TYPE_CREATE_EXP_PREFIX.length()));
                    } else {
                        post.put(DefConstants.NODE_POST_PROP_WHEN, conditionExp);
                    }
                }
                post.put(DefConstants.NODE_POST_PROP_TO, sequenceFlow.getTargetRef());
            } else if (sequenceFlowList.size() > 1) {
                Map<String, Object> post = new HashMap<>();
                node.put(DefConstants.NODE_PROP_POST, post);
                List<Map<String, Object>> conditionList = new ArrayList<>();
                post.put(DefConstants.NODE_POST_PROP_CONDITIONS, conditionList);
                for (SequenceFlow sequenceFlow : sequenceFlowList) {
                    if (sequenceFlow.getId().equals(defaultFlow)) {
                        post.put(DefConstants.NODE_POST_PROP_DEFAULT_TO, sequenceFlow.getTargetRef());
                    } else {
                        Map<String, Object> condition = new HashMap<>();
                        if (StringUtils.isNotEmpty(sequenceFlow.getConditionExpression())) {
                            condition.put(DefConstants.NODE_POST_PROP_WHEN, sequenceFlow.getConditionExpression());
                        }
                        condition.put(DefConstants.NODE_POST_PROP_TO, sequenceFlow.getTargetRef());
                        conditionList.add(condition);
                    }
                }
    
                // conditionType
                String conditionType = ConditionalNodePostHandler.INCLUSIVE_TYPE;
                if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.CONDITION_TYPE)) {
                    conditionType = (String) (extensionElementMap.get(BpmnXmlConstants.CONDITION_TYPE)).get(0)
                            .getElementText();
                }
                if (!ConditionalNodePostHandler.EXCLUSIVE_TYPE.equals(conditionType)) {
                    post.put(DefConstants.NODE_POST_PROP_CONDITION_TYPE, conditionType);
                }
            }
        }
        return node;
    }

}

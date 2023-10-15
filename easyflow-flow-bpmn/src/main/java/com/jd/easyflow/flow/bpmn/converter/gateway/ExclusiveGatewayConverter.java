package com.jd.easyflow.flow.bpmn.converter.gateway;

import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.ExclusiveGateway;
import org.activiti.bpmn.model.FlowNode;
import org.apache.commons.lang3.StringUtils;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Exclusive Gateway Converter.
 * @author liyuliang5
 *
 */
public class ExclusiveGatewayConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        ExclusiveGateway exclusiveGateway = (ExclusiveGateway) flowNode;
        Map<String, Object> post = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_POST);
        post.remove(DefConstants.NODE_POST_PROP_CONDITION_TYPE);
        return node;
    }

}

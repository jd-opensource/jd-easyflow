package com.jd.easyflow.flow.bpmn.converter.gateway;

import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.InclusiveGateway;
import org.apache.commons.lang3.StringUtils;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;

/**
 * Inclusive Gateway Converter.
 * 
 * @author liyuliang5
 *
 */
public class InclusiveGatewayConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        InclusiveGateway inclusiveGateway = (InclusiveGateway) flowNode;

        Map<String, Object> post = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_POST);
        post.put(DefConstants.NODE_POST_PROP_CONDITION_TYPE, ConditionalNodePostHandler.INCLUSIVE_TYPE);
        if (StringUtils.isNotEmpty(inclusiveGateway.getDefaultFlow())) {
            post.put(DefConstants.NODE_POST_PROP_DEFAULT_TO, inclusiveGateway.getDefaultFlow());
        }
        // 入口包容网关暂不支持，后续可支持. 需1、节点中增加preInclusiveNodes属性
        // 2、新增StdMultiInclusiveCheckPreHandler 3、持久化监听器中增加前置检查能力
        if (inclusiveGateway.getIncomingFlows().size() > 1) {
            throw new FlowException("包容网关暂不支持多个入口流程, ID:" + flowNode.getId());
        }
        return node;
    }

}

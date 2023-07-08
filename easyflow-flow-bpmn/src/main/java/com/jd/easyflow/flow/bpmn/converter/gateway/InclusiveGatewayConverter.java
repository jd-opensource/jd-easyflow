package com.jd.easyflow.flow.bpmn.converter.gateway;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.InclusiveGateway;
import org.activiti.bpmn.model.SequenceFlow;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;

/**
 * Inclusive Gateway Converter.
 * 
 * @author liyuliang5
 *
 */
public class InclusiveGatewayConverter extends BaseFlowNodeConverter {

    private static final Logger logger = LoggerFactory.getLogger(InclusiveGatewayConverter.class);

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        InclusiveGateway inclusiveGateway = (InclusiveGateway) flowNode;

        Map<String, Object> post = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_POST);
        post.put(DefConstants.NODE_POST_PROP_CONDITION_TYPE, ConditionalNodePostHandler.INCLUSIVE_TYPE);
        if (StringUtils.isNotEmpty(inclusiveGateway.getDefaultFlow())) {
            post.put(DefConstants.NODE_POST_PROP_DEFAULT_TO, inclusiveGateway.getDefaultFlow());
        }
        // Inclusive gateway with multiple incoming flows is limited supported!
        // Currently only support this pattern: A(Inclusive gateway) -> (B、C、D...) -> E(Inclusive gateway), no checking.
        // User can implements other patterns by customize pre handler.
        if (inclusiveGateway.getIncomingFlows().size() > 1) {
            logger.warn("Inclusive gateway with multiple incomming flows is limited supported!");
            if (node.get(DefConstants.NODE_PROP_PRE) == null) {
                Map<String, Object> pre = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_PRE);
                List<String> preNodes = new ArrayList<>();
                flowNode.getIncomingFlows().forEach(incomingFlow -> preNodes.add(incomingFlow.getSourceRef()));
                pre.put(DefConstants.COMMON_PROP_TYPE, DefConstants.NODE_PRE_TYPE_INCLUSIVECHECK);
                pre.put(DefConstants.NODE_PRE_PROP_PRE_NODES, preNodes);
            }
        }
        return node;
    }

}

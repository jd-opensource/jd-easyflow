package com.jd.easyflow.flow.bpmn.converter.gateway;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.ParallelGateway;
import org.activiti.bpmn.model.SequenceFlow;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.post.ConditionalNodePostHandler;
import com.jd.easyflow.flow.model.pre.MultiCheckPreHandler;

/**
 * Parallel Gateway Converter.
 * 
 * @author liyuliang5
 *
 */
public class ParallelGatewayConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        ParallelGateway parallelGateway = (ParallelGateway) flowNode;
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        Map<String, Object> post = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_POST);
        post.put(DefConstants.NODE_POST_PROP_CONDITION_TYPE, ConditionalNodePostHandler.INCLUSIVE_TYPE);
        // Set pre nodes.
        List<SequenceFlow> list = parallelGateway.getIncomingFlows();
        if (list.size() == 0) {
            throw new FlowException("Parallel gateway:" + flowNode.getId() + " no incomming flows");
        }
        if (list.size() > 1) {
            if (node.get(DefConstants.NODE_PROP_PRE) == null) {
                Map<String, Object> pre = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_PRE);
                pre.put(DefConstants.COMMON_PROP_CREATE_EXP, "new " + MultiCheckPreHandler.class.getName() + "()");

                Map<String, Object> properties = ConvertUtil.getMapValue(node, DefConstants.COMMON_PROP_PROPERTIES);
                List<String> preNodes = new ArrayList<>();
                flowNode.getIncomingFlows().forEach(incomingFlow -> preNodes.add(incomingFlow.getSourceRef()));
                properties.put(DefConstants.NODE_PROPERTIES_PROP_PRE_NODES, preNodes);
            }
        }
        return node;
    }

}

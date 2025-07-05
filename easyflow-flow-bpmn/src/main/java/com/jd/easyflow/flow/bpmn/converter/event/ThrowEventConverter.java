package com.jd.easyflow.flow.bpmn.converter.event;

import java.util.List;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.CompensateEventDefinition;
import org.activiti.bpmn.model.EventDefinition;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.ThrowEvent;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * Throw event converter.
 * 
 * @author liyuliang5
 */
public class ThrowEventConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        List<EventDefinition> events = ((ThrowEvent) flowNode).getEventDefinitions();

        boolean isCompensateEvent = false;
        if (events != null) {
            for (EventDefinition definition : events) {
                if (definition instanceof CompensateEventDefinition) {
                    isCompensateEvent = true;
                }
            }
        }

        if (node.get(DefConstants.NODE_PROP_ACTION) == null) {
            if (isCompensateEvent) {
                Map<String, Object> action = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_ACTION);
                action.put(DefConstants.COMMON_PROP_TYPE, DefConstants.NODE_ACTION_TYPE_COMPENSATE);
            }
        }
        return node;
    }
}

package com.jd.easyflow.flow.bpmn.converter.event;

import java.util.List;
import java.util.Map;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.CompensateEventDefinition;
import org.activiti.bpmn.model.EndEvent;
import org.activiti.bpmn.model.EventDefinition;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.TerminateEventDefinition;

import com.jd.easyflow.flow.bpmn.converter.BaseFlowNodeConverter;
import com.jd.easyflow.flow.bpmn.converter.util.ConvertUtil;
import com.jd.easyflow.flow.model.definition.DefConstants;

/**
 * End Event Converter.
 * 
 * @author liyuliang5
 *
 */
public class EndEventConverter extends BaseFlowNodeConverter {

    @Override
    public Map<String, Object> convert(FlowNode flowNode, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        Map<String, Object> node = super.convert(flowNode, bpmnModel, flowDef);
        // Properties
        Map<String, Object> properties = ConvertUtil.getMapValue(node, DefConstants.COMMON_PROP_PROPERTIES);
        properties.put(DefConstants.NODE_PROPERTIES_PROP_END, true);

        List<EventDefinition> events = ((EndEvent) flowNode).getEventDefinitions();
        boolean isTerminateEvent = false;
        boolean isCompensateEvent = false;
        if (events != null) {
            for (EventDefinition definition : events) {
                if (definition instanceof TerminateEventDefinition) {
                    isTerminateEvent = true;
                } else if (definition instanceof CompensateEventDefinition) {
                    isCompensateEvent = true;
                }
            }
        }
        
        if (node.get(DefConstants.NODE_PROP_ACTION) == null) {
            if (isTerminateEvent) {
                Map<String, Object> action = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_ACTION);
                action.put(DefConstants.COMMON_PROP_TYPE, DefConstants.NODE_ACTION_TYPE_INTERRUPT);
            } else if (isCompensateEvent) {
                Map<String, Object> action = ConvertUtil.getMapValue(node, DefConstants.NODE_PROP_ACTION);
                action.put(DefConstants.COMMON_PROP_TYPE, DefConstants.NODE_ACTION_TYPE_COMPENSATE);
            }
        }

        return node;
    }
}

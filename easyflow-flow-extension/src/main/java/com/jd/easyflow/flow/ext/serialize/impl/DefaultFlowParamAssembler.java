package com.jd.easyflow.flow.ext.serialize.impl;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.ext.serialize.FlowParamAssembleData;
import com.jd.easyflow.flow.ext.serialize.FlowParamAssembler;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public class DefaultFlowParamAssembler implements FlowParamAssembler {

    @Override
    public FlowParam assemble(FlowParamAssembleData data, Map<String, Object> assembleConfig) {
        try {
            FlowParam flowParam = data.getFlowContext().getParam();
            FlowParam newFlowParam = flowParam.getClass().newInstance();
            newFlowParam.setFlowId(flowParam.getFlowId());
            newFlowParam.setLogFlag(flowParam.getLogFlag());
            newFlowParam.setParam(flowParam.getParam());
            newFlowParam.setDataMapFrom(flowParam);

            if (data.getNodeContext() != null) {
                newFlowParam.setNodeId(data.getNodeContext().getNodeId());
            } else {
                newFlowParam.setNodeIds(flowParam.getNodeIds());
            }
            // TODO add flowContext
            return newFlowParam;
        } catch (Exception e) {
            throw ExceptionUtil.throwException(e);
        }
    }

}

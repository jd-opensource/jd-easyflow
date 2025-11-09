package com.jd.easyflow.flow.ext.serialize.impl;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.ext.serialize.FlowParamSerializer;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public abstract class BaseFlowParamSerializer implements FlowParamSerializer {
    
    protected FlowParam createFlowParamToSerialize(FlowParam flowParam, Map<String, Object> config) {
        try {
            FlowParam newFlowParam = flowParam.getClass().newInstance();
            newFlowParam.setFlowId(flowParam.getFlowId());
            newFlowParam.setLogFlag(flowParam.getLogFlag());
            newFlowParam.setNodeIds(flowParam.getNodeIds());
            newFlowParam.setParam(flowParam.getParam());
            newFlowParam.setDataMapFrom(flowParam);

            if (flowParam.getContext() != null) {
                FlowContext flowContext = flowParam.getContext();
                FlowContext newFlowContext;

                newFlowContext = flowContext.getClass().newInstance();

                newFlowContext.setContext(flowContext.getContext());
                newFlowContext.getData().putAll(flowContext.getData());
                newFlowContext.setLogFlag(flowContext.getLogFlag());
                newFlowParam.setContext(newFlowContext);
            }
            return newFlowParam;
        } catch (InstantiationException | IllegalAccessException e) {
            throw ExceptionUtil.throwException(e);
        }
    }

}

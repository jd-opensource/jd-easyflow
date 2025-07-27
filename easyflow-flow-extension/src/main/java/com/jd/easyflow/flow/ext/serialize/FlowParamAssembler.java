package com.jd.easyflow.flow.ext.serialize;

import java.util.Map;

import com.jd.easyflow.flow.engine.FlowParam;

public interface FlowParamAssembler {
    
    public FlowParam assemble(FlowParamAssembleData data, Map<String, Object> assembleConfig);

}

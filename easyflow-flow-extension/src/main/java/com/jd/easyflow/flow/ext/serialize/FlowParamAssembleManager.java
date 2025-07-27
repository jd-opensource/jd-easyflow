package com.jd.easyflow.flow.ext.serialize;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public class FlowParamAssembleManager {
    
    private static final FlowParamAssembleManager INSTANCE  = new FlowParamAssembleManager();
    
    public static FlowParamAssembleManager getInstance() {
        return INSTANCE;
    }
    
    private Map<String, FlowParamAssembler> assemblerMap = new ConcurrentHashMap<String, FlowParamAssembler>();
    
    public FlowParamAssembler getAssembler(String className) {
        return assemblerMap.computeIfAbsent(className, key -> {
            try {
                return (FlowParamAssembler) Class.forName(className).newInstance();
            } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
                throw ExceptionUtil.throwException(e);
            }
        });
    }

    public FlowParam assemble(String assemblerClass, FlowParamAssembleData assembleData, Map<String, Object> assembleConfig) {
        FlowParamAssembler assembler = getAssembler(assemblerClass);
        FlowParam flowParam = assembler.assemble(assembleData, assembleConfig);
        return flowParam;
    }
    
}

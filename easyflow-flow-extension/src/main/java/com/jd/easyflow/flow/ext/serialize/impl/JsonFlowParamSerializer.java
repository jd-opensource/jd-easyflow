package com.jd.easyflow.flow.ext.serialize.impl;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.impl.LaissezFaireSubTypeValidator;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public class JsonFlowParamSerializer extends BaseFlowParamSerializer {
    
    private ObjectMapper mapper;
    {
        mapper = new ObjectMapper();
        mapper.activateDefaultTyping(LaissezFaireSubTypeValidator.instance, ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY);
        mapper.setVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.ANY);
    }

    @Override
    public String serialize(FlowParam flowParam, Map<String, Object> config) {
        FlowParam param = super.createFlowParamToSerialize(flowParam, config);
        try {
            String s = mapper.writeValueAsString(param);
            return s;
        } catch (JsonProcessingException e) {
            throw ExceptionUtil.throwException(e);
        }
    }
    
    @Override
    public FlowParam deserialize(String s, Map<String, Object> config) {
        try {
            FlowParam param = mapper.readValue(s, FlowParam.class);
            return param;
        } catch (JsonProcessingException e) {
            throw ExceptionUtil.throwException(e);
        }
    }
    
}

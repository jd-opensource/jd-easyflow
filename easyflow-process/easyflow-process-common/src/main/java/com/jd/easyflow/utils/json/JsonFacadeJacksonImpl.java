package com.jd.easyflow.utils.json;

import java.io.IOException;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * @author liyuliang5
 */
public class JsonFacadeJacksonImpl implements JsonFacade {


    private ObjectMapper mapper = new ObjectMapper();

    {
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.setSerializationInclusion(Include.NON_NULL);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }
    
    @Override
    public String toJSONString(Object o) {
        try {
            return mapper.writeValueAsString(o);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override
    public <T> T parseObject(Object object, Class<T> clazz) {
        String s = null;
        if (object instanceof String) {
            s = (String) object;
        } else {
            s = toJSONString(object);
        }
        try {
            return mapper.readValue(s, clazz);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public <T> List<T> parseArray(Object object, Class<T> clazz) {
        try {
            String src;
            if(object instanceof String) {
                src = (String) object;
            } else {
                src = toJSONString(object);
            }
            JavaType javaType = mapper.getTypeFactory().constructCollectionType(List.class, clazz);

            return mapper.readValue(src, javaType);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toJSONString(Object o, JsonSerializeConfig config) {
        ObjectMapper mapper = new ObjectMapper();
        mapper.setDateFormat(config.getDateFormat());
        try {
            return mapper.writeValueAsString(o);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override 
    public <T> T parseObject(Object src, TypeReference<T> typeReference) {
        try {
            if(src instanceof String) {
                return mapper.readValue((String)src, typeReference);
            } else {
                return mapper.readValue(toJSONString(src), typeReference);
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public ObjectMapper getMapper() {
        return mapper;
    }

    public void setMapper(ObjectMapper mapper) {
        this.mapper = mapper;
    }
    
}

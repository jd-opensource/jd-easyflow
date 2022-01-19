package com.jd.easyflow.flow.util;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * JSON Util.
 * 
 * @author liyuliang5
 *
 */
public class JsonUtil {

    private static ObjectMapper mapper = new ObjectMapper();

    static {
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.setSerializationInclusion(Include.NON_NULL);
    }

    public static String toJsonString(Object o) {
        return toJsonString(o, mapper);
    }

    public static String toJsonString(Object o, ObjectMapper mapper) {
        try {
            return mapper.writeValueAsString(o);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }
    
    public static String toPrettyJsonString(Object o) {
        return toPrettyJsonString(o, mapper);
    }
    
    public static String toPrettyJsonString(Object o, ObjectMapper mapper) {
        try {
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(o);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    public static <T> T parseObject(String s, Class<T> clazz) {
        return parseObject(s, clazz, mapper);
    }

    public static <T> T parseObject(String s, Class<T> clazz, ObjectMapper mapper) {
        try {
            return mapper.readValue(s, clazz);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}

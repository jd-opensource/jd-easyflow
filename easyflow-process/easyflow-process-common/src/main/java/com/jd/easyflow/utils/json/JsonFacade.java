package com.jd.easyflow.utils.json;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;

/**
 * @author liyuliang5
 */
public interface JsonFacade {

    public String toJSONString(Object o);
    
    public String toJSONString(Object o, JsonSerializeConfig config);

    public <T> T parseObject(Object object, Class<T> clazz);

    public <T> List<T> parseArray(Object object, Class<T> clazz);

    public <T> T parseObject(Object src, TypeReference<T> typeReference);
}

package com.jd.easyflow.flow.util;

/**
 * @author liyuliang5
 */
public interface JsonFacade {


    public String toJsonString(Object o);

    public <T> T parseObject(String s, Class<T> clazz);

}

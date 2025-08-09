package com.jd.easyflow.utils.json;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;

/**
 * 
 * @author liyuliang5
 *
 */
public class JSON {
    
    private static JsonFacade jsonFacade;

    public static String toJSONString(Object o) {
        return jsonFacade().toJSONString(o);
    }
	
    public static <T> T parseObject(Object object, Class<T> clazz) {
        return jsonFacade().parseObject(object, clazz);
    }

    public static <T> List<T> parseArray(Object object, Class<T> clazz) {
        return jsonFacade().parseArray(object, clazz);
    }

	public static String toJSONString(Object o, JsonSerializeConfig config) {
		return jsonFacade().toJSONString(o, config);
	}
	
	public static <T> T parseObject(Object src, TypeReference<T> typeReference) {
		return jsonFacade().parseObject(src, typeReference);
	}
	
	private static JsonFacade jsonFacade() {
	    if (jsonFacade == null) {
	        jsonFacade = new JsonFacadeJacksonImpl();
	    }
	    return jsonFacade;
	}

    public static JsonFacade getJsonFacade() {
        return jsonFacade;
    }

    public static void setJsonFacade(JsonFacade jsonFacade) {
        JSON.jsonFacade = jsonFacade;
    }
	
	

}
package com.jd.easyflow.fsm.util;

/**
 * JSON Util.
 * 
 * @author liyuliang5
 *
 */
public class JsonUtil {

    private static JsonFacade jsonFacade;

    public static String toJsonString(Object o) {
        return jsonFacade().toJsonString(o);
    }

    public static <T> T parseObject(String s, Class<T> clazz) {
        return jsonFacade().parseObject(s, clazz);
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
        JsonUtil.jsonFacade = jsonFacade;
    }
    

}

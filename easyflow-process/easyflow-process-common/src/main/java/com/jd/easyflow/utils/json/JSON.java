package com.jd.easyflow.utils.json;

import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * 
 * @author liyuliang5
 *
 */
public class JSON {
    
    private static final Logger logger = LoggerFactory.getLogger(JSON.class);

	private static ObjectMapper mapper = new ObjectMapper();

	static {
		initMapper(mapper);
	}
	
	private static void initMapper(ObjectMapper mapper) {
	    mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.setSerializationInclusion(Include.NON_NULL);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}

	public static String toJSONString(Object o) {
		return toJSONString(o, mapper);
	}

	public static String objectToJSONString(Object o){
		if (o instanceof String){
			return (String) o;
		}
		return toJSONString(o, mapper);
	}
	
    public static String toJSONString(Object o, boolean catchException) {
        if (!catchException) {
            return toJSONString(o);
        }
        try {
            String s = toJSONString(o);
            return s;
        } catch (Exception e) {
            logger.info("JSON serialize exception:" + e.getMessage());
            return null;
        }
    }

	public static String toJSONString(Object o, ObjectMapper mapper) {
		try {
			return mapper.writeValueAsString(o);
		} catch (JsonProcessingException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static String toJSONString(Object o, Class targetClass, Class mixinSource) {
	    ObjectMapper objectMapper = new ObjectMapper();
        initMapper(objectMapper);
        objectMapper.addMixIn(targetClass, mixinSource);
        return toJSONString(o, objectMapper);
	}
	
	public static <T> T parseObject(Object object, Class<T> clazz) {
		if(object instanceof String) {
			return parseObject((String)object, clazz, mapper);
		} else {
			return parseObject(toJSONString(object), clazz, mapper);
		}
	}

	public static <T> List<T> parseArray(Object object, Class<T> clazz) {

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

	public static <T> List<T> parseArrayNotReturnNull(Object object, Class<T> clazz) {

		List<T> resultList = parseArray(object, clazz);
		if(resultList == null) {
			return new ArrayList<>();
		}
		return resultList;
	}

	public static <T> T[] parseRealArray(Object object, Class<T> clazz) {
		List<T> resList = parseArray(object, clazz);
		if(resList == null) {
			return null;
		} else {
			return resList.toArray((T[]) Array.newInstance(clazz, resList.size()));
		}
	}

	public static <T> T[] parseRealArrayNotReturnNull(Object object, Class<T> clazz) {
		return (T[])parseArrayNotReturnNull(object, clazz).toArray();
	}

	public static <T> T parseObject(String s, Class<T> clazz, ObjectMapper mapper) {
		try {
			return mapper.readValue(s, clazz);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static <T> T parseObject(Object src, TypeReference<T> typeReference) {
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

	public static <T> T parseObject(Object src, Type type) {
		try {
			JavaType javaType = mapper.getTypeFactory().constructType(type);
			if(src instanceof String) {
				return mapper.readValue((String)src, javaType);
			} else {
				return mapper.readValue(toJSONString(src), javaType);
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}


}
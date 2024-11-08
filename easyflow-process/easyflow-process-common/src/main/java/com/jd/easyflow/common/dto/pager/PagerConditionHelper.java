package com.jd.easyflow.common.dto.pager;

import java.util.Arrays;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class PagerConditionHelper {

    public static PagerCondition setValueType(PagerCondition condition, String fieldName, Class<?> clazz) {
        FieldEntry fieldEntry = condition.getField(fieldName);
        if (fieldEntry == null) {
            return condition;
        }
        Object value = fieldEntry.getValue();
        if (value == null) {
            return condition;
        }
        if (clazz == String[].class) {
           if (value instanceof String[]) {
               return condition;
           }
           fieldEntry.setValue(new String[] {String.valueOf(value)});
           return condition;
        } else if (clazz == List.class) {
            if (value instanceof List) {
                return condition;
            } else if (value instanceof String[]) {
                List<String> list = Arrays.asList((String[]) value);
                fieldEntry.setValue(list);
            } else {
                fieldEntry.setValue(Arrays.asList(value));
            }
            return condition;
        }
        throw new UnsupportedOperationException("Unsupported class:" + clazz.getName());
    }
    
    public static <T>T getValue(PagerCondition condition, String fieldName) {
        FieldEntry entry = condition.getField(fieldName);
        if (entry == null) {
            return null;
        }
        return (T) entry.getValue();
    }
    
    public static String getStringValue(PagerCondition condition, String fieldName) {
        return (String) getValue(condition, fieldName);
    }
}

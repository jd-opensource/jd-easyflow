package com.jd.easyflow.common.util;

import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.helpers.MessageFormatter;

import com.jd.easyflow.common.exception.EasyFlowException;

@FunctionalInterface
public interface IAssertUtils {

    IAssertUtils DEFAULT = (e, p) ->{
        throw new EasyFlowException(CommonErrorCode.E0000001.getCode(), MessageFormatter.arrayFormat(e, p).getMessage());
    };

    void error(String message, Object... params);

    default void isTrue(boolean expression, String message, Object... params) {
        if (!expression) {
            error(message, params);
        }
    }

    default void isTrue(boolean expression) {
        isTrue(expression, "this expression must be true");
    }

    default void isFalse(boolean expression, String message, Object... params) {
        if (expression) {
            error(message, params);
        }
    }

    default void isFalse(boolean expression) {
        isFalse(expression, "this expression must be true");
    }

    default void isNull(Object object, String message, Object... params) {
        if (object != null) {
            error(message, params);
        }
    }

    default void isNull(Object object) {
        isNull(object, "the object argument must be null");
    }

    default void isNotNull(Object object, String message, Object... params) {
        if (object == null) {
            error(message, params);
        }
    }

    default void isNotNull(Object object) {
        isNotNull(object, "this argument is required; it must not be null");
    }


    default void isBlank(String text, String message, Object... params) {
        if (!StringUtils.isBlank(text)) {
            error(message, params);
        }
    }

    default void isBlank(String text) {
        isBlank(text, "this String argument must have length; it must be null or empty");
    }

    default void isNotBlank(String text, String message, Object... params) {
        if (StringUtils.isBlank(text)) {
            error(message, params);
        }
    }

    default void isNotBlank(String text) {
        isNotBlank(text, "this String argument must have length; it must not be null or empty");
    }

    default void isContain(String sourceString, String subString, String message, Object... params) {
        if (StringUtils.isNotBlank(sourceString) && StringUtils.isNotBlank(subString) &&
                !sourceString.contains(subString)) {
            error(message, params);
        }
    }

    default void isContain(String sourceString, String subString) {
        isContain(sourceString, subString,
                "this String argument must contain the substring [{}]", subString);
    }

    default void isNotContain(String sourceString, String subString, String message, Object... params) {
        if (StringUtils.isNotBlank(sourceString) && StringUtils.isNotBlank(subString) &&
                sourceString.contains(subString)) {
            error(message, params);
        }
    }

    default void isNotContain(String sourceString, String subString) {
        isNotContain(sourceString, subString,
                "this String argument must not contain the substring [{}]", subString);
    }

    default void isEmpty(Object[] array, String message, Object... params) {
        if (array != null && array.length > 0) {
            error(message, params);
        }
    }

    default void isEmpty(Object[] array) {
        isEmpty(array, "this array must be empty");
    }

    default void isNotEmpty(Object[] array, String message, Object... params) {
        if (array == null || array.length == 0) {
            error(message, params);
        }
    }

    default void isNotEmpty(Object[] array) {
        isNotEmpty(array, "this array must not be empty: it must contain at least 1 element");
    }

    default void isNoNullElements(Object[] array, String message, Object... params) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    error(message, params);
                }
            }
        }
    }

    default void isNoNullElements(Object[] array) {
        isNoNullElements(array, "this array must not contain any null elements");
    }

    default void isEmpty(Collection collection, String message, Object... params) {
        if (collection != null && ! collection.isEmpty()) {
            error(message, params);
        }
    }

    default void isEmpty(Collection collection) {
        isEmpty(collection,
                "this collection must  be empty");
    }

    default void isNotEmpty(Collection collection, String message, Object... params) {
        if (collection == null || collection.isEmpty()) {
            error(message, params);
        }
    }

    default void isNotEmpty(Collection collection) {
        isNotEmpty(collection,
                "this collection must not be empty: it must contain at least 1 element");
    }

    default void isEmpty(Map map, String message, Object... params) {
        if (map != null && ! map.isEmpty()) {
            error(message, params);
        }
    }

    default void isEmpty(Map map) {
        isEmpty(map, "this map must  be empty");
    }

    default void isNotEmpty(Map map, String message, Object... params) {
        if (map == null || map.isEmpty()) {
            error(message, params);
        }
    }

    default void isNotEmpty(Map map) {
        isNotEmpty(map, "this map must not be empty; it must contain at least one entry");
    }


    default void isAssignable(Class<?> superType, Class<?> subType) {
        isAssignable(superType, subType, "");
    }

    default void isAssignable(Class<?> superType, Class<?> subType, String message) {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            throw new IllegalArgumentException(message + subType + " is not assignable to " + superType);
        }
    }

    default void state(boolean expression, String message) {
        if (!expression) {
            throw new IllegalStateException(message);
        }
    }

    default void state(boolean expression) {
        state(expression, "[Assertion failed] - this state invariant must be true");
    }
}

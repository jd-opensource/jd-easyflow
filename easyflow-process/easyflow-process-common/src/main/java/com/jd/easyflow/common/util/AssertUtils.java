package com.jd.easyflow.common.util;

import java.util.Collection;
import java.util.Map;

import org.slf4j.helpers.MessageFormatter;

import com.jd.easyflow.common.exception.EasyFlowException;

public class AssertUtils {

    public static IAssertUtils TIPS = (e, p) ->{
        throw new EasyFlowException(CommonErrorCode.E0000001.getCode(), MessageFormatter.arrayFormat(e, p).getMessage());
    };

    public static void isTrue(boolean expression, String message, Object... params) {
        IAssertUtils.DEFAULT.isTrue(expression, message, params);
    }

    public static void isTrue(boolean expression) {
        IAssertUtils.DEFAULT.isTrue(expression);
    }

    public static void isFalse(boolean expression, String message, Object... params) {
        IAssertUtils.DEFAULT.isFalse(expression, message, params);
    }

    public static void isFalse(boolean expression) {
        IAssertUtils.DEFAULT.isFalse(expression);
    }

    public static void isNull(Object object, String message, Object... params) {
        IAssertUtils.DEFAULT.isNull(object, message, params);
    }

    public static void isNull(Object object) {
        IAssertUtils.DEFAULT.isNull(object);
    }

    public static void isNotNull(Object object, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotNull(object, message, params);
    }

    public static void isNotNull(Object object) {
        IAssertUtils.DEFAULT.isNotNull(object);
    }

    public static void isBlank(String text, String message, Object... params) {
        IAssertUtils.DEFAULT.isBlank(text, message, params);
    }

    public static void isBlank(String text) {
        IAssertUtils.DEFAULT.isBlank(text);
    }


    public static void isNotBlank(String text, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotBlank(text, message, params);
    }

    public static void isNotBlank(String text) {
        IAssertUtils.DEFAULT.isNotBlank(text);
    }

    public static void isContain(String sourceString, String subString, String message, Object... params) {
        IAssertUtils.DEFAULT.isContain(sourceString, subString, message, params);
    }

    public static void isContain(String sourceString, String subString) {
        IAssertUtils.DEFAULT.isContain(sourceString, subString);
    }

    public static void isNotContain(String sourceString, String subString, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotContain(sourceString, subString, message, params);
    }

    public static void isNotContain(String sourceString, String subString) {
        IAssertUtils.DEFAULT.isNotContain(sourceString, subString);
    }

    public static void isEmpty(Object[] array, String message, Object... params) {
        IAssertUtils.DEFAULT.isEmpty(array, message, params);
    }

    public static void isEmpty(Object[] array) {
        IAssertUtils.DEFAULT.isEmpty(array);
    }

    public static void isNotEmpty(Object[] array, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotEmpty(array, message, params);
    }

    public static void isNotEmpty(Object[] array) {
        IAssertUtils.DEFAULT.isNotEmpty(array);
    }

    public static void isNoNullElements(Object[] array, String message, Object... params) {
        IAssertUtils.DEFAULT.isNoNullElements(array, message, params);
    }

    public static void isNoNullElements(Object[] array) {
        IAssertUtils.DEFAULT.isNoNullElements(array);
    }

    public static void isEmpty(Collection collection, String message, Object... params) {
        IAssertUtils.DEFAULT.isEmpty(collection, message, params);
    }

    public static void isEmpty(Collection collection) {
        IAssertUtils.DEFAULT.isEmpty(collection);
    }

    public static void isNotEmpty(Collection collection, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotEmpty(collection, message, params);
    }

    public static void isNotEmpty(Collection collection) {
        IAssertUtils.DEFAULT.isNotEmpty(collection);
    }

    public static void isEmpty(Map map, String message, Object... params) {
        IAssertUtils.DEFAULT.isEmpty(map, message, params);
    }

    public static void isEmpty(Map map) {
        IAssertUtils.DEFAULT.isEmpty(map);
    }

    public static void isNotEmpty(Map map, String message, Object... params) {
        IAssertUtils.DEFAULT.isNotEmpty(map, message, params);
    }

    public static void isNotEmpty(Map map) {
        IAssertUtils.DEFAULT.isNotEmpty(map);
    }


    public static void isAssignable(Class<?> superType, Class<?> subType) {
        IAssertUtils.DEFAULT.isAssignable(superType, subType);
    }

    public static void isAssignable(Class<?> superType, Class<?> subType, String message) {
        IAssertUtils.DEFAULT.isAssignable(superType, subType, message);
    }

    public static void state(boolean expression, String message) {
        IAssertUtils.DEFAULT.state(expression, message);
    }

    public static void state(boolean expression) {
        IAssertUtils.DEFAULT.state(expression);
    }
}

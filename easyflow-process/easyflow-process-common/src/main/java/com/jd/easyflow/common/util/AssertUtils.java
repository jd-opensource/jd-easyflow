package com.jd.easyflow.common.util;

import org.slf4j.helpers.MessageFormatter;

import com.jd.easyflow.common.exception.UserException;

public class AssertUtils {

    public static IAssertUtils TIPS = (e, p) ->{
        throw new UserException(CommonErrorCode.E0000001.getCode(), MessageFormatter.arrayFormat(e, p).getMessage());
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


}

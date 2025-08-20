package com.jd.easyflow.common.util;

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

}

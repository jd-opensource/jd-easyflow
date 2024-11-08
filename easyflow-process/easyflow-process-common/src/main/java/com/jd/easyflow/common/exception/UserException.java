package com.jd.easyflow.common.exception;

import com.jd.easyflow.common.util.CommonErrorCode;

/**
 * @author liyuliang5
 *
 */
public class UserException extends EasyFlowException {

    public UserException(Throwable cause, String code, String info, Object... objects) {
        super(cause, null == code ? CommonErrorCode.E0000001.getCode() : code, info, objects);
    }

    public UserException(String code, String info) {
        this(null, code, info);
    }

    public UserException(String info) {
        this(CommonErrorCode.E0000001.getCode(), info);
    }

    public UserException(Throwable cause) {
        this(cause, null, null);
    }

    public UserException(String info, Throwable cause) {
        this(cause, null, info);
    }

    public UserException(String code, String info, Throwable cause) {
        this(cause, code, info);
    }

}
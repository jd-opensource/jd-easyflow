package com.jd.easyflow.process.client.runtime.core;

/**
 * 
 * @author liyuliang5
 * 
 */
public class ProcessException extends RuntimeException {

    private String code;

    private String info;

    private Object data;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public ProcessException(Throwable cause, String code, String info, Object... objects) {
        super((code == null ? "" : code + " ")
                + (info != null && objects != null && objects.length > 0 ? String.format(info, objects) : info), cause);
        this.code = code;
        this.info = info != null && objects != null && objects.length > 0 ? String.format(info, objects) : info;
    }

    public ProcessException(String code, String info) {
        this(null, code, info);
    }

    public ProcessException(String info) {
        this(null, info);
    }

    public ProcessException(Throwable cause) {
        this(cause, null, null);
    }

    public ProcessException(String info, Throwable cause) {
        this(cause, null, info);
    }

    public ProcessException(String code, String info, Throwable cause) {
        this(cause, code, info);
    }

    public String getInfo() {
        return info;
    }

    public void setInfo(String info) {
        this.info = info;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }

}

package com.jd.easyflow.common.exception;

/**
 * @author liyuliang5
 *
 */
public class EasyFlowException extends RuntimeException {
    
    private static final long serialVersionUID = 3772032165336446312L;

    private static final String DEFAULT_ERROR_CODE = "9999999";

    private String code;

    private String info;
    
    private Object data;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public EasyFlowException(Throwable cause, String code, String info, Object... objects) {
        super((code == null ? DEFAULT_ERROR_CODE : code + " ")
                + (info != null && objects != null && objects.length > 0 ? String.format(info, objects) : info), cause);
        this.code = code == null ? DEFAULT_ERROR_CODE : code;
        this.info = info != null && objects != null && objects.length > 0 ? String.format(info, objects) : info;
    }

    public EasyFlowException(String code, String info) {
        this(null, code, info);
    }

    public EasyFlowException(String info) {
        this(null, info);
    }

    public EasyFlowException(Throwable cause) {
        this(cause, null, null);
    }

    public EasyFlowException(String info, Throwable cause) {
        this(cause, null, info);
    }

    public EasyFlowException(String code, String info, Throwable cause) {
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



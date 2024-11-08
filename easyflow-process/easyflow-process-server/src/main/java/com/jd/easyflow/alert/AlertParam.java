package com.jd.easyflow.alert;

import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class AlertParam {

    private String alertCode;
    
    private String[] alertKeys;
    
    private String message;
    
    private String productCode;
    
    private Throwable throwable;
    
    private Object alertObject;
    
    private Map<String, Object> data;

    public String getAlertCode() {
        return alertCode;
    }

    public void setAlertCode(String alertCode) {
        this.alertCode = alertCode;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Throwable getThrowable() {
        return throwable;
    }

    public void setThrowable(Throwable throwable) {
        this.throwable = throwable;
    }

    public Object getAlertObject() {
        return alertObject;
    }

    public void setAlertObject(Object alertObject) {
        this.alertObject = alertObject;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    public String[] getAlertKeys() {
        return alertKeys;
    }

    public void setAlertKeys(String[] alertKeys) {
        this.alertKeys = alertKeys;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

}

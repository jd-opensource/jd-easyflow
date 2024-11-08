 package com.jd.easyflow.common.app.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 * 
 */
public class AppResponse <T> implements Serializable {

    private static final long serialVersionUID = -4256209410226007408L;

    private String resultCode;

    private String resultMsg;

    private T resultData;

    private Object extData;

    public AppResponse() {
    }

    public AppResponse(String resultCode, String resultMsg) {
        this.resultCode = resultCode;
        this.resultMsg = resultMsg;
    }

    public String getResultCode() {
        return resultCode;
    }

    public void setResultCode(String resultCode) {
        this.resultCode = resultCode;
    }

    public String getResultMsg() {
        return resultMsg;
    }

    public void setResultMsg(String resultMsg) {
        this.resultMsg = resultMsg;
    }

    public T getResultData() {
        return resultData;
    }

    public void setResultData(T resultData) {
        this.resultData = resultData;
    }

    public Object getExtData() {
        return extData;
    }

    public void setExtData(Object extData) {
        this.extData = extData;
    }

    @Override
    public String toString() {
        return "AppResponse [resultCode=" + resultCode + ", resultMsg=" + resultMsg + ", resultData=" + resultData
                + ", extData=" + extData + "]";
    }
    
    

}

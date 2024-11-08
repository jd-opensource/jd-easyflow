package com.jd.easyflow.common.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class BaseResponse implements Serializable {


    protected String resultCode;

    protected String resultMsg;
    
    public BaseResponse() {
    }
    
    public BaseResponse(String resultCode, String resultMsg) {
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

    @Override
    public String toString() {
        return "BaseResponse [resultCode=" + resultCode + ", resultMsg=" + resultMsg + "]";
    }
    
    
}

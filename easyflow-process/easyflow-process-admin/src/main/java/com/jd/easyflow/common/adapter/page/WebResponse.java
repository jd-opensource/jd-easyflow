package com.jd.easyflow.common.adapter.page;

/**
* 
* @author liyuliang5
*/
public class WebResponse<T> {

    private String resultCode;

    private String resultMsg;

    private T resultData;

    private Object extData;

    public WebResponse() {
    }
    
    public WebResponse(String resultCode, String resultMsg) {
        this.resultCode = resultCode;
        this.resultMsg = resultMsg;
    }


    public WebResponse(String resultCode, String resultMsg,T resultData) {
        this.resultCode = resultCode;
        this.resultMsg = resultMsg;
        this.resultData = resultData;
    }


    public static <T> WebResponse buildResponse(String resultCode, String resultMsg, T resultData){
        return new WebResponse(resultCode,resultMsg,resultData);
    }


    public static <T> WebResponse buildResponse(String resultCode, String resultMsg){
        return new WebResponse(resultCode,resultMsg);
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
        return "WebResponse [resultCode=" + resultCode + ", resultMsg=" + resultMsg + ", resultData=" + resultData
                + ", extData=" + extData + "]";
    }
    
    
}

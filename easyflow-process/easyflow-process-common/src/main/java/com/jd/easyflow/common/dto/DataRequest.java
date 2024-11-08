package com.jd.easyflow.common.dto;

import java.util.Map;

/**
* 
* @author liyuliang5
*/
public class DataRequest<T> extends BaseRequest {

    private T requestData;
    
    private Map<String, Object> extData;

    public T getRequestData() {
        return requestData;
    }

    public void setRequestData(T requestData) {
        this.requestData = requestData;
    }

    public Map<String, Object> getExtData() {
        return extData;
    }

    public void setExtData(Map<String, Object> extData) {
        this.extData = extData;
    }

    @Override
    public String toString() {
        return "DataRequest [requestData=" + requestData + ", extData=" + extData + "]";
    }
    
    
}

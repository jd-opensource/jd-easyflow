package com.jd.easyflow.common.dto;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.common.util.CommonErrorCode;

/**
 * 
 * 
 * @author liyuliang5
 */
public class DataResponse<T> extends BaseResponse {
    private T resultData;
    private Map<String, Object> extData;
    
    public DataResponse() {
        
    }

    public DataResponse(String resultCode, String resultMsg, T resultData) {
        this.resultCode = resultCode;
        this.resultMsg = resultMsg;
        this.resultData = resultData;
    }

    public DataResponse(T resultData) {
        this.resultData = resultData;
    }

    public static <T> DataResponse buildSuccessResponse(T t) {
        return new DataResponse(CommonErrorCode.E0000000.getCode(), "SUCCESS", t);
    }
    
    public void addExtData(String key, Object value) {
        if (extData == null) {
            extData = new HashMap<>();
        }
        extData.put(key, value);
    }
    
    public <T>T getExtData(String key) {
        if (extData == null) {
            return null;
        }
        return (T) extData.get(key);
    }

    public T getResultData() {
        return resultData;
    }

    public void setResultData(T resultData) {
        this.resultData = resultData;
    }

    public Map<String, Object> getExtData() {
        return extData;
    }

    public void setExtData(Map<String, Object> extData) {
        this.extData = extData;
    }

    @Override
    public String toString() {
        return "DataResponse [resultData=" + resultData + ", extData=" + extData + "]";
    }
    
    
}

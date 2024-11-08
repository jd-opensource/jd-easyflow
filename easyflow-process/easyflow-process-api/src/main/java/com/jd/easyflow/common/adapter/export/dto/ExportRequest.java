package com.jd.easyflow.common.adapter.export.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ExportRequest<T> implements Serializable {

    private static final long serialVersionUID = 1L;

    protected String reqNo;

     protected Map<String, Object> ext;

    protected String requestTime;

    protected T data;
    
    public ExportRequest() {
        
    }
    
    public ExportRequest(T data) {
        this.data = data;
    }

    public String getReqNo() {
        return reqNo;
    }

    public void setReqNo(String reqNo) {
        this.reqNo = reqNo;
    }

    public Map<String, Object> getExt() {
        return ext;
    }

    public void setExt(Map<String, Object> ext) {
        this.ext = ext;
    }

    public String getRequestTime() {
        return requestTime;
    }

    public void setRequestTime(String requestTime) {
        this.requestTime = requestTime;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return "ExportRequest [reqNo=" + reqNo + ", ext=" + ext + ", requestTime=" + requestTime + ", data=" + data
                + "]";
    }
    
    
}

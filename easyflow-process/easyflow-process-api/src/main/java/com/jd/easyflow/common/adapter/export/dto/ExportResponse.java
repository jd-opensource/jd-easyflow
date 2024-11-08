package com.jd.easyflow.common.adapter.export.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class ExportResponse<T> implements Serializable {
	private static final long serialVersionUID = 1L;

    private String resCode;

    private String resDesc;

    private Map<String, Object> ext;

    private T data;

    public ExportResponse() {
    }

    public ExportResponse(String resCode, String resDesc) {
        this.resCode = resCode;
        this.resDesc = resDesc;
    }


    public static ExportResponse build4Failed(String errorCode, String errorMessage) {
        ExportResponse res = new ExportResponse();
        res.setResCode(errorCode);
        res.setResDesc(errorMessage);
        return res;
    }


    public static ExportResponse build4Failed(ExportResponseCode error) {
        return build4Failed(error.getCode(),error.getDesc());
    }

    public static ExportResponse build4Success() {
        ExportResponse res = new ExportResponse();
        res.setResCode(ExportResponseCode.SUCCESS.getCode());
        res.setResDesc(ExportResponseCode.SUCCESS.getDesc());
        return res;
    }

    public static  <T> ExportResponse build4Success(T data) {
        ExportResponse res = new ExportResponse();
        res.setResCode(ExportResponseCode.SUCCESS.getCode());
        res.setResDesc(ExportResponseCode.SUCCESS.getDesc());
        res.setData(data);
        return res;
    }

    public boolean isSuccess() {
        return ExportResponseCode.SUCCESS.getCode().equals(this.getResCode());
    }

    public String getResCode() {
        return resCode;
    }

    public void setResCode(String resCode) {
        this.resCode = resCode;
    }

    public String getResDesc() {
        return resDesc;
    }

    public void setResDesc(String resDesc) {
        this.resDesc = resDesc;
    }

    public Map<String, Object> getExt() {
        return ext;
    }

    public void setExt(Map<String, Object> ext) {
        this.ext = ext;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return "ExportResponse [resCode=" + resCode + ", resDesc=" + resDesc + ", ext=" + ext + ", data=" + data + "]";
    }
    
    

}

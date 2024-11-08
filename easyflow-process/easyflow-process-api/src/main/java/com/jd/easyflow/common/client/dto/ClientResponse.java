package com.jd.easyflow.common.client.dto;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;

/**
 * @author liyuliang5
 *
 */
public class ClientResponse<T> implements Serializable {
    private static final long serialVersionUID = 1L;

    private String resCode;

    private String resDesc;

    private Map<String, Object> ext;

    private T data;

    public ClientResponse() {
    }

    public ClientResponse(String resCode, String resDesc) {
        this.resCode = resCode;
        this.resDesc = resDesc;
    }

    public static ClientResponse build4Failed(String errorCode, String errorMessage) {
        return build4Failed(errorCode, errorMessage, null);
    }

    public static ClientResponse build4Failed(ExportResponseCode error) {
        return build4Failed(error.getCode(), error.getDesc(), null);
    }

    public static ClientResponse build4Failed(String errorCode, String errorMessage, Map<String, Object> map) {
        ClientResponse res = new ClientResponse();
        res.setResCode(errorCode);
        res.setResDesc(errorMessage);
        if (!Objects.isNull(map) && map.size() > 0) {
            res.setExt(map);
        }
        return res;
    }

    public static ClientResponse build4Success() {
        ClientResponse res = new ClientResponse();
        res.setResCode(ClientResponseCode.SUCCESS.getCode());
        res.setResDesc(ClientResponseCode.SUCCESS.getDesc());
        return res;
    }

    public static <T> ClientResponse build4Success(T data) {
        ClientResponse res = new ClientResponse();
        res.setResCode(ClientResponseCode.SUCCESS.getCode());
        res.setResDesc(ClientResponseCode.SUCCESS.getDesc());
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
        return "ClientResponse [resCode=" + resCode + ", resDesc=" + resDesc + ", ext=" + ext + ", data=" + data + "]";
    }
    
    

}

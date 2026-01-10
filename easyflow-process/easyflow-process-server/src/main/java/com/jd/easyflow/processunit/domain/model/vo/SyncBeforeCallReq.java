 package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Map;

/**
  * @author liyuliang5
  * 
  */
 public class SyncBeforeCallReq {

     private String unitCode;
     
     private String bizNo;
     
     private String requestNo;
     
     private String requestContent;

     private String version;
     
     private String productCode;
     
     private Map<String, String> clientInfo;

    public String getUnitCode() {
        return unitCode;
    }

    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
    }

    public String getRequestNo() {
        return requestNo;
    }

    public void setRequestNo(String requestNo) {
        this.requestNo = requestNo;
    }

    public String getRequestContent() {
        return requestContent;
    }

    public void setRequestContent(String requestContent) {
        this.requestContent = requestContent;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }

    @Override
    public String toString() {
        return "SyncBeforeCallReq [unitCode=" + unitCode + ", bizNo=" + bizNo + ", requestNo=" + requestNo
                + ", requestContent=" + requestContent + ", version=" + version + ", productCode=" + productCode
                + ", clientInfo=" + clientInfo + "]";
    }
     
     
}

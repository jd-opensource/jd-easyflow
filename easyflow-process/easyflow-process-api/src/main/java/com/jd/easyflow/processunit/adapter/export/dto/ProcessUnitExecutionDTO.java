package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 */
public class ProcessUnitExecutionDTO implements Serializable {


    private String executionNo;
    
    private String requestNo;
    
    private String parentNo;
    
    private String instanceNo;
    
    private String processUnitCode;
    
    private String bizNo;
    
    private String productCode;
    
    private String result;

    private String requestContent;
    
    private String responseContent;
    
    private String execType;
         
    private String extData;
         
    private Date requestTime;
    
    private Date responseTime;
    
    private int elaspeTime;
    
    private Date createdDate;
    
    private Date modifiedDate;
    
    private Long id;

   public String getExecutionNo() {
       return executionNo;
   }

   public void setExecutionNo(String executionNo) {
       this.executionNo = executionNo;
   }

   public String getRequestNo() {
       return requestNo;
   }

   public void setRequestNo(String requestNo) {
       this.requestNo = requestNo;
   }

   public String getParentNo() {
       return parentNo;
   }

   public void setParentNo(String parentNo) {
       this.parentNo = parentNo;
   }

   public String getInstanceNo() {
       return instanceNo;
   }

   public void setInstanceNo(String instanceNo) {
       this.instanceNo = instanceNo;
   }

   public String getProcessUnitCode() {
       return processUnitCode;
   }

   public void setProcessUnitCode(String processUnitCode) {
       this.processUnitCode = processUnitCode;
   }

   public String getBizNo() {
       return bizNo;
   }

   public void setBizNo(String bizNo) {
       this.bizNo = bizNo;
   }

   public String getProductCode() {
       return productCode;
   }

   public void setProductCode(String productCode) {
       this.productCode = productCode;
   }

   public String getResult() {
       return result;
   }

   public void setResult(String result) {
       this.result = result;
   }

   public String getRequestContent() {
       return requestContent;
   }

   public void setRequestContent(String requestContent) {
       this.requestContent = requestContent;
   }

   public String getResponseContent() {
       return responseContent;
   }

   public void setResponseContent(String responseContent) {
       this.responseContent = responseContent;
   }

   public String getExecType() {
       return execType;
   }

   public void setExecType(String execType) {
       this.execType = execType;
   }

   public String getExtData() {
       return extData;
   }

   public void setExtData(String extData) {
       this.extData = extData;
   }

   public Date getRequestTime() {
       return requestTime;
   }

   public void setRequestTime(Date requestTime) {
       this.requestTime = requestTime;
   }

   public Date getResponseTime() {
       return responseTime;
   }

   public void setResponseTime(Date responseTime) {
       this.responseTime = responseTime;
   }

   public int getElaspeTime() {
       return elaspeTime;
   }

   public void setElaspeTime(int elaspeTime) {
       this.elaspeTime = elaspeTime;
   }

   public Date getCreatedDate() {
       return createdDate;
   }

   public void setCreatedDate(Date createdDate) {
       this.createdDate = createdDate;
   }

   public Date getModifiedDate() {
       return modifiedDate;
   }

   public void setModifiedDate(Date modifiedDate) {
       this.modifiedDate = modifiedDate;
   }
   
   public Long getId() {
       return id;
   }

   public void setId(Long id) {
       this.id = id;
   }

@Override
   public String toString() {
       return "ProcessUnitExecutionEntity [executionNo=" + executionNo + ", requestNo=" + requestNo
               + ", parentNo=" + parentNo + ", instanceNo=" + instanceNo + ", processUnitCode=" + processUnitCode + ", bizNo=" + bizNo
               + ", productCode=" + productCode + ", result=" + result + ", requestContent=" + requestContent
               + ", responseContent=" + responseContent + ", execType=" + execType + ", extData=" + extData
               + ", requestTime=" + requestTime + ", responseTime=" + responseTime + ", elaspeTime=" + elaspeTime
               + ", createdDate=" + createdDate + ", modifiedDate=" + modifiedDate +", id=" + id + "]";
   }
    
    
  
}

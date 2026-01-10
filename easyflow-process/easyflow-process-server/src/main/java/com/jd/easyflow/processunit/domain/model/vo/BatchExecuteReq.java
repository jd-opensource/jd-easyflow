 package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
  * @author liyuliang5
  * 
  */
 public class BatchExecuteReq {
     

     public static final String TYPE_BATCH = "batch";

     public static final String TYPE_AUTO = "auto";


     private String type;
     
     private List<String> bizNoList;
     
     private List<String> instanceNoList;
     
     private Map<String, Object> requestContext;
     
     private List<String> processUnitCodeList;

     private List<String> excludeProcessUnitCodeList;

     private List<String> productCodeList;

     private List<String> excludeProductCodeList;
     

     private List<String> resultList;

     private Date nextAutoRunTimeStart;
     
     private Date nextAutoRunTimeEnd;

     private Integer nextAutoRunTimeMaxInterval;
     
     private List<VariableEntry> variableList;
     
     private String batchRunPolicy;
     
     private Integer threadNum;
     
     
     public String getType() {
        return type;
    }



    public void setType(String type) {
        this.type = type;
    }



    public List<String> getInstanceNoList() {
        return instanceNoList;
    }



    public void setInstanceNoList(List<String> instanceNoList) {
        this.instanceNoList = instanceNoList;
    }



    public Map<String, Object> getRequestContext() {
        return requestContext;
    }



    public void setRequestContext(Map<String, Object> requestContext) {
        this.requestContext = requestContext;
    }



    public List<String> getProcessUnitCodeList() {
        return processUnitCodeList;
    }



    public void setProcessUnitCodeList(List<String> processUnitCodeList) {
        this.processUnitCodeList = processUnitCodeList;
    }



    public List<String> getExcludeProcessUnitCodeList() {
        return excludeProcessUnitCodeList;
    }



    public void setExcludeProcessUnitCodeList(List<String> excludeProcessUnitCodeList) {
        this.excludeProcessUnitCodeList = excludeProcessUnitCodeList;
    }



    public List<String> getProductCodeList() {
        return productCodeList;
    }



    public void setProductCodeList(List<String> productCodeList) {
        this.productCodeList = productCodeList;
    }



    public List<String> getExcludeProductCodeList() {
        return excludeProductCodeList;
    }



    public void setExcludeProductCodeList(List<String> excludeProductCodeList) {
        this.excludeProductCodeList = excludeProductCodeList;
    }



    public List<String> getResultList() {
        return resultList;
    }



    public void setResultList(List<String> resultList) {
        this.resultList = resultList;
    }



    public Date getNextAutoRunTimeStart() {
        return nextAutoRunTimeStart;
    }



    public void setNextAutoRunTimeStart(Date nextAutoRunTimeStart) {
        this.nextAutoRunTimeStart = nextAutoRunTimeStart;
    }



    public Integer getNextAutoRunTimeMaxInterval() {
        return nextAutoRunTimeMaxInterval;
    }



    public void setNextAutoRunTimeMaxInterval(Integer nextAutoRunTimeMaxInterval) {
        this.nextAutoRunTimeMaxInterval = nextAutoRunTimeMaxInterval;
    }



    public List<VariableEntry> getVariableList() {
        return variableList;
    }



    public void setVariableList(List<VariableEntry> variableList) {
        this.variableList = variableList;
    }

    public List<String> getBizNoList() {
        return bizNoList;
    }



    public void setBizNoList(List<String> bizNoList) {
        this.bizNoList = bizNoList;
    }


    public String getBatchRunPolicy() {
        return batchRunPolicy;
    }



    public void setBatchRunPolicy(String batchRunPolicy) {
        this.batchRunPolicy = batchRunPolicy;
    }



    public Integer getThreadNum() {
        return threadNum;
    }



    public void setThreadNum(Integer threadNum) {
        this.threadNum = threadNum;
    }
    
    public Date getNextAutoRunTimeEnd() {
        return nextAutoRunTimeEnd;
    }



    public void setNextAutoRunTimeEnd(Date nextAutoRunTimeEnd) {
        this.nextAutoRunTimeEnd = nextAutoRunTimeEnd;
    }



    @Override
    public String toString() {
        return "BatchExecuteReq [type=" + type + ", instanceNoList=" + instanceNoList + ", requestContext="
                + requestContext + ", bizNoList=" + bizNoList
                + ", processUnitCodeList=" + processUnitCodeList + ", excludeProcessUnitCodeList="
                + excludeProcessUnitCodeList + ", productCodeList=" + productCodeList + ", excludeProductCodeList="
                + excludeProductCodeList + ", resultList=" + resultList + ", nextAutoRunTimeStart="
                + nextAutoRunTimeStart+ ", nextAutoRunTimeEnd=" + nextAutoRunTimeEnd + ", nextAutoRunTimeMaxInterval=" + nextAutoRunTimeMaxInterval
                + ", variableList=" + variableList + ", batchRunPolicy=" + batchRunPolicy + ", threadNum=" + threadNum + "]";
    }



     public static class VariableEntry {
         private String name;
         private String operator;
         private String value;
        public String getName() {
            return name;
        }
        public void setName(String name) {
            this.name = name;
        }
        public String getOperator() {
            return operator;
        }
        public void setOperator(String operator) {
            this.operator = operator;
        }
        public String getValue() {
            return value;
        }
        public void setValue(String value) {
            this.value = value;
        }
        @Override
        public String toString() {
            return "VariableEntry [name=" + name + ", operator=" + operator + ", value=" + value + "]";
        }
         
         
     }
     
}


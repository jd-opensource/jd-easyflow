 package com.jd.easyflow.processunit.domain.model.vo;

 import java.util.Date;
import java.util.List;

/**
  * @author liyuliang5
  * 
  */
 public class QueryAsyncInstanceVO {

     private String unitCode;
     
     private Date nextAutoRunTime;
     
     private Date nextAutoRunTimeStart;

     private List<String> productCodeList;
     
     private List<String> excludeProductCodeList;
     
     private List<String> resultList;

     private List<VariableEntry> variableList;
     
     private Integer maxCount;
     
     
     
     public String getUnitCode() {
        return unitCode;
    }



    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }



    public Date getNextAutoRunTime() {
        return nextAutoRunTime;
    }



    public void setNextAutoRunTime(Date nextAutoRunTime) {
        this.nextAutoRunTime = nextAutoRunTime;
    }



    public Date getNextAutoRunTimeStart() {
        return nextAutoRunTimeStart;
    }



    public void setNextAutoRunTimeStart(Date nextAutoRunTimeStart) {
        this.nextAutoRunTimeStart = nextAutoRunTimeStart;
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



    public List<VariableEntry> getVariableList() {
        return variableList;
    }



    public void setVariableList(List<VariableEntry> variableList) {
        this.variableList = variableList;
    }
    

    public Integer getMaxCount() {
        return maxCount;
    }



    public void setMaxCount(Integer maxCount) {
        this.maxCount = maxCount;
    }



    @Override
    public String toString() {
        return "QueryAsyncInstanceVO [unitCode=" + unitCode + ", nextAutoRunTime=" + nextAutoRunTime
                + ", nextAutoRunTimeStart=" + nextAutoRunTimeStart + ", productCodeList=" + productCodeList
                + ", excludeProductCodeList=" + excludeProductCodeList + ", resultList=" + resultList + ", maxCount=" + maxCount
                + ", variableList=" + variableList + "]";
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


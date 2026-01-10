 package com.jd.easyflow.processunit.domain.model.vo;

 /**
  * @author liyuliang5
  * 
  */
public class ExecuteReq {

    private String unitInstanceNo;
    
    private String unitCode;
    
    private String bizNo;

    public String getUnitInstanceNo() {
        return unitInstanceNo;
    }

    public void setUnitInstanceNo(String unitInstanceNo) {
        this.unitInstanceNo = unitInstanceNo;
    }
    
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

    @Override
    public String toString() {
        return "ExecuteReq [unitInstanceNo=" + unitInstanceNo + ", unitCode=" + unitCode + ", bizNo=" + bizNo + "]";
    }

}

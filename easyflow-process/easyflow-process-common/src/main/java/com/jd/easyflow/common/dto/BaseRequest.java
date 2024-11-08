package com.jd.easyflow.common.dto;

import java.io.Serializable;

/**
* 
* @author liyuliang5
*/
public class BaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    private String reqNo;

    private String operator;

    public String getReqNo() {
        return reqNo;
    }

    public void setReqNo(String reqNo) {
        this.reqNo = reqNo;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    @Override
    public String toString() {
        return "BaseRequest [reqNo=" + reqNo + ", operator=" + operator + "]";
    }
    
}

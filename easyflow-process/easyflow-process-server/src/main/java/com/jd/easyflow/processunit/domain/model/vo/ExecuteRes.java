 package com.jd.easyflow.processunit.domain.model.vo;

 /**
  * @author liyuliang5
  * 
  */
public class ExecuteRes {
    
    public ExecuteRes() {
        
    }
    
    public ExecuteRes(String result, String responseContent) {
        this.result = result;
        this.responseContent = responseContent;
    }

     private String result;
     
     private String responseContent;

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getResponseContent() {
        return responseContent;
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    @Override
    public String toString() {
        return "ExecuteRes [result=" + result + ", responseContent=" + responseContent + "]";
    }
     
     
}

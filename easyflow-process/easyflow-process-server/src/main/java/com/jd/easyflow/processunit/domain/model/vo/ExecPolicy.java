package com.jd.easyflow.processunit.domain.model.vo;

/**
 * @author liyuliang5
 * 
 */
public class ExecPolicy {

    private String policyType;
    
    private String policyData;
    
    private String result;

    private String requestContent;

    private String responseContent;
    
    public ExecPolicy() {
        // NOOP
    }
    
    public ExecPolicy(String policyType) {
        this.policyType = policyType;
    }

    public String getPolicyType() {
        return policyType;
    }

    public void setPolicyType(String policyType) {
        this.policyType = policyType;
    }

    public String getPolicyData() {
        return policyData;
    }

    public void setPolicyData(String policyData) {
        this.policyData = policyData;
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

    @Override
    public String toString() {
        return "ExecPolicy [policyType=" + policyType + ", policyData=" + policyData + ", result=" + result
                + ", requestContent=" + requestContent + ", responseContent=" + responseContent + "]";
    }
    
    
}

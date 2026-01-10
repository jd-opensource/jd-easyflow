package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 */
public class ProcessUnitExecuteRes implements Serializable {

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
    
    
}

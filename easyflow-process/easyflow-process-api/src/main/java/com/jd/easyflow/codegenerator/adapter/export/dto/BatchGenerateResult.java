package com.jd.easyflow.codegenerator.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class BatchGenerateResult implements Serializable {
    private static final long serialVersionUID = 4964199112018029216L;

    private String[] codes;
    public String[] getCodes() {
        return codes;
    }
    public void setCodes(String[] codes) {
        this.codes = codes;
    }
    
    
}

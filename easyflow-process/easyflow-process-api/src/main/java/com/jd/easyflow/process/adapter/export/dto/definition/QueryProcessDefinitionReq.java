package com.jd.easyflow.process.adapter.export.dto.definition;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class QueryProcessDefinitionReq implements Serializable {

    private String defId;
    
    private Integer defVersion;

    public String getDefId() {
        return defId;
    }

    public void setDefId(String defId) {
        this.defId = defId;
    }

    public Integer getDefVersion() {
        return defVersion;
    }

    public void setDefVersion(Integer defVersion) {
        this.defVersion = defVersion;
    }

    @Override
    public String toString() {
        return "QueryProcessDefinitionReq [defId=" + defId + ", defVersion=" + defVersion + "]";
    }
    
    
}

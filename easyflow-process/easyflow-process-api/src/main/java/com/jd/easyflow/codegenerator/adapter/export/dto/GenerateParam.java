package com.jd.easyflow.codegenerator.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class GenerateParam implements Serializable {

    private static final long serialVersionUID = 2404501159595567936L;

    private String typeId;

    private String codePrefix;
    
    public GenerateParam() {
        
    }
    
    public GenerateParam(String typeId, String codePrefix) {
        this.typeId = typeId;
        this.codePrefix = codePrefix;
    }

    public String getTypeId() {
        return typeId;
    }

    public void setTypeId(String typeId) {
        this.typeId = typeId;
    }

    public String getCodePrefix() {
        return codePrefix;
    }

    public void setCodePrefix(String codePrefix) {
        this.codePrefix = codePrefix;
    }
    
    public static GenerateParam.GenerateParamBuilder builder() {
        return new GenerateParam.GenerateParamBuilder();
    }

    public static class GenerateParamBuilder {
        private String typeId;
        private String codePrefix;

        public GenerateParam.GenerateParamBuilder typeId(String typeId) {
            this.typeId = typeId;
            return this;
        }

        public GenerateParam.GenerateParamBuilder codePrefix(String codePrefix) {
            this.codePrefix = codePrefix;
            return this;
        }

        public GenerateParam build() {
            return new GenerateParam(this.typeId, this.codePrefix);
        }

        public String toString() {
            return "GenerateParam.GenerateParamBuilder(typeId=" + this.typeId + ", codePrefix=" + this.codePrefix + ")";
        }
    }
    
    
}

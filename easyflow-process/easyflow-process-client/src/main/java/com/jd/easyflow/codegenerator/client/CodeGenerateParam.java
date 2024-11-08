package com.jd.easyflow.codegenerator.client;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerateParam {
    
    public CodeGenerateParam() {
        
    }
    
    public CodeGenerateParam(String typeId, String codePrefix, int batchSize) {
        this.typeId = typeId;
        this.codePrefix = codePrefix;
        this.batchSize = batchSize;
    }

    private String typeId;

    private String codePrefix;

    private int batchSize;

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

    public int getBatchSize() {
        return batchSize;
    }

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }
    
    public static CodeGenerateParam.CodeGenerateParamBuilder builder() {
        return new CodeGenerateParam.CodeGenerateParamBuilder();
    }
    

    public static class CodeGenerateParamBuilder {
        private String typeId;
        private String codePrefix;
        private int batchSize;

        public CodeGenerateParam.CodeGenerateParamBuilder typeId(String typeId) {
            this.typeId = typeId;
            return this;
        }

        public CodeGenerateParam.CodeGenerateParamBuilder codePrefix(String codePrefix) {
            this.codePrefix = codePrefix;
            return this;
        }

        public CodeGenerateParam.CodeGenerateParamBuilder batchSize(int batchSize) {
            this.batchSize = batchSize;
            return this;
        }

        public CodeGenerateParam build() {
            return new CodeGenerateParam(this.typeId, this.codePrefix, this.batchSize);
        }

        public String toString() {
            return "CodeGenerateParam.CodeGenerateParamBuilder(typeId=" + this.typeId + ", codePrefix="
                    + this.codePrefix + ", batchSize=" + this.batchSize + ")";
        }
    }
    
    
}

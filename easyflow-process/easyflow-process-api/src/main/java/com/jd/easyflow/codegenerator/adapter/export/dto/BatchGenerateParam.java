package com.jd.easyflow.codegenerator.adapter.export.dto;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class BatchGenerateParam implements Serializable {


    private static final long serialVersionUID = 2404501159595567936L;

    private String typeId;

    private String codePrefix;

    private int batchSize;
    
    public BatchGenerateParam() {
        
    }
    
    public BatchGenerateParam(String typeId, String codePrefix, int batchSize) {
        this.typeId = typeId;
        this.codePrefix = codePrefix;
        this.batchSize = batchSize;
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

    public int getBatchSize() {
        return batchSize;
    }

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }

    @Override
    public String toString() {
        return "BatchGenerateParam [typeId=" + typeId + ", codePrefix=" + codePrefix + ", batchSize=" + batchSize + "]";
    }
    
    public static BatchGenerateParam.BatchGenerateParamBuilder builder() {
        return new BatchGenerateParam.BatchGenerateParamBuilder();
    }

    public static class BatchGenerateParamBuilder {
        private String typeId;
        private String codePrefix;
        private int batchSize;

        public BatchGenerateParam.BatchGenerateParamBuilder typeId(String typeId) {
            this.typeId = typeId;
            return this;
        }

        public BatchGenerateParam.BatchGenerateParamBuilder codePrefix(String codePrefix) {
            this.codePrefix = codePrefix;
            return this;
        }

        public BatchGenerateParam.BatchGenerateParamBuilder batchSize(int batchSize) {
            this.batchSize = batchSize;
            return this;
        }

        public BatchGenerateParam build() {
            return new BatchGenerateParam(this.typeId, this.codePrefix, this.batchSize);
        }

        public String toString() {
            return "BatchGenerateParam.BatchGenerateParamBuilder(typeId=" + this.typeId + ", codePrefix="
                    + this.codePrefix + ", batchSize=" + this.batchSize + ")";
        }
    }
    
    
}

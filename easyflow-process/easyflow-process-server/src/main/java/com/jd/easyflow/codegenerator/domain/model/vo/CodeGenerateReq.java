package com.jd.easyflow.codegenerator.domain.model.vo;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerateReq {

    public CodeGenerateReq(String typeId) {
        this.typeId = typeId;
    }

    public CodeGenerateReq(String typeId,String codePrefix) {
        this.typeId = typeId;
        this.codePrefix = codePrefix;
    }
    
    public CodeGenerateReq(String typeId,String codePrefix, int batchSize) {
        this.typeId = typeId;
        this.codePrefix = codePrefix;
        this.batchSize = batchSize;
    }

    public static final int ROLLING_TYPE_NONE = 0;

    public static final int ROLLING_TYPE_DAY = 1;

    String typeId;

    String codePrefix = "";

    int rollingType = ROLLING_TYPE_DAY;

    long start = 1000000;

    int startRandomRange = 0;

    int step = 1;

    int stepRandomRange = 0;

    int numLength = 8;

    int cacheSize = 1000;

    private String seperator1 = "";

    private String seperator2 = "";
    
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

    public int getRollingType() {
        return rollingType;
    }

    public void setRollingType(int rollingType) {
        this.rollingType = rollingType;
    }

    public long getStart() {
        return start;
    }

    public void setStart(long start) {
        this.start = start;
    }

    public int getStartRandomRange() {
        return startRandomRange;
    }

    public void setStartRandomRange(int startRandomRange) {
        this.startRandomRange = startRandomRange;
    }

    public int getStep() {
        return step;
    }

    public void setStep(int step) {
        this.step = step;
    }

    public int getStepRandomRange() {
        return stepRandomRange;
    }

    public void setStepRandomRange(int stepRandomRange) {
        this.stepRandomRange = stepRandomRange;
    }

    public int getNumLength() {
        return numLength;
    }

    public void setNumLength(int numLength) {
        this.numLength = numLength;
    }

    public int getCacheSize() {
        return cacheSize;
    }

    public void setCacheSize(int cacheSize) {
        this.cacheSize = cacheSize;
    }

    public String getSeperator1() {
        return seperator1;
    }

    public void setSeperator1(String seperator1) {
        this.seperator1 = seperator1;
    }

    public String getSeperator2() {
        return seperator2;
    }

    public void setSeperator2(String seperator2) {
        this.seperator2 = seperator2;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public void setBatchSize(int batchSize) {
        this.batchSize = batchSize;
    }
    
    
}

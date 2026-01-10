package com.jd.easyflow.processunit.adapter.export.dto;

import java.io.Serializable;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class SyncBeforeCallRes implements Serializable {

    private ExecPolicyDTO execPolicy;

    private String executionNo;

    private String instanceNo;

    private Map<String, Object> unitConf;

    private Map<String, String> contextData;
    
    private Map<String, String> variables;

    public ExecPolicyDTO getExecPolicy() {
        return execPolicy;
    }

    public void setExecPolicy(ExecPolicyDTO execPolicy) {
        this.execPolicy = execPolicy;
    }

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public String getInstanceNo() {
        return instanceNo;
    }

    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }

    public Map<String, Object> getUnitConf() {
        return unitConf;
    }

    public void setUnitConf(Map<String, Object> unitConf) {
        this.unitConf = unitConf;
    }

    public Map<String, String> getContextData() {
        return contextData;
    }

    public void setContextData(Map<String, String> contextData) {
        this.contextData = contextData;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }

    @Override
    public String toString() {
        return "SyncBeforeCallRes [execPolicy=" + execPolicy + ", executionNo=" + executionNo + ", instanceNo="
                + instanceNo + ", unitConf=" + unitConf + ", contextData=" + contextData + ", variables=" + variables
                + "]";
    }
    
    
}

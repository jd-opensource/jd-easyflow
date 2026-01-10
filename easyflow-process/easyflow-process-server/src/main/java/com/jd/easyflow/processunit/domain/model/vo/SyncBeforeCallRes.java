package com.jd.easyflow.processunit.domain.model.vo;

import java.util.HashMap;
import java.util.Map;

/**
 * @author liyuliang5
 * 
 */
public class SyncBeforeCallRes {

    private ExecPolicy execPolicy;

    private String executionNo;

    private String instanceNo;

    private Map<String, Object> unitConf;

    private Map<String, String> contextData;
    
    private Map<String, String> variables;

    public void putData(String key, String value) {
        if (contextData == null) {
          contextData = new HashMap();  
        }
        contextData.put(key,value);
    }

    public <T> T getData(String key){
        if(contextData == null){
            return null;
        }
        return (T) contextData.get(key);
    }


    public ExecPolicy getExecPolicy() {
        return execPolicy;
    }

    public void setExecPolicy(ExecPolicy execPolicy) {
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

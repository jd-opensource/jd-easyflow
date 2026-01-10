package com.jd.easyflow.processunit.client.bean;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author liyuliang5
 *
 */
public class ExecContext {
    
    public ExecContext() {
        // NOOP
    }
    
    public ExecContext(ExecParam param) {
        this.param = param;
        this.result = new ExecResult();
    }
    
    private String unitCode;
    
    private String bizNo;

    private String instanceNo;
    
    private Map<String, Object> unitConf;
    
    private String executionNo;
    
    private ExecParam param;
    
    private ExecResult result;
    
    private ExecPolicy policy;
        
    private boolean executed;

    private Map<String, String> serverContextData;
    
    private Map<String, String> variables;

    public Object getUnitConf(String key) {
        if (unitConf == null) {
            return null;
        }
        return unitConf.get(key);
    }
    
    public String getVariable(String key) {
        if (variables == null) {
            return null;
        }
        return variables.get(key);
    }
    
    public void putVariable(String key, String value) {
        if (variables == null) {
            variables = new ConcurrentHashMap<>();
        }
        if (value == null) {
            variables.remove(key);
        } else {
            variables.put(key, value);
        }
    }

    public String getUnitCode() {
        return unitCode;
    }

    public void setUnitCode(String unitCode) {
        this.unitCode = unitCode;
    }

    public String getBizNo() {
        return bizNo;
    }

    public void setBizNo(String bizNo) {
        this.bizNo = bizNo;
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

    public String getExecutionNo() {
        return executionNo;
    }

    public void setExecutionNo(String executionNo) {
        this.executionNo = executionNo;
    }

    public ExecParam getParam() {
        return param;
    }

    public void setParam(ExecParam param) {
        this.param = param;
    }

    public ExecResult getResult() {
        return result;
    }

    public void setResult(ExecResult result) {
        this.result = result;
    }

    public ExecPolicy getPolicy() {
        return policy;
    }

    public void setPolicy(ExecPolicy policy) {
        this.policy = policy;
    }

    public boolean isExecuted() {
        return executed;
    }

    public void setExecuted(boolean executed) {
        this.executed = executed;
    }

    public Map<String, String> getServerContextData() {
        return serverContextData;
    }

    public void setServerContextData(Map<String, String> serverContextData) {
        this.serverContextData = serverContextData;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }

    @Override
    public String toString() {
        return "ExecContext [unitCode=" + unitCode + ", bizNo=" + bizNo + ", instanceNo=" + instanceNo + ", unitConf="
                + unitConf + ", executionNo=" + executionNo + ", param=" + param + ", result=" + result + ", policy="
                + policy + ", executed=" + executed + ", serverContextData=" + serverContextData + ", variables="
                + variables + "]";
    }
    
    
}

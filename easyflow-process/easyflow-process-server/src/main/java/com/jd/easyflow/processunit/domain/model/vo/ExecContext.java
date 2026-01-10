package com.jd.easyflow.processunit.domain.model.vo;

import java.util.Map;

import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitExecutionEntity;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitInstanceEntity;

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
    
    private ProcessUnitEntity processUnit;
    
    private boolean newInstance;
    
    private ProcessUnitInstanceEntity instance;
    
    private ProcessUnitExecutionEntity execution;
    
    private ExecParam param;
    
    private ExecResult result;
    
    private boolean locked;
    
    private String lockRequestId;
    
    private boolean executed;

    private boolean realCall;

    private ExecPolicy policy;

    private String executionNo;

    private String instanceNo;

    private String lastResult;

    private String lastRequestContent;

    private String lastResponseContent;

    private long requestTime = 0;
    
    private String parentNo;
    
    private Map<String, String> variables;
    
    private Map<String, String> clientInfo;
    
    private String extDataStr;    

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

    public ProcessUnitEntity getProcessUnit() {
        return processUnit;
    }

    public void setProcessUnit(ProcessUnitEntity processUnit) {
        this.processUnit = processUnit;
    }

    public boolean isNewInstance() {
        return newInstance;
    }

    public void setNewInstance(boolean newInstance) {
        this.newInstance = newInstance;
    }

    public ProcessUnitInstanceEntity getInstance() {
        return instance;
    }

    public void setInstance(ProcessUnitInstanceEntity instance) {
        this.instance = instance;
    }

    public ProcessUnitExecutionEntity getExecution() {
        return execution;
    }

    public void setExecution(ProcessUnitExecutionEntity execution) {
        this.execution = execution;
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

    public boolean isLocked() {
        return locked;
    }

    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    public String getLockRequestId() {
        return lockRequestId;
    }

    public void setLockRequestId(String lockRequestId) {
        this.lockRequestId = lockRequestId;
    }

    public boolean isExecuted() {
        return executed;
    }

    public void setExecuted(boolean executed) {
        this.executed = executed;
    }

    public boolean isRealCall() {
        return realCall;
    }

    public void setRealCall(boolean realCall) {
        this.realCall = realCall;
    }

    public ExecPolicy getPolicy() {
        return policy;
    }

    public void setPolicy(ExecPolicy policy) {
        this.policy = policy;
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

    public String getLastResult() {
        return lastResult;
    }

    public void setLastResult(String lastResult) {
        this.lastResult = lastResult;
    }

    public String getLastRequestContent() {
        return lastRequestContent;
    }

    public void setLastRequestContent(String lastRequestContent) {
        this.lastRequestContent = lastRequestContent;
    }

    public String getLastResponseContent() {
        return lastResponseContent;
    }

    public void setLastResponseContent(String lastResponseContent) {
        this.lastResponseContent = lastResponseContent;
    }

    public long getRequestTime() {
        return requestTime;
    }

    public void setRequestTime(long requestTime) {
        this.requestTime = requestTime;
    }

    public String getParentNo() {
        return parentNo;
    }

    public void setParentNo(String parentNo) {
        this.parentNo = parentNo;
    }

    public Map<String, String> getVariables() {
        return variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }
    
    public Map<String, String> getClientInfo() {
        return clientInfo;
    }

    public void setClientInfo(Map<String, String> clientInfo) {
        this.clientInfo = clientInfo;
    }
    
    public String getExtDataStr() {
        return extDataStr;
    }

    public void setExtDataStr(String extDataStr) {
        this.extDataStr = extDataStr;
    }


    @Override
    public String toString() {
        return "ExecContext [unitCode=" + unitCode + ", bizNo=" + bizNo + ", processUnit=" + processUnit
                + ", newInstance=" + newInstance + ", instance=" + instance + ", execution=" + execution + ", param="
                + param + ", result=" + result + ", locked=" + locked + ", lockRequestId=" + lockRequestId
                + ", executed=" + executed + ", realCall=" + realCall + ", policy=" + policy + ", executionNo="
                + executionNo + ", instanceNo=" + instanceNo + ", lastResult=" + lastResult + ", lastRequestContent="
                + lastRequestContent + ", lastResponseContent=" + lastResponseContent + ", requestTime=" + requestTime
                + ", parentNo=" + parentNo + ", variables=" + variables + ", clientInfo=" + clientInfo  + ", extDataStr=" + extDataStr + "]";
    }
    
}

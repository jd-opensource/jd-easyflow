package com.jd.easyflow.process.adapter.export.dto.transaction.command;

import java.util.Date;

import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessContextDTO;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommand;

/**
 * 
 * @author liyuliang5
 *
 */
public class InterruptTxnCommand extends TxnCommand {
    
    private String processInstanceNo;
    
    private StdProcessContextDTO processContext;
    
    private String interruptUser;
    
    private Date interruptTime;

    public InterruptTxnCommand() {
        this.commandType = ProcessTransactionConstants.TXN_COMMAND_INTERRUPT;
    }

    public String getProcessInstanceNo() {
        return processInstanceNo;
    }

    public void setProcessInstanceNo(String processInstanceNo) {
        this.processInstanceNo = processInstanceNo;
    }

    public StdProcessContextDTO getProcessContext() {
        return processContext;
    }

    public void setProcessContext(StdProcessContextDTO processContext) {
        this.processContext = processContext;
    }

    public String getInterruptUser() {
        return interruptUser;
    }

    public void setInterruptUser(String interruptUser) {
        this.interruptUser = interruptUser;
    }

    public Date getInterruptTime() {
        return interruptTime;
    }

    public void setInterruptTime(Date interruptTime) {
        this.interruptTime = interruptTime;
    }

    @Override
    public String toString() {
        return "InterruptTxnCommand [processInstanceNo=" + processInstanceNo + ", processContext=" + processContext
                + ", interruptUser=" + interruptUser + ", interruptTime=" + interruptTime + "]";
    }
    
    
}

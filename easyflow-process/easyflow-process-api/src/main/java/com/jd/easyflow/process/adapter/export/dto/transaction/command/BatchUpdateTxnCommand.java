package com.jd.easyflow.process.adapter.export.dto.transaction.command;

import java.util.List;

import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.StdProcessContextDTO;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommand;

/**
 * 
 * @author liyuliang5
 *
 */
public class BatchUpdateTxnCommand extends TxnCommand {

    private List<Object> objects;

    private StdProcessContextDTO processContext;

    public BatchUpdateTxnCommand() {
        this.commandType = ProcessTransactionConstants.TXN_COMMAND_BATCH_UPDATE;
    }

    public List<Object> getObjects() {
        return objects;
    }

    public void setObjects(List<Object> objects) {
        this.objects = objects;
    }

    public StdProcessContextDTO getProcessContext() {
        return processContext;
    }

    public void setProcessContext(StdProcessContextDTO processContext) {
        this.processContext = processContext;
    }

    @Override
    public String toString() {
        return "BatchUpdateTxnCommand [objects=" + objects + ", processContext=" + processContext + "]";
    }
    
    
    
}

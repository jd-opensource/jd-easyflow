package com.jd.easyflow.process.adapter.export.dto.transaction;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class TxnReq implements Serializable {

    private List<TxnCommand> commandList;

    public List<TxnCommand> getCommandList() {
        return commandList;
    }

    public void setCommandList(List<TxnCommand> commandList) {
        this.commandList = commandList;
    }

    @Override
    public String toString() {
        return "TxnReq [commandList=" + commandList + "]";
    }
    
    
}

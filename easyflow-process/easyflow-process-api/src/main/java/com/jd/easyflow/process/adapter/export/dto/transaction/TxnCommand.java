package com.jd.easyflow.process.adapter.export.dto.transaction;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class TxnCommand implements Serializable {

    protected String commandType;

    public String getCommandType() {
        return commandType;
    }

    public void setCommandType(String commandType) {
        this.commandType = commandType;
    }

    @Override
    public String toString() {
        return "TxnCommand [commandType=" + commandType + "]";
    }
    
    
}

package com.jd.easyflow.process.adapter.export.dto.transaction;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class TxnRes implements Serializable {

    private List<TxnCommandResult> resultList;

    public List<TxnCommandResult> getResultList() {
        return resultList;
    }

    public void setResultList(List<TxnCommandResult> resultList) {
        this.resultList = resultList;
    }

    @Override
    public String toString() {
        return "TxnRes [resultList=" + resultList + "]";
    }
    
    
}

package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class CanCancelProcessInstanceRes implements Serializable {
    private static final long serialVersionUID = 5517298115339740671L;

    private boolean canCancel;

    private String reason;
    public boolean isCanCancel() {
        return canCancel;
    }
    public void setCanCancel(boolean canCancel) {
        this.canCancel = canCancel;
    }
    public String getReason() {
        return reason;
    }
    public void setReason(String reason) {
        this.reason = reason;
    }
    @Override
    public String toString() {
        return "CanCancelProcessInstanceRes [canCancel=" + canCancel + ", reason=" + reason + "]";
    }
    
    
}

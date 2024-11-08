package com.jd.easyflow.process.adapter.export.dto.task;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class CanWithdrawTaskRes implements Serializable {

    private boolean canWithDraw;
    
    private String reason;

    public boolean isCanWithDraw() {
        return canWithDraw;
    }

    public void setCanWithDraw(boolean canWithDraw) {
        this.canWithDraw = canWithDraw;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    @Override
    public String toString() {
        return "CanWithdrawTaskRes [canWithDraw=" + canWithDraw + ", reason=" + reason + "]";
    }
    
    
}

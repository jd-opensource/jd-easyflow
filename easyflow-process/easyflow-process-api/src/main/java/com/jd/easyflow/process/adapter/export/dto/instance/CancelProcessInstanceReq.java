package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liyuliang5
 *
 */
public class CancelProcessInstanceReq implements Serializable {
    private static final long serialVersionUID = -6341207830866532031L;

    private String instanceNo;

    private String cancelUser;

    private Date cancelTime;

    private String cancelReason;
    public String getInstanceNo() {
        return instanceNo;
    }
    public void setInstanceNo(String instanceNo) {
        this.instanceNo = instanceNo;
    }
    public String getCancelUser() {
        return cancelUser;
    }
    public void setCancelUser(String cancelUser) {
        this.cancelUser = cancelUser;
    }
    public Date getCancelTime() {
        return cancelTime;
    }
    public void setCancelTime(Date cancelTime) {
        this.cancelTime = cancelTime;
    }
    public String getCancelReason() {
        return cancelReason;
    }
    public void setCancelReason(String cancelReason) {
        this.cancelReason = cancelReason;
    }
    @Override
    public String toString() {
        return "CancelProcessInstanceReq [instanceNo=" + instanceNo + ", cancelUser=" + cancelUser + ", cancelTime="
                + cancelTime + ", cancelReason=" + cancelReason + "]";
    }
    
    
}

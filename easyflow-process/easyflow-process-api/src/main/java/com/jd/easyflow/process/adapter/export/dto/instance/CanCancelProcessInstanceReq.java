package com.jd.easyflow.process.adapter.export.dto.instance;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class CanCancelProcessInstanceReq implements Serializable {
    private static final long serialVersionUID = 1853675006208281830L;

    private String instanceNo;

    private String cancelUser;

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

    @Override
    public String toString() {
        return "CanCancelProcessInstanceReq [instanceNo=" + instanceNo + ", cancelUser=" + cancelUser + "]";
    }
    
    
}

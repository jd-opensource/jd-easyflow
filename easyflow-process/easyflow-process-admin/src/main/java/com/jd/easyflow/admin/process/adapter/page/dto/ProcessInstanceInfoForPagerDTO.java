package com.jd.easyflow.admin.process.adapter.page.dto;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;

/**
 * @author liyuliang5
 */
public class ProcessInstanceInfoForPagerDTO extends ProcessInstanceDTO {

    
    private boolean canCancel;

    public boolean isCanCancel() {
        return canCancel;
    }

    public void setCanCancel(boolean canCancel) {
        this.canCancel = canCancel;
    }

    @Override
    public String toString() {
        return "ProcessInstanceInfoForPagerDTO [canCancel=" + canCancel + ", toString()=" + super.toString() + "]";
    }


    
}

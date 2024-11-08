package com.jd.easyflow.admin.process.adapter.page.dto;

import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;

/**
 * @author liyuliang5
 */
public class ProcessInstanceInfoForPagerDTO extends ProcessInstanceDTO {

    
    private boolean canCanCel;

    public boolean isCanCanCel() {
        return canCanCel;
    }

    public void setCanCanCel(boolean canCanCel) {
        this.canCanCel = canCanCel;
    }

    @Override
    public String toString() {
        return "ProcessInstanceInfoForPagerDTO [canCanCel=" + canCanCel + "]";
    }
    
    
}

package com.jd.easyflow.flow.ext.check;

import java.util.ArrayList;
import java.util.List;

/**
 * @author liyuliang5
 */
public class CheckResult {
    
    List<CheckErrorItem> errorItemList = new ArrayList<CheckErrorItem>();
    
    public void addErrorItem(CheckErrorItem errorItem) {
        errorItemList.add(errorItem);
    }

    public List<CheckErrorItem> getErrorItemList() {
        return errorItemList;
    }

    public void setErrorItemList(List<CheckErrorItem> errorItemList) {
        this.errorItemList = errorItemList;
    }
    
}

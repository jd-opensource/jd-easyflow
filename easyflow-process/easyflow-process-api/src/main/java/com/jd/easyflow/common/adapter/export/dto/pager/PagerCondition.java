package com.jd.easyflow.common.adapter.export.dto.pager;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class PagerCondition implements Serializable {

    private static final long serialVersionUID = 7553262335974337343L;

    private long start = -1;

    private int pageSize = -1;

    private long pageIndex = -1;

    private List<FieldEntry> fieldList = new ArrayList<FieldEntry>();

    private List<SortEntry> sortList = new ArrayList<SortEntry>();

    private boolean count = true;

    private Boolean page = true;
    
    private Map<String, Object> extData;

    public PagerCondition() {

    }

    public PagerCondition(long pageIndex, int pageSize) {
        this.pageIndex = pageIndex;
        this.start = (pageIndex - 1) * pageSize;
        this.pageSize = pageSize;
    }

    public void addSortField(String key, long seq, String order) {
        if (sortList == null) {
            sortList = new ArrayList<SortEntry>();
        }
        SortEntry sortEntry = new SortEntry(key, seq, order);
        sortList.add(sortEntry);
    }

    public long getPageIndex() {
        return pageIndex;
    }

    @Deprecated
    public void setPageIndex(long pageIndex) {
        this.pageIndex = pageIndex;
    }

    public FieldEntry field(String name) {
        return getField(name);
    }

    public FieldEntry getField(String name) {
        if (fieldList != null) {
            for (FieldEntry entry : fieldList) {
                if (name.equals(entry.getName())) {
                    return entry;
                }
            }
        }
        return null;
    }

    public Map<String, FieldEntry> getFieldMap() {
        Map<String, FieldEntry> map = new HashMap<String, FieldEntry>(fieldList.size());
        fieldList.forEach(entry -> map.put(entry.getName(), entry));
        return map;
    }

    public Map<String, SortEntry> getSortMap() {
        Map<String, SortEntry> map = new HashMap<String, SortEntry>(sortList.size());
        sortList.forEach(entry -> map.put(entry.getKey(), entry));
        return map;
    }

    public void addField(FieldEntry entry) {
        fieldList.add(entry);
    }

    public void addSort(SortEntry entry) {
        sortList.add(entry);
    }

    public SortEntry sort(String name) {
        return getSort(name);
    }

    public SortEntry getSort(String name) {
        for (SortEntry entry : sortList) {
            if (name.equals(entry.getKey())) {
                return entry;
            }
        }
        return null;
    }

    public long getStart() {
        if (start == -1) {
            start = (pageIndex - 1) * pageSize;
        }
        return start;
    }

    public Boolean getPage() {
        return page;
    }

    public void setPage(Boolean page) {
        this.page = page;
    }

    public List<FieldEntry> getFieldList() {
        return fieldList;
    }

    public void setFieldList(List<FieldEntry> fieldList) {
        this.fieldList = fieldList;
    }

    public void setStart(long start) {
        this.start = start;
    }

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public List<SortEntry> getSortList() {
        return sortList;
    }

    public void setSortList(List<SortEntry> sortList) {
        this.sortList = sortList;
    }

    public boolean isCount() {
        return count;
    }

    public void setCount(boolean count) {
        this.count = count;
    }
    
    public void putExtData(String key, Object value) {
        if (extData == null) {
            extData = new HashMap<>();
        }
        extData.put(key, value);
    }
    
    public <T>T getExtData(String key) {
        if (extData == null) {
            return null;
        }
        return (T) extData.get(key);
    }

    public Map<String, Object> getExtData() {
        return extData;
    }

    public void setExtData(Map<String, Object> extData) {
        this.extData = extData;
    }

    @Override
    public String toString() {
        return "PagerCondition [start=" + start + ", pageSize=" + pageSize + ", pageIndex=" + pageIndex + ", fieldList="
                + fieldList + ", sortList=" + sortList + ", count=" + count + ", page=" + page + ", extData=" + extData
                + "]";
    }
    
    

}

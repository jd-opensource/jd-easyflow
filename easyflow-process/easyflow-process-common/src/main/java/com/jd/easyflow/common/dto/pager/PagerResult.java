package com.jd.easyflow.common.dto.pager;

import java.io.Serializable;
import java.util.List;

/**
 * @author liyuliang5
 *
 */
public class PagerResult<T> implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long count;

    private List<T> list;

    private int pageNum;

    private int pageSize;

    private int pages;


    public PagerResult() {
    }

    public PagerResult(List<T> list) {
        this.list = list;
    }

    public PagerResult(Long count, List<T> list) {
        this.count = count;
        this.list = list;
    }

    public PagerResult(Long count, List<T> list,int pageNum,int pageSize) {
        this.count = count;
        this.list = list;
        this.pageNum = pageNum;
        this.pageSize = pageSize;
    }

    public Long getCount() {
        return count;
    }

    public void setCount(Long count) {
        this.count = count;
    }

    public List<T> getList() {
        return list;
    }

    public void setList(List<T> list) {
        this.list = list;
    }


    public int getPageNum() {
        return pageNum;
    }

    public void setPageNum(int pageNum) {
        this.pageNum = pageNum;
    }

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public int getPages() {
        if (count== null || pageSize < 1){
            return 0;
        }
        int totalPages = (int) (count / pageSize);
        if (count % pageSize != 0){
            totalPages ++;
        }
        return totalPages;
    }

    @Override
    public String toString() {
        return "PagerResult [count=" + count + ", list=" + list + ", pageNum=" + pageNum + ", pageSize=" + pageSize
                + ", pages=" + pages + "]";
    }
    
    
}

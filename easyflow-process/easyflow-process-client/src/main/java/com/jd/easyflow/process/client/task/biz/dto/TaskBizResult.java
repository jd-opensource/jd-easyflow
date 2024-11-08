package com.jd.easyflow.process.client.task.biz.dto;

/**
 * @author liyuliang5
 *
 */
public class TaskBizResult {

    private String code;
    
    private String msg;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    @Override
    public String toString() {
        return "TaskBizResult [code=" + code + ", msg=" + msg + "]";
    }
    
    
}

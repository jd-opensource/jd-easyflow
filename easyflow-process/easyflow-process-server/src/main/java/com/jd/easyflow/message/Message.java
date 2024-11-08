package com.jd.easyflow.message;

/**
 * 
 * @author liyuliang5
 *
 */
public class Message {
    
    String bizId;
    
    String msgTopic;
    
    String text;
    
    public Message() {
        // NOOP
    }
    
    public Message(String bizId, String msgTopic, String text) {
        this.bizId = bizId;
        this.msgTopic = msgTopic;
        this.text = text;
    }

    public String getBizId() {
        return bizId;
    }

    public void setBizId(String bizId) {
        this.bizId = bizId;
    }

    public String getMsgTopic() {
        return msgTopic;
    }

    public void setMsgTopic(String msgTopic) {
        this.msgTopic = msgTopic;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
    
    
 
}

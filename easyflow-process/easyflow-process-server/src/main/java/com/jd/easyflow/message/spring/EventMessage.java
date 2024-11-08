package com.jd.easyflow.message.spring;

import org.springframework.context.ApplicationEvent;

/**
 * @author liyuliang5
 *
 */
public class EventMessage extends ApplicationEvent {
    
    private String bizId;
    
    private String messageTopic;
    
    private String text;

    public EventMessage(Object source) {
        super(source);
    }

    public String getBizId() {
        return bizId;
    }

    public void setBizId(String bizId) {
        this.bizId = bizId;
    }

    public String getMessageTopic() {
        return messageTopic;
    }

    public void setMessageTopic(String messageTopic) {
        this.messageTopic = messageTopic;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @Override
    public String toString() {
        return "EventMessage [bizId=" + bizId + ", messageTopic=" + messageTopic + ", text=" + text + "]";
    }
    
    
    
    

}

package com.jd.easyflow.message;

/**
 * @author liyuliang5
 *
 */
public interface MessageSendService {
    
    void sendMessage(Message message);
    
    void sendMessage(Message message, int timeoutMillis);
    
    default void sendMessage(String bizId, String msgTopic, String text) {
        sendMessage(new Message(bizId, msgTopic, text));
    }

}

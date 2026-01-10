package com.jd.easyflow.message.spring;

import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;

import com.jd.easyflow.message.Message;
import com.jd.easyflow.message.MessageListener;

/**
 * 
 * @author liyuliang5
 *
 */
public class EventMessageListener implements ApplicationListener<EventMessage> {

    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    private Map<String, com.jd.easyflow.message.MessageListener> messageListenerMap;

    private Executor executor = Executors.newCachedThreadPool();

    @Override
    public void onApplicationEvent(EventMessage event) {
        MessageListener messageListener = messageListenerMap.get(event.getMessageTopic());
        if (messageListener == null) {
            logger.warn("topic:" + event.getMessageTopic() + " has no listener");
            return;
        }
        executor.execute(() -> {
            messageListener
                    .onMessage(Arrays.asList(new Message(event.getBizId(), event.getMessageTopic(), event.getText())));
        });
    }

    public Map<String, com.jd.easyflow.message.MessageListener> getMessageListenerMap() {
        return messageListenerMap;
    }

    public void setMessageListenerMap(Map<String, com.jd.easyflow.message.MessageListener> messageListenerMap) {
        this.messageListenerMap = messageListenerMap;
    }

    public Executor getExecutor() {
        return executor;
    }

    public void setExecutor(Executor executor) {
        this.executor = executor;
    }

}

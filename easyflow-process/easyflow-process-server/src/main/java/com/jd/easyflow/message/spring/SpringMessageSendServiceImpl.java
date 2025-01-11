package com.jd.easyflow.message.spring;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;

import com.jd.easyflow.message.Message;
import com.jd.easyflow.message.MessageSendService;

/**
 * @author liyuliang5
 *
 */
public class SpringMessageSendServiceImpl implements MessageSendService {
    
    private static final Logger log = LoggerFactory.getLogger(SpringMessageSendServiceImpl.class);


    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Override
    public void sendMessage(Message message) {
        EventMessage eventMessage = new EventMessage(message.getMsgTopic());
        eventMessage.setBizId(message.getBizId());
        eventMessage.setMessageTopic(message.getMsgTopic());
        eventMessage.setText(message.getText());
        log.info("Spring send message messageTopic:{},bizId:{},text:{}", message.getMsgTopic(), message.getBizId(),
                message.getText());
        applicationEventPublisher.publishEvent(eventMessage);
        log.info("Spring send message end");
    }

    @Override
    public void sendMessage(Message message, int timeoutMillis) {
        log.info("spring send message has no timeout");
        sendMessage(message);
    }
}

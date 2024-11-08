package com.jd.easyflow.message;

import java.lang.reflect.ParameterizedType;
import java.util.List;
import java.util.Optional;

import javax.xml.bind.ValidationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public abstract class BaseMessageListener<T> implements MessageListener, ApplicationContextAware {
    
    private Logger logger = LoggerFactory.getLogger(this.getClass());

    private boolean exceptionConsume;
    
    private ApplicationContext applicationContext;

    @Override
    public void onMessage(List<Message> messages) {
        for (Message message : messages) {
            onMessage(message);
        }
    }
    
    public void onMessage(Message message) {
        T request = null;
        try {
            Optional<ParameterizedType> genericSuperclass = Optional
                    .ofNullable(((ParameterizedType) getClass().getGenericSuperclass()));
            if (genericSuperclass.isPresent()) {
                request = JSON.parseObject(message.getText(),
                        (Class<T>) (genericSuperclass.get().getActualTypeArguments()[0]));
            } else {
                throw new EasyFlowException("Message JSON parse exception");
            }
        } catch (Exception e) {
            logger.error("Message JSON parse exception, content:" + message.getText(), e);
            return;
        }

        if (!validate(request, message)) {
            return;
        }
        logger.info("Start process message,topic:{}, bizId:{}, content:{}", message.getMsgTopic(), message.getMsgTopic(), message.getText());
        try {
            BaseMessageListener messageListener = applicationContext.getBean(getClass());
            messageListener.process(request, message);
        } catch (Exception e) {
            logger.error("message[" + message.getText() + "] process exception, " + e.getMessage(), e);
            if (exceptionConsume) {
                logger.warn("Message process exception, exceptionConsume is true, no retry");
            } else {
                throw e;
            }
        }
    }
    
    public abstract boolean validate( T obj, Message message);

    public abstract void process(T obj, Message message);

    public boolean isExceptionConsume() {
        return exceptionConsume;
    }

    public void setExceptionConsume(boolean exceptionConsume) {
        this.exceptionConsume = exceptionConsume;
    }
    
    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }


}

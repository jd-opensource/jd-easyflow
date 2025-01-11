package com.jd.easyflow.message.impl;

import com.jd.easyflow.message.Message;
import com.jd.easyflow.message.MessageSendService;

/**
 * @author liyuliang5
 *
 */
public class NoopMessageSendServiceImpl implements MessageSendService {

    @Override
    public void sendMessage(Message message) {
        // NOOP
    }

    @Override
    public void sendMessage(Message message, int timeoutMillis) {
        // NOOP
    }

}

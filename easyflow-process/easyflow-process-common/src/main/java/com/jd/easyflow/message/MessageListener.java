package com.jd.easyflow.message;

import java.util.List;

/**
 * @author liyuliang5
 *
 */
public interface MessageListener {
    
    void onMessage(List<Message> message);
    
}

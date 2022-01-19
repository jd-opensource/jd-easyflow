package com.jd.easyflow.flow.exception;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowException extends RuntimeException {

    public FlowException() {
        
    }
    
    public FlowException(String message) {
        super(message);
    }
    
    public FlowException(Throwable cause) {
        super(cause);
    }

    
    public FlowException(String message, Throwable cause) {
        super(message, cause);
    }
}

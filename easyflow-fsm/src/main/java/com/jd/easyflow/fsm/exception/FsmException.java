package com.jd.easyflow.fsm.exception;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmException extends RuntimeException{

    public FsmException() {
        super();
    }
    
    public FsmException(String message) {
        super(message);
    }
    
    public FsmException(String message, Throwable cause) {
        super(message, cause);
    }
}

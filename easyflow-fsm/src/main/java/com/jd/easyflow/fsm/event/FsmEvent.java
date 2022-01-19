package com.jd.easyflow.fsm.event;

import com.jd.easyflow.fsm.FsmContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmEvent {

	private String type;
	
	private Object data;
	
	private FsmContext context;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Object getData() {
		return data;
	}

	public void setData(Object data) {
		this.data = data;
	}

	public FsmContext getContext() {
		return context;
	}

	public void setContext(FsmContext context) {
		this.context = context;
	}
	
	
}

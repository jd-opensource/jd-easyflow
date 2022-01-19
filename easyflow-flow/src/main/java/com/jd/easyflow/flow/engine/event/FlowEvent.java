package com.jd.easyflow.flow.engine.event;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowEvent {

	private String type;
	
	private Object data;
	
	private FlowContext context;
	
	public FlowEvent() {
	    
	}
	
	public FlowEvent(String type, Object data, FlowContext context) {
	    this.type = type;
	    this.data = data;
	    this.context = context;
	}

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
	
	public <T>T getMapData(String key) {
		if (data == null) {
			return null;
		}
		return (T) ((Map<String, Object>) data).get(key);
	}
	
	public <T>T getListData(int index) {
		if (data == null) {
			return null;
		}
		return (T) ((List<Object>) data).get(index);
	}

	public FlowContext getContext() {
		return context;
	}

	public void setContext(FlowContext context) {
		this.context = context;
	}
	
	
}

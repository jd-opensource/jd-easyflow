package com.jd.easyflow.flow.engine.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowParamBuilder {

	private FlowParam param;
	
	public static FlowParamBuilder create(String flowId, String nodeId) {
		FlowParam param = new FlowParam();
		param.setFlowId(flowId);
		param.setNodeIds(new String[] {nodeId});
		FlowParamBuilder builder = new FlowParamBuilder();
		builder.param = param;
		return builder;
	}
	
	   public static FlowParamBuilder create(String flowId, String[] nodeIds) {
	        FlowParam param = new FlowParam();
	        param.setFlowId(flowId);
	        param.setNodeIds(nodeIds);
	        FlowParamBuilder builder = new FlowParamBuilder();
	        builder.param = param;
	        return builder;
	    }
	
	public FlowParamBuilder putParam(String key, Object value) {
		if (param.getParam() == null) {
			param.setParam( new HashMap<>());
		}
		((Map<String, Object>) param.getParam()).put(key, value);
		return this;
	}
	
	public FlowParamBuilder addParam(Object value) {
		if (param.getParam() == null) {
			param.setParam(new ArrayList());
		}
		((List<Object>) param.getParam()).add(value);
		return this;
	}
	
	public FlowParamBuilder addParams(Object... values) {
		if (param.getParam() == null) {
			param.setParam(new ArrayList());
		}
		((List<Object>) param.getParam()).addAll(Arrays.asList(values));
		return this;
	}
	
	public FlowParamBuilder paramObject(Object o) {
		param.setParam(o);
		return this;
	}
	
    public FlowParamBuilder putData(String key, Object value) {
        param.put(key, value);
        return this;
    }
    
    public FlowParamBuilder putData(Map<String, Object> dataMap) {
        if (dataMap != null) {
            for (Entry<String, Object> entry : dataMap.entrySet()) {
                param.put(entry.getKey(), entry.getValue());
            }
        }
        return this;
    }
    
    public FlowParamBuilder setContext(FlowContext context) {
        param.setContext(context);
        return this;
    }
	
	public FlowParam build() {
		return param;
	}
	
	
	
	
	
}

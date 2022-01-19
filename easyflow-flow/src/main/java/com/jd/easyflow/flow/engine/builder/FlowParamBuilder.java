package com.jd.easyflow.flow.engine.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
	
	public FlowParam build() {
		return param;
	}
	
	
	
	
	
}

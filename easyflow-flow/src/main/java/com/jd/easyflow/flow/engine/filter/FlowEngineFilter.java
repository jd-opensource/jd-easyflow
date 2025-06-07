package com.jd.easyflow.flow.engine.filter;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public interface FlowEngineFilter extends Filter<Pair<FlowParam, FlowEngine>, FlowResult> {

}

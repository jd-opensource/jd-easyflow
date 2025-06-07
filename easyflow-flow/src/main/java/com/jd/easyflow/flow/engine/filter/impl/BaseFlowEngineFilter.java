package com.jd.easyflow.flow.engine.filter.impl;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.filter.FlowEngineFilter;
import com.jd.easyflow.flow.filter.BaseFilter;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public abstract class BaseFlowEngineFilter extends BaseFilter<Pair<FlowParam, FlowEngine>, FlowResult> implements FlowEngineFilter {

}

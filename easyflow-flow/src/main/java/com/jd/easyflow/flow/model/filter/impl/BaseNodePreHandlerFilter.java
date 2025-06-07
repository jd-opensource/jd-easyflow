package com.jd.easyflow.flow.model.filter.impl;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.BaseFilter;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.NodePreHandlerFilter;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 * 
 */
public abstract class BaseNodePreHandlerFilter extends BaseFilter<Pair<NodeContext, FlowContext>, Boolean> implements NodePreHandlerFilter {

}

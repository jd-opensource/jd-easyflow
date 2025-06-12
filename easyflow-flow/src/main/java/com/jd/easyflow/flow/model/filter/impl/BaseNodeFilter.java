package com.jd.easyflow.flow.model.filter.impl;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.BaseFilter;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.NodeFilter;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public abstract class BaseNodeFilter extends BaseFilter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> implements NodeFilter {

}

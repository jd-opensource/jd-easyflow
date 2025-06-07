package com.jd.easyflow.flow.model.filter;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 */
public interface NodeFilter extends Filter<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> {

}

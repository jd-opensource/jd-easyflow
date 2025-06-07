package com.jd.easyflow.flow.model.filter;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.Pair;

/**
 * @author liyuliang5
 */
public interface NodePostHandlerFilter extends Filter<Pair<NodeContext, FlowContext>, NodeContext[]> {

}

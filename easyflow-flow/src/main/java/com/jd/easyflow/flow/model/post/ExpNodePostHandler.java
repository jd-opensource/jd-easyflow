package com.jd.easyflow.flow.model.post;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpNodePostHandler extends AbstractNodePostHandler {
	
	private static final Logger logger = LoggerFactory.getLogger(ExpNodePostHandler.class);

    private String exp;
    
    public ExpNodePostHandler() {
	}
    public ExpNodePostHandler(String exp) {
    	this.exp = exp;
    }

	@Override
	public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
		List<String> result = ElFactory.get().eval(exp, nodeContext, context, null);
		return nodeIds2Nodes(result);
	}
	public String getExp() {
		return exp;
	}
	public void setExp(String exp) {
		this.exp = exp;
	}
	
	

}

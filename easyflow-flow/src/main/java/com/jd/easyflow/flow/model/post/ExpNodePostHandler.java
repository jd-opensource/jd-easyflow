package com.jd.easyflow.flow.model.post;

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
		Object result = ElFactory.get().eval(exp, nodeContext, context, null);
		return parseToNodes(result, nodeContext, context);
	}
	public String getExp() {
		return exp;
	}
	public void setExp(String exp) {
		this.exp = exp;
	}
	
	

}

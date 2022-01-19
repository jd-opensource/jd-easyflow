package com.jd.easyflow.flow.model.action;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class ExpNodeAction implements NodeAction {
	
	private static final Logger logger = LoggerFactory.getLogger(ExpNodeAction.class);
	
    private String exp;
    
    public ExpNodeAction() {
	}
    public ExpNodeAction(String exp) {
    	this.exp = exp;
    }

	@Override
	public <T>T execute(NodeContext nodeContext, FlowContext context) {
		Object result = ElFactory.get().eval(exp, nodeContext, context, null);
		return (T) result;
	}
	public String getExp() {
		return exp;
	}
	public void setExp(String exp) {
		this.exp = exp;
	}
	
	

}

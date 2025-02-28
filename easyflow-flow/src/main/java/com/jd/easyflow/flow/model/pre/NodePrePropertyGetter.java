package com.jd.easyflow.flow.model.pre;

import java.util.List;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public interface NodePrePropertyGetter {
    
    String getCheckType(NodeContext nodeContext, FlowContext flowContext);
        
    List<String> getPreNodes(NodeContext nodeContext, FlowContext flowContext);
    
}

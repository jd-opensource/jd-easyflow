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
    
    /**
     * used for static compute.
     * @return
     */
    String getCheckType();
    
    /**
     * used for static compute.
     * @return should contains all nodes in getPreNodes(nodeContext, flowContext)
     */
    List<String> getPreNodes();
        
    /**
     * used for runtime.
     * @param nodeContext
     * @param flowContext
     * @return All nodes should in getPreNodes().
     */
    List<String> getPreNodes(NodeContext nodeContext, FlowContext flowContext);
    
}

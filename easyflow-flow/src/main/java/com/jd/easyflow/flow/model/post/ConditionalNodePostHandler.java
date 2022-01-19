package com.jd.easyflow.flow.model.post;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;

/**
 * 
 * @author liyuliang5
 *
 */
public class ConditionalNodePostHandler extends AbstractNodePostHandler {
    
    private static final Logger logger = LoggerFactory.getLogger(ConditionalNodePostHandler.class);

    public static final String EXCLUSIVE_TYPE = "exclusive";

    public static final String INCLUSIVE_TYPE = "inclusive";

    private String type;

    private List<Map<String, Object>> branchList;

    private Object defaultBranch;

    public ConditionalNodePostHandler() {
    }

    public ConditionalNodePostHandler(Map<String, Object> branch) {
        this.branchList = Arrays.asList(branch);
    }

    public ConditionalNodePostHandler(List<Map<String, Object>> branchList) {
        this.branchList = branchList;
    }
    
    public ConditionalNodePostHandler(String type, List<Map<String, Object>> branchList, Object defaultBranch) {
        this.type = type;
        this.branchList = branchList;
        this.defaultBranch = defaultBranch;
    }   

    @Override
    public NodeContext[] postHandle(NodeContext nodeContext, FlowContext context) {
        // Exclusive
        if (type == null || type.equals(EXCLUSIVE_TYPE)) {
            for (Map<String, Object> branch : branchList) {
                boolean result = evalCondition(branch.get("when"), nodeContext, context);
                if (result) {
                    Object next = branch.get("to");
                    return nodeIds2Nodes(parseToNodeIds(next, nodeContext, context));
                }
            }
            if (defaultBranch != null) {
                return nodeIds2Nodes(parseToNodeIds(defaultBranch, nodeContext, context));
            }
            // Inclusive
        } else {
            List<String> nextList = new ArrayList<>();
            for (Map<String, Object> branch : branchList) {
                boolean result = evalCondition(branch.get("when"), nodeContext, context);
                if (result) {
                    Object next = branch.get("to");
                        nextList.addAll(parseToNodeIds(next, nodeContext, context));
                }
            }
            if (nextList.isEmpty() && defaultBranch != null) {
                nextList.addAll(parseToNodeIds(defaultBranch, nodeContext, context));
            }
            if (!nextList.isEmpty()) {
                return nodeIds2Nodes(nextList);
            }
        }
        return null;
    }
    
    /**
     * Evaluate condition.
     * @param nodeContext
     * @param context
     * @return
     */
    private boolean evalCondition(Object condition, NodeContext nodeContext, FlowContext context) {
        if (condition == null) {
            return true;
        }
        if (condition instanceof String) {
            return ElFactory.get().eval((String) condition, nodeContext, context, null);
        }
        return ((NodeExecutor<Boolean>) condition).execute(nodeContext, context);
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Object getDefaultBranch() {
        return defaultBranch;
    }

    public void setDefaultBranch(Object defaultBranch) {
        this.defaultBranch = defaultBranch;
    }

    public List<Map<String, Object>> getBranchList() {
        return branchList;
    }

    public void setBranchList(List<Map<String, Object>> branchList) {
        this.branchList = branchList;
    }
    
    

}
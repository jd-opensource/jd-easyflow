package com.jd.easyflow.flow.model.post;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.util.FlowUtil;

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
    
    private List<Branch> branchInfoList;

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
        if (branchInfoList == null) {
            init(nodeContext, context);
        }
        // Exclusive
        if (type == null || type.equals(EXCLUSIVE_TYPE)) {
            for (Branch branch : branchInfoList) {
                boolean result = branch.when == null ? true : branch.when.execute(nodeContext, context);
                if (result) {
                    return parseToNodes(branch.to, nodeContext, context);
                }
            }
            if (defaultBranch != null) {
                return parseToNodes(defaultBranch, nodeContext, context);
            }
            // Inclusive
        } else {
            List<NodeContext> nextList = new ArrayList<>();
            for (Branch branch : branchInfoList) {
                boolean result = branch.when == null ? true : branch.when.execute(nodeContext, context);
                if (result) {
                    addArray2List(parseToNodes(branch.to, nodeContext, context), nextList);
                }
            }
            if (nextList.isEmpty() && defaultBranch != null) {
                addArray2List(parseToNodes(defaultBranch, nodeContext, context), nextList);
            }
            if (!nextList.isEmpty()) {
                return list2Array(nextList);
            }
        }
        return null;
    }

    
    private void init(NodeContext nodeContext, FlowContext context) {
        InitContext initContext = new InitContext();
        initContext.setFlowParser(context.getFlowEngine().getFlowParser());
        initContext.setParseEl(true);
        FlowNode node = FlowUtil.node(nodeContext, context);
        init(initContext, node);
    }
    
    @Override
    public void init(InitContext initContext, Object parent) {
        List<Branch> branchInfoList = new ArrayList<ConditionalNodePostHandler.Branch>();
        for (Map<String, Object> branch : branchList) {
            Branch branchInfo = new Branch();
            Object whenObj = branch.get("when");
            if (whenObj == null) {
                branchInfo.when = null;
            } else if (whenObj instanceof String) {
                branchInfo.when = new ExpWhen((String) whenObj);
            } else if (whenObj instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) whenObj;
                String type = (String) map.get(DefConstants.COMMON_PROP_TYPE);
                String createExp = (String) map.get(DefConstants.COMMON_PROP_CREATE_EXP);
                if (DefConstants.COMMON_PROP_CREATE.equals(type) || createExp != null) {
                    if (initContext.isParseEl()) {
                        Map<String, Object> context = new HashMap<>(3);
                        context.put("definition", map);
                        context.put("node", (FlowNode) parent);
                        context.put("flow", initContext.getFlow());
                        context.put("flowParser", initContext.getFlowParser());
                        NodeExecutor<Boolean> executor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, context, false);
                        branchInfo.when = new ExecutorWhen(executor);
                    }
                } else {
                    throw new IllegalArgumentException("illegal param " + branch);
                }
            } else if (whenObj instanceof NodeExecutor) {
                branchInfo.when = new ExecutorWhen((NodeExecutor) whenObj);
            } else {
                throw new IllegalArgumentException("illegal param " + branch);
            }
            
            branchInfo.to = parseToDefinition(branch.get("to"), (FlowNode) parent, initContext);
            branchInfoList.add(branchInfo);
        }
        this.branchInfoList = branchInfoList;
        if (this.defaultBranch != null) {
            this.defaultBranch = parseToDefinition(defaultBranch, (FlowNode) parent, initContext);
        }
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

    private static class Branch {
        private When when;
        private Object to;
    }
    
    private static interface When {
        
        public boolean execute(NodeContext nodeContext, FlowContext context);
    }
    
    private static class ExpWhen implements When {
        
        String exp;
        
        ExpWhen(String exp) {
            this.exp = exp;
        }

        @Override
        public boolean execute(NodeContext nodeContext, FlowContext context) {
            return context.getElEvaluator().eval(exp, nodeContext, context, null);
        }
        
    }
    
    private static class ExecutorWhen implements When {
        
        NodeExecutor<Boolean> executor;
        
        ExecutorWhen(NodeExecutor<Boolean> executor) {
            this.executor = executor;
        }

        @Override
        public boolean execute(NodeContext nodeContext, FlowContext context) {
            return executor.execute(nodeContext, context);
        }
        
        
    }
    
}
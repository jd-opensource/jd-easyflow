package com.jd.easyflow.flow.model.action;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;

/**
 * 
 * IMPORTANT NOTICE! This class should not be singleton!
 * @author liyuliang5
 */
public class LoopNodeAction implements NodeAction {

    private static final Logger logger = LoggerFactory.getLogger(LoopNodeAction.class);

    // Condition
    private Boolean testBefore;
    private String loopConditionExp;
    private NodeExecutor<Boolean> loopConditionExecutor;  

    // MaxCount
    private Long loopMaxCount;
    private String loopMaxCountExp;
    private NodeExecutor<Long> loopMaxCountExecutor;

    // Action
    protected NodeAction loopAction;
    
    private String loopPreExp;
    private NodeExecutor<Object> loopPreExecutor;

    private String loopPostExp;
    private NodeExecutor<Object> loopPostExecutor;

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        // compute loop maximum
        Long maximum = null;
        if (loopMaxCountExecutor != null) {
            maximum = loopMaxCountExecutor.execute(nodeContext, context);
        } else if (loopMaxCountExp != null) {
            maximum = ((Number) context.getElEvaluator().eval(loopMaxCountExp, nodeContext, context, null)).longValue();
        } else if (loopMaxCount != null) {
            maximum = loopMaxCount;
        }
        if (context.isLogOn() && logger.isDebugEnabled()) {
            logger.debug("Loop maximum:" + loopMaxCount);
        }

        if (maximum == null && loopConditionExp == null && loopConditionExecutor == null) {
            throw new FlowException("Loop maximum and condition can not both be null");
        }
        
        if (loopPreExecutor != null) {
            loopPreExecutor.execute(nodeContext, context);
        } else if (loopPreExp != null) {
            context.getElEvaluator().eval(loopPreExp, nodeContext, context, null);
        }
        
        for (long i = 0; maximum == null || i < maximum; i++) {
            nodeContext.put("currentLoopIndex", i);
            // test before
            if (testBefore) {
                if (loopConditionExecutor != null) {
                    boolean testResult = loopConditionExecutor.execute(nodeContext, context);
                    if (!testResult) {
                        break;
                    }
                } else if (loopConditionExp != null) {
                    boolean testResult = context.getElEvaluator().eval(loopConditionExp, nodeContext, context, null);
                    if (!testResult) {
                        break;
                    }
                }
            }

            // execute
            loopAction.execute(nodeContext, context);

            // test after
            if (!testBefore) {
                if (loopConditionExecutor != null) {
                    boolean testResult = loopConditionExecutor.execute(nodeContext, context);
                    if (!testResult) {
                        break;
                    }
                } else if (loopConditionExp != null) {
                    boolean testResult = context.getElEvaluator().eval(loopConditionExp, nodeContext, context, null);
                    if (!testResult) {
                        break;
                    }
                }
            }
        }
        
        if (loopPostExecutor != null) {
            loopPostExecutor.execute(nodeContext, context);
        } else if (loopPostExp != null) {
            context.getElEvaluator().eval(loopPostExp, nodeContext, context, null);
        }
        // result put to NodeContext
        return null;
    }

    @Override
    public void init(InitContext initContext, Object parent) {
        FlowNode node = (FlowNode) parent;
        testBefore = node.getProperty("loopTestBefore");
        if (testBefore == null) {
            throw new FlowException("Test before can not be null");
        }
        loopConditionExp = node.getProperty("loopConditionExp");
        Map<String, Object> loopConditionExecutorConf = (Map<String, Object>) node
                .getProperty("loopConditionExecutor");
        if (loopConditionExecutorConf != null) {
            String createExp = (String) loopConditionExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopConditionExecutor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, initContext, false);
            }
        }
        
        loopPreExp = node.getProperty("loopPreExp");
        Map<String, Object> loopPreExecutorConf = (Map<String, Object>) node
                .getProperty("loopPreExecutor");
        if (loopPreExecutorConf != null) {
            String createExp = (String) loopPreExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopPreExecutor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, initContext, false);
            }
        }        

        loopPostExp = node.getProperty("loopPostExp");
        Map<String, Object> loopPostExecutorConf = (Map<String, Object>) node
                .getProperty("loopPostExecutor");
        if (loopPostExecutorConf != null) {
            String createExp = (String) loopPostExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopPostExecutor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, initContext, false);
            }
        }  
        
        
        Object loopMaxCountConf = node.getProperty("loopMaxCount");
        if (loopMaxCountConf != null) {
            loopMaxCount = ((Number)loopMaxCount).longValue();
        }
        loopMaxCountExp = node.getProperty("loopMaxCountExp");
        Map<String, Object> loopMaxCountExecutorConf = (Map<String, Object>) node.getProperty("loopMaxCountExecutor");
        if (loopMaxCountExecutorConf != null) {
            String createExp = (String) loopMaxCountExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopMaxCountExecutor = initContext.getFlowParser().getElEvaluator().evalWithDefaultContext(createExp, initContext, false);
            }
        }
        Object loopActionConf = node.getProperty("loopAction");
        if (loopActionConf != null) {
            ActionParseParam param = new ActionParseParam(loopActionConf, initContext.getFlowList(),
                    initContext.isParseEl(), node);
            loopAction = initContext.getFlowParser().parseNodeAction(param);
        }
        
        if (loopAction == null) {
            throw new FlowException("Loop node action can not be null");
        }
        loopAction.init(initContext, node);
        if (loopMaxCount == null && loopMaxCountExp == null && loopMaxCountExecutor == null && loopConditionExp == null && loopConditionExecutor == null) {
            throw new FlowException("Loop maximum and loop condition can not both be null");
        }
        
    }

    public NodeAction getLoopAction() {
        return loopAction;
    }

    public void setLoopAction(NodeAction loopAction) {
        this.loopAction = loopAction;
    }
    
    
    

}

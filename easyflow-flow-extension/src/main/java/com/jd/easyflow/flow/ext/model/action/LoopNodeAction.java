package com.jd.easyflow.flow.ext.model.action;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;

/**
 * Loop node action
 * 
 * @author liyuliang5
 *
 */
public class LoopNodeAction implements NodeAction {

    private static final Logger logger = LoggerFactory.getLogger(LoopNodeAction.class);

    // Condition
    private boolean testBefore = false;
    private String loopConditionExp;
    private NodeExecutor<Boolean> loopConditionExecutor;
    
    private String loopPreExp;
    private NodeExecutor<Object> loopPreExecutor;

    private String loopPostExp;
    private NodeExecutor<Object> loopPostExecutor;
    

    // Maximum
    private Long loopMaximum;
    private String loopMaximumExp;
    private NodeExecutor<Long> loopMaximumExecutor;

    // Action
    protected NodeAction loopAction;

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        // compute loop maximum
        Long maximum = null;
        if (loopMaximumExecutor != null) {
            maximum = loopMaximumExecutor.execute(nodeContext, context);
        } else if (loopMaximumExp != null) {
            maximum = ((Number) ElFactory.get().eval(loopMaximumExp, nodeContext, context, null)).longValue();
        } else if (loopMaximum != null) {
            maximum = loopMaximum;
        }
        if (logger.isDebugEnabled()) {
            logger.debug("Loop maximum:" + loopMaximum);
        }

        if (maximum == null && loopConditionExp == null && loopConditionExecutor == null) {
            throw new FlowException("Loop maximum and condition can not both be null");
        }
        
        if (loopPreExecutor != null) {
            loopPreExecutor.execute(nodeContext, context);
        } else if (loopPreExp != null) {
            ElFactory.get().eval(loopPreExp, nodeContext, context, null);
        }
        
        for (int i = 0; maximum == null || i < maximum; i++) {
            nodeContext.put("currentLoopIndex", i);
            // test before
            if (testBefore) {
                if (loopConditionExecutor != null) {
                    boolean testResult = loopConditionExecutor.execute(nodeContext, context);
                    if (!testResult) {
                        break;
                    }
                } else if (loopConditionExp != null) {
                    boolean testResult = ElFactory.get().eval(loopConditionExp, nodeContext, context, null);
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
                    boolean testResult = ElFactory.get().eval(loopConditionExp, nodeContext, context, null);
                    if (!testResult) {
                        break;
                    }
                }
            }
        }
        
        if (loopPostExecutor != null) {
            loopPostExecutor.execute(nodeContext, context);
        } else if (loopPostExp != null) {
            ElFactory.get().eval(loopPostExp, nodeContext, context, null);
        }
        // result put to NodeContext
        return null;
    }

    public void init(InitContext initContext, FlowNode node) {
        Boolean testBeforeConf = node.getProperty("loopTestBefore");
        if (testBeforeConf != null) {
            testBefore = testBeforeConf;
        }
        loopConditionExp = node.getProperty("loopConditionExp");
        Map<String, Object> loopConditionExecutorConf = (Map<String, Object>) node
                .getProperty("loopConditionExecutor");
        if (loopConditionExecutorConf != null) {
            String createExp = (String) loopConditionExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopConditionExecutor = ElFactory.get().evalWithDefaultContext(createExp, initContext, false);
            }
        }
        
        loopPreExp = node.getProperty("loopPreExp");
        Map<String, Object> loopPreExecutorConf = (Map<String, Object>) node
                .getProperty("loopPreExecutor");
        if (loopPreExecutorConf != null) {
            String createExp = (String) loopPreExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopPreExecutor = ElFactory.get().evalWithDefaultContext(createExp, initContext, false);
            }
        }        

        loopPostExp = node.getProperty("loopPostExp");
        Map<String, Object> loopPostExecutorConf = (Map<String, Object>) node
                .getProperty("loopPostExecutor");
        if (loopPostExecutorConf != null) {
            String createExp = (String) loopPostExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopPostExecutor = ElFactory.get().evalWithDefaultContext(createExp, initContext, false);
            }
        }  
        
        
        Object loopMaximumConf = node.getProperty("loopMaximum");
        if (loopMaximumConf != null) {
            loopMaximum = ((Number)loopMaximum).longValue();
        }
        loopMaximumExp = node.getProperty("loopMaximumExp");
        Map<String, Object> loopMaximumExecutorConf = (Map<String, Object>) node.getProperty("loopMaximumExecutor");
        if (loopMaximumExecutorConf != null) {
            String createExp = (String) loopMaximumExecutorConf.get("createExp");
            if (initContext.isParseEl() && createExp != null) {
                loopMaximumExecutor = ElFactory.get().evalWithDefaultContext(createExp, initContext, false);
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
        if (loopMaximum == null && loopMaximumExp == null && loopMaximumExecutor == null && loopConditionExp == null && loopConditionExecutor == null) {
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

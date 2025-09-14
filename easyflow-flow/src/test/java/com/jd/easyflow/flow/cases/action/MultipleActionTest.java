package com.jd.easyflow.flow.cases.action;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.parser.param.ActionParseParam;

/**
 * @author liyuliang5
 */
public class MultipleActionTest {

    
    @Test
    public void testMultipleActionNodeAction() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/action/multiple_action_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("multi_action_001");
        FlowResult result = flowEngine.execute(param);
    }
    
    
    
    
    public static class TestMultipleActionNodeAction implements NodeAction {

        // Action
        protected NodeAction action;
        
        @Override
        public <T> T execute(NodeContext nodeContext, FlowContext context) {
            for (int i = 0; i < 10; i++) {
                action.execute(nodeContext, context);
            }
            return null;
        }
        

        @Override
        public void init(InitContext initContext, Object parent) {
            
            FlowNode node = (FlowNode) parent;
            Object actionConf = node.getProperty("action");
            if (actionConf != null) {
                ActionParseParam param = new ActionParseParam(actionConf, initContext.getFlowList(),
                        initContext.isParseEl(), initContext.getFlow(), node);
                action = initContext.getFlowParser().parseNodeAction(param);
            }
            if (action != null) {
                action.init(initContext, node);
            }
            if (initContext.isParseEl()) {
                if (action == null) {
                    throw new FlowException("Action can not be null");
                }
            }
            
        }

        
    }
}

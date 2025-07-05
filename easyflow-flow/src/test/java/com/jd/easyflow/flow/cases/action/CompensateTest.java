package com.jd.easyflow.flow.cases.action;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.compensate.CompensateAction;
import com.jd.easyflow.flow.model.action.compensate.CompensateFlowParseEventListener;
import com.jd.easyflow.flow.model.action.compensate.CompensateHelper;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowUtil;

/**
 * @author liyuliang5
 */
public class CompensateTest {
    
    private static final Logger logger = LoggerFactory.getLogger(CompensateTest.class);

    @Test
    public void testCompensate() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:/flow/cases/action/compensate_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("compensate_001");
        FlowResult result = flowEngine.execute(param);
        assertEquals("node001", result.getContext().getEndNodes().get(0).getNodeId());
        
    }
    
    @Test
    public void testCompensate002() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:/flow/cases/action/compensate_002.json");
        flowEngine.init();
        FlowParam param = new FlowParam("compensate_002");
        FlowResult result = flowEngine.execute(param);
        
    }
    
    @Test
    public void testCompensateFlow() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        FlowParserImpl flowParser = new FlowParserImpl();
        flowParser.setPostListeners(Arrays.asList(new CompensateFlowParseEventListener()));
        flowEngine.setFlowParser(flowParser);
        flowEngine.setFlowPath("classpath:/flow/cases/action/compensate_flow_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("compensate_flow_001");
        FlowResult result = flowEngine.execute(param);
        
        logger.info("start compensate flow");
        List<NodeContext> endNodes = result.getContext().getEndNodes();
        List<NodeContext> startNodes = new ArrayList<NodeContext>();
        for (NodeContext endNode : endNodes) {
            NodeContext startNode = CompensateHelper.createCompensateNode(endNode);
            startNodes.add(startNode);
        }
        FlowParam param2 = new FlowParam();
        result.getContext().setStartNodes(startNodes);
        param2.setContext(result.getContext());
        FlowResult compensateResult = result.getContext().getFlowEngine().execute(param2);
        
        
    }
    
    @Test
    public void testCompensateFlowFilter() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        FlowParserImpl flowParser = new FlowParserImpl();
        flowParser.setPostListeners(Arrays.asList(new CompensateFlowParseEventListener()));
        flowEngine.setFlowParser(flowParser);
        flowEngine.setFlowPath("classpath:/flow/cases/action/compensate_flow_001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("compensate_flow_001");
        FlowResult result = flowEngine.execute(param);
        
        logger.info("start compensate flow");
        CompensateHelper.compensate(result.getContext());
        FlowParam param2 = new FlowParam();
        param2.setContext(result.getContext());
        FlowResult compensateResult = result.getContext().getFlowEngine().execute(param2);
        
        
    }
    
    public static class TestCompensateNodeAction implements NodeAction, CompensateAction {
        
        public TestCompensateNodeAction() {
        }

        @Override
        public <T> T execute(NodeContext nodeContext, FlowContext context) {
            logger.info("execute:" + FlowUtil.node(nodeContext, context).getId());
            return null;
        }

        @Override
        public <T> T compensate(NodeContext nodeContext, FlowContext context) {
            logger.info("compensate:" + FlowUtil.node(nodeContext, context).getId());
            return null;
        }
        
    }
   
}

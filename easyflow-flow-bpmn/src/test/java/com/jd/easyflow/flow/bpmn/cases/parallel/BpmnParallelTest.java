package com.jd.easyflow.flow.bpmn.cases.parallel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.model.parser.param.FlowParseParam;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnParallelTest {
    
    private static final Logger logger = LoggerFactory.getLogger(BpmnParallelTest.class);

    /**
     * test bpmn parallel nodes.
     */
    @Test
    public void testParallel001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/parallel/flow_parallel001.bpmn");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_bpmn_parallel001");
        FlowResult result = flowEngine.execute(param);
    }
    
    
    @Test
    public void testMultiParallelSubFlow001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/parallel/flow_multi_parallel_subflow_001.bpmn");
        flowEngine.init();
        
        FlowParam param = new FlowParam("multi_parallel_subflow_001");
        param.put("recordList", Arrays.asList(1, 2, 3, 4, 5, 6));
        FlowResult result = flowEngine.execute(param);
    }
    
    
    
    public static class MainFlowNodeAction implements NodeAction {
        
        private Flow flow;
        
        @Override
        public <T> T execute(NodeContext nodeContext, FlowContext context) {
            List<Integer> recordList = context.getParam().get("recordList");
            Map<Integer, Object> result = new ConcurrentHashMap<Integer, Object>();
            CountDownLatch latch = new CountDownLatch(recordList.size());
            Executor executor = Executors.newFixedThreadPool(3);
            for (Integer i : recordList) {
                executor.execute(() -> {
                    try {
                        FlowParam subParam = new FlowParam();
                        subParam.put("record", i);
                        FlowContext subContext = new FlowContextImpl();
                        subContext.setFlow(flow);
                        subParam.setContext(subContext);
                        FlowResult flowResult = context.getFlowEngine().execute(subParam);
                        result.put(i, flowResult.get("subResult"));
                    } finally {
                        latch.countDown();
                    }
                });
            }
            try {
                latch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            logger.info("all subResult:" + result);
            return null;
        }
        
        @Override
        public void init(InitContext initContext, Object parent) {
            FlowNode node = (FlowNode) parent;
            Map<String, Object> flowDef = (Map<String, Object>) node.getProperty(DefConstants.COMMON_PROP_FLOW);
            FlowParseParam param = new FlowParseParam();
            param.setObjectDefinition(flowDef);
            param.setParseEl(initContext.isParseEl());
            List<Flow> flowList = initContext.getFlowParser().parse(param);
            initContext.getFlowList().addAll(flowList);
            Flow subFlow = flowList.get(0);
            flow = subFlow;
        }
        
    }
    
    public static class SubFlowNodeAction implements NodeAction {

        @Override
        public <T> T execute(NodeContext nodeContext, FlowContext context) {
            logger.info("subFlow");
            int param = context.getParam().get("record");
            context.getResult().put("subResult", param * param);
            
            return null;
        }
        
    }
    
}

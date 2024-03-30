package com.jd.easyflow.flow.cases.flowengine;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.builder.FlowParamBuilder;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.action.ExecutorNodeAction;
import com.jd.easyflow.flow.model.builder.FlowBuilder;
import com.jd.easyflow.flow.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowEngineImplTest {
	
	public static final Logger logger = LoggerFactory.getLogger(FlowEngineImplTest.class);

    /**
     * 
     * Basic flow engine test.
     * Steps：define flow, execute flow, validate result
     *
     */
	@Test
    public void testSample() {
    	//define flow
        Flow flow = FlowBuilder.create("test", "testName")
        		//add node
        		.addNode("node1", new ExecutorNodeAction((nc, c) -> {logger.info("hello"); return "hello";}))
        		.build();
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.addFlow(flow);
        flowEngine.init();
        
        // create param
        FlowParam param = FlowParamBuilder.create("test", "node1").build();
        // execute flow
        FlowResult result = flowEngine.execute(param);
        // print result
        logger.info("Result:" + result);
        
    }
	
	/**
	 * Change flowId from test to test2 by filter.
	 */
	@Test
	public void testFlowEngineFilter() {
	    Flow flow1 = FlowBuilder.create("test", null).build();
	    Flow flow2 = FlowBuilder.create("test2", null).build();
	    FlowEngineImpl flowEngine = new FlowEngineImpl();
	    flowEngine.addFlow(flow1);
	    flowEngine.addFlow(flow2);
	    flowEngine.setFilters(Arrays.asList(new TestFlowEngineFilter()));
	    flowEngine.init();
	    
	    FlowParam param = new FlowParam("test", new String[] {}, null);
	    flowEngine.execute(param);
	    assertEquals("test2", param.getFlowId());
	}
}

class TestFlowEngineFilter implements Filter<Pair<FlowParam, FlowEngine>, FlowResult> {

    @Override
    public FlowResult doFilter(Pair<FlowParam, FlowEngine> request,
            FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain) {
        request.getLeft().setFlowId("test2");
        return chain.doFilter(request);
    }
    
}

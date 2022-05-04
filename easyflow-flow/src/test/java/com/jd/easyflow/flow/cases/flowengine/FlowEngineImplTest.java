package com.jd.easyflow.flow.cases.flowengine;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.builder.FlowParamBuilder;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.action.ExecutorNodeAction;
import com.jd.easyflow.flow.model.builder.FlowBuilder;

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
        
        // create param
        FlowParam param = FlowParamBuilder.create("test", "node1").build();
        // execute flow
        FlowResult result = flowEngine.execute(param);
        // print result
        logger.info("Result:" + result);
        
    }
}

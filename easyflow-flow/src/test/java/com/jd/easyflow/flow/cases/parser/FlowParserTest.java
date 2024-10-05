package com.jd.easyflow.flow.cases.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowIOUtil;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class FlowParserTest {

	private static final Logger logger = LoggerFactory.getLogger(FlowParserTest.class);

	@Test
    public void testParseFlow() throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        InputStream is = null;
        resources = resolver.getResources("classpath:flow/cases/parser/parser_test_001.json");
        for (Resource resource : resources) {
            logger.info("Start parse flow definition:" + resource.getURI());
            is = resource.getInputStream();
            String flowConfigStr = FlowIOUtil.toString(is);
            List<Flow> flowList = new FlowParserImpl().parse(flowConfigStr);
            logger.info("Parse end, model:" + JsonUtil.toJsonString(flowList));
            is.close();
        }
    }
	
	@Test
    public void testFlowParseListener() throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        InputStream is = null;
        Flow flow = null;
        resources = resolver.getResources("classpath:flow/cases/parser/parser_test_002.json");
        for (Resource resource : resources) {
            logger.info("Start parse flow definition:" + resource.getURI());
            is = resource.getInputStream();
            String flowConfigStr = FlowIOUtil.toString(is);
            List<Flow> flowList = new FlowParserImpl().parse(flowConfigStr);
            logger.info("Parse end, model:" + JsonUtil.toJsonString(flowList));
            flow = flowList.get(0);

        }

        assertEquals("node001", flow.getNodeList().get(0).getName());
        assertEquals("node001", flow.getStartNodeIds()[0]);

    }
	
	@Test
    public void testMultiple() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/parser/flow_multiple001.json");
        flowEngine.init();
        assertTrue(flowEngine.getFlowMap().size() == 2);

        FlowParam param1 = new FlowParam("flow_multiple0011");
        flowEngine.execute(param1);

        FlowParam param2 = new FlowParam("flow_multiple0012");
        flowEngine.execute(param2);
    }
	
	   @Test
	    public void testAddFilterForAllFlow() throws Exception {
	        FlowEngineImpl flowEngine = new FlowEngineImpl();
	        flowEngine.setFlowPath("classpath:flow/cases/parser/parser_test_003.json");
	        flowEngine.setFlowParser(new TestFlowParserImpl());
	        flowEngine.init();
	        
	        FlowParam param = new FlowParam("parser_test_003", new String[] {}, null);
	        flowEngine.execute(param);
	    }
	
	
}

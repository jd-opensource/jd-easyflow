package com.jd.easyflow.flow.cases.spring;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;

/**
 * 
 * @author liyuliang5
 */
public class SpringFlowTest {
    
    private static final Logger logger = LoggerFactory.getLogger(SpringFlowTest.class);

    @Test
    public void testIntegrationWithSpring() {
        ApplicationContext context = new ClassPathXmlApplicationContext("classpath:flow/cases/spring/applicationContext-flow-test.xml");
        FlowEngine flowEngine = context.getBean(FlowEngine.class);
        
        FlowParam param = new FlowParam("flow_spring_test_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
        
    }
}

package com.jd.easyflow.flow.cases.spring;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.quickstart.QuickStart001Node01Action;
import com.jd.easyflow.flow.quickstart.QuickStart002Node01Action;
import com.jd.easyflow.flow.quickstart.QuickStart003Node01Action;

/**
 * 
 * @author liyuliang5
 */
public class SpringFlowTest {
    
    private static final Logger logger = LoggerFactory.getLogger(SpringFlowTest.class);

    @Test
    public void testIntegrationWithSpringXml() {
        ApplicationContext context = new ClassPathXmlApplicationContext("classpath:flow/cases/spring/applicationContext-flow-test.xml");
        FlowEngine flowEngine = context.getBean(FlowEngine.class);
        
        FlowParam param = new FlowParam("flow_spring_test_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }
    
    @Test
    public void testIntegrationWithSpringAnnotation() {
        ApplicationContext context = new AnnotationConfigApplicationContext(SpringConfig.class);
        FlowEngine flowEngine = context.getBean(FlowEngine.class);
        
        FlowParam param = new FlowParam("flow_spring_test_001");
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute finish, current node is:" + result.getContext().getEndNodes().get(0).getNodeId());
        assertEquals("node003", result.getContext().getEndNodes().get(0).getNodeId());
    }
    
    
    @Configuration
    public static class SpringConfig {

        @Bean
        public FlowEngine flowEngine() {
            FlowEngineImpl flowEngine = new FlowEngineImpl();
            flowEngine.setFlowPath("classpath:flow/cases/spring/flow_spring_001.json");
            return flowEngine;
        }

        @Bean
        public QuickStart001Node01Action quickStart001Node01Action() {
            return new QuickStart001Node01Action();
        }

        @Bean
        public QuickStart002Node01Action quickStart002Node01Action() {
            return new QuickStart002Node01Action();
        }

        @Bean
        public QuickStart003Node01Action quickStart003Node01Action() {
            return new QuickStart003Node01Action();
        }
    }

}

package com.jd.easyflow.flow.ext.cases.funcall;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;

/**
 * 
 * @author liyuliang5
 *
 */
public class FunCallTest {
    
    private static final Logger logger = LoggerFactory.getLogger(FunCallTest.class);

    /**
     * Test customize action.
     */
    @Test
    public void testFunCall001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/funcall/funcall_test_001.json");
        flowEngine.init();
        
        Map<String, Object> bizParam = new HashMap<>();
        bizParam.put("a", "a1");
        bizParam.put("b", "b1");
        logger.info("请求参数:" + bizParam);
        FlowParam param = new FlowParam("funcall_test_001", bizParam);
        FlowResult result = flowEngine.execute(param);
        logger.info("响应结果:" + result.getResult());
    }
    
    /**
     * Test customize action.
     */
    @Test
    public void testFunCall002() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/funcall/funcall_test_002.json");
        flowEngine.init();
        
        Map<String, Object> bizParam = new HashMap<>();
        bizParam.put("a", "a1");
        bizParam.put("b", "b1");
        logger.info("请求参数:" + bizParam);
        FlowParam param = new FlowParam("funcall_test_002", bizParam);
        FlowResult result = flowEngine.execute(param);
        logger.info("响应结果:" + result.getResult());
    }
}

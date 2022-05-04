package com.jd.easyflow.flow.bpmn.cases.converter;

import java.io.IOException;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.util.JsonPrettyHelper;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnConverterTest {
    
    private static final Logger logger = LoggerFactory.getLogger(BpmnConverterTest.class);

    /**
     * 验证转换过程
     * @throws IOException
     */
    @Test
    public void testConvert() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/converter/process_1.bpmn");
        Map<String, Object> model = BpmnConverter.convert(resource.getInputStream());
        String flowPrettyConf =  IOUtils.toString(BpmnConverterTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
        String pretty = JsonPrettyHelper.pretty(model, JsonUtil.parseObject(flowPrettyConf, Map.class));
        logger.info("Model is:" + pretty);
    }
    
    /**
     * Validate convert.
     * @throws IOException
     */
    @Test
    public void testConvertSimple() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/converter/process_1_simple.bpmn");
        Map<String, Object> model = BpmnConverter.convert(resource.getInputStream());
        logger.info("Model is:" + JsonUtil.toJsonString(model));
    }
    
    /**
     * Validate Convert result.
     */
    @Test
    public void testConvertResult() {
        // Init flow engine
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowParser(new BpmnFlowParser());
        flowEngine.setFlowPath("classpath:flow/cases/converter/flow001.json, classpath:flow/cases/converter/process_1.bpmn");
        flowEngine.init();
        // Execute flow instance 1.
        FlowParam param = new FlowParam("process_1");
        param.put("input", 1);
        FlowResult result = flowEngine.execute(param);
        logger.info("Execute flow instance 1 result:" + result);
        // Execute flow instance 2.
        FlowParam param2 = new FlowParam("process_1");
        param2.put("input", 10);
        FlowResult result2 = flowEngine.execute(param2);
        logger.info("Execute flow instance 2 result:" + result2);        
    }
}

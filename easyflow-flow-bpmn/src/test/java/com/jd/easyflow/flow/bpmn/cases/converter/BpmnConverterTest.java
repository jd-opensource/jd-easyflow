package com.jd.easyflow.flow.bpmn.cases.converter;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.List;
import java.util.Map;

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
import com.jd.easyflow.flow.util.FlowIOUtil;
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
     * test convert.
     * @throws IOException
     */
    @Test
    public void testConvert() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/converter/process_1.bpmn");
        List<Map<String, Object>> model = BpmnConverter.convert(resource.getInputStream());
        String flowPrettyConf =  FlowIOUtil.toString(BpmnConverterTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
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
        List<Map<String, Object>> model = BpmnConverter.convert(resource.getInputStream());
        logger.info("Model is:" + JsonUtil.toJsonString(model));
    }
    
    /**
     * Converter bpmn with multiple process.
     * @throws IOException
     */
    @Test
    public void testConvertMultiple() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/converter/flow_multiple_001.bpmn");
        String modelString = BpmnConverter.convert(FlowIOUtil.toString(resource.getInputStream()));
        logger.info("Model is:" + modelString);
        List<Map<String, Object>> model = JsonUtil.parseObject(modelString, List.class);
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
    
    @Test
    public void testCompensate() throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/converter/compensate001.bpmn");
        List<Map<String, Object>> model = BpmnConverter.convert(resource.getInputStream());
        logger.info("Model is:" + JsonUtil.toJsonString(model));
        List<Map<String, Object>> nodeList = (List<Map<String, Object>>)model.get(0).get("nodes");
        Map<String, Object> node001 = nodeList.stream().filter(map -> map.get("id").equals("node001")).findFirst().get();
        assertEquals(((Map<String, Object>)((Map<String, Object>)node001.get("properties")).get("compensateAction")).get("createExp"), "@compensate1Action");
        Map<String, Object> node003 = nodeList.stream().filter(map -> map.get("id").equals("node003")).findFirst().get();
        assertEquals(((Map<String, Object>)node003.get("action")).get("type"), "compensate");
        Map<String, Object> node004 = nodeList.stream().filter(map -> map.get("id").equals("node004")).findFirst().get();
        assertEquals(((Map<String, Object>)node004.get("action")).get("type"), "compensate");
    }
   
}

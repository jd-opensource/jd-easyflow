package com.jd.easyflow.flow.bpmn.cases.throwevent;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.bpmn.cases.converter.BpmnConverterTest;
import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.flow.util.FlowIOUtil;
import com.jd.easyflow.flow.util.JsonPrettyHelper;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 */
public class BpmnThrowEventTest {


    private static final Logger logger = LoggerFactory.getLogger(BpmnThrowEventTest.class);

    /**
     * test convert.
     * @throws IOException
     */
    @Test
    public void testConvert() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/throwevent/flow_throwevent001.bpmn");
        List<Map<String, Object>> model = BpmnConverter.convert(resource.getInputStream());
        String flowPrettyConf =  FlowIOUtil.toString(BpmnConverterTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
        String pretty = JsonPrettyHelper.pretty(model, JsonUtil.parseObject(flowPrettyConf, Map.class));
        logger.info("Model is:" + pretty);
    }
}

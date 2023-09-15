package com.jd.easyflow.flow.bpmn.cases.extension;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.Process;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnExtensionTest {
    
    private static final Logger logger = LoggerFactory.getLogger(BpmnExtensionTest.class);

    @Test
    public void testExtension() {
        XMLStreamReader reader = null;
        try {
            reader = XMLInputFactory.newFactory().createXMLStreamReader(this.getClass().getResourceAsStream("/flow/cases/extension/process_1.bpmn"));
        } catch (XMLStreamException | FactoryConfigurationError e) {
            throw new FlowException("BPMN parse error", e);
        }
        // Parse BPMN
        BpmnXMLConverter bpmnXmlConverter = new BpmnXMLConverter();
        BpmnModel bpmnModel = bpmnXmlConverter.convertToBpmnModel(reader);
        // BPMN to Easy Flow
        Process process = bpmnModel.getProcesses().get(0);
    }
    
    @Test
    public void testConvert() throws IOException {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource resource = resolver.getResource("classpath:flow/cases/extension/process_1.bpmn");
        List<Map<String, Object>> model = BpmnConverter.convert(resource.getInputStream());
        logger.info("Model is:" + JsonUtil.toJsonString(model));
    }
}

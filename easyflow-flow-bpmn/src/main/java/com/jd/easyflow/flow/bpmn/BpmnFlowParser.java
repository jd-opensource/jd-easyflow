package com.jd.easyflow.flow.bpmn;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnFlowParser extends FlowParserImpl {

    public static final Logger logger = LoggerFactory.getLogger(BpmnFlowParser.class);

    private static final String FLOW_BPMN_STRING_KEY = "_flow_bpmn_string";

    private static final String DEFAULT_FLOW_PRETTY_CONFIG_PATH = "/pretty/pretty-flow.json";

    private static final String XML_PREFIX = "<?xml";

    private Map<String, Object> flowPrettyConfig;

    private String flowPrettyConfigPath;

    public BpmnFlowParser() {
        init();
    }

    public void init() {
        if (flowPrettyConfigPath == null) {
            flowPrettyConfigPath = DEFAULT_FLOW_PRETTY_CONFIG_PATH;
        }
        String flowPrettyConfStr = null;
        InputStream inputStream = BpmnFlowParser.class.getResourceAsStream(flowPrettyConfigPath);
        if (inputStream == null) {
            flowPrettyConfStr = BpmnConverter.defaultFlowPrettyConfigStr;
        } else {
            try {
                flowPrettyConfStr = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
            } catch (IOException e) {
                throw new FlowException(
                        "Pretty conf parse exception, path:" + flowPrettyConfigPath + " message:" + e.getMessage(), e);
            }
        }
        flowPrettyConfig = JsonUtil.parseObject(flowPrettyConfStr, Map.class);
    }

    @Override
    public List<Flow> parse(String data) {
        if (!data.trim().startsWith(XML_PREFIX)) {
            return super.parse(data);
        }
        try {
            logger.info("BPMN Definition:\n" + data);
            String easyFlowDef = BpmnConverter.convert(data, flowPrettyConfig);
            logger.info("EasyFlow Definition:\n" + easyFlowDef);
            List<Flow> flowList = super.parse(easyFlowDef);
            flowList.get(0).setProperty(FLOW_BPMN_STRING_KEY, data);
            return flowList;
        } catch (Exception e) {
            throw new FlowException(e);
        }
    }

    /**
     * 
     * BPMN Model to string
     *
     * @param flow
     * @return
     */
    public static String bpmnStringify(Flow flow) {
        if (flow.getProperty(FLOW_BPMN_STRING_KEY) != null) {
            return flow.getProperty(FLOW_BPMN_STRING_KEY);
        }
        return null;
    }

    public String getFlowPrettyConfigPath() {
        return flowPrettyConfigPath;
    }

    public void setFlowPrettyConfigPath(String flowPrettyConfigPath) {
        this.flowPrettyConfigPath = flowPrettyConfigPath;
    }

}

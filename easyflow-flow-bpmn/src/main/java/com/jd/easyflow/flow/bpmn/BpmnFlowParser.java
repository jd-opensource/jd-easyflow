package com.jd.easyflow.flow.bpmn;

import java.io.IOException;
import java.io.InputStream;
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

    private BpmnConverter bpmnConverter = new BpmnConverter();

    private static final String XML_PREFIX = "<?xml";

    private Map<String, Object> flowPrettyConfig;

    public BpmnFlowParser() {
        String flowPrettyConf;
        try {
            InputStream inputStream = BpmnFlowParser.class.getResourceAsStream("/pretty/pretty-flow.json");
            if (inputStream == null) {
                throw new FlowException("Pretty config not found");
            }
            flowPrettyConf = IOUtils.toString(inputStream);
        } catch (IOException e) {
            throw new FlowException("Pretty conf parse exception," + e.getMessage(), e);
        }
        flowPrettyConfig = JsonUtil.parseObject(flowPrettyConf, Map.class);
    }

    @Override
    public List<Flow> parse(String data) {
        if (!data.trim().startsWith(XML_PREFIX)) {
            return super.parse(data);
        }
        try {
            logger.info("BPMN Definition:\n" + data);
            String easyFlowDef = bpmnConverter.convert(data, flowPrettyConfig);
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
}

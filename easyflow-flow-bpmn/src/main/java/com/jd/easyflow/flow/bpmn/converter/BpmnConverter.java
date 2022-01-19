package com.jd.easyflow.flow.bpmn.converter;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.EndEvent;
import org.activiti.bpmn.model.ExclusiveGateway;
import org.activiti.bpmn.model.ExtensionElement;
import org.activiti.bpmn.model.FlowElement;
import org.activiti.bpmn.model.FlowNode;
import org.activiti.bpmn.model.InclusiveGateway;
import org.activiti.bpmn.model.IntermediateCatchEvent;
import org.activiti.bpmn.model.ParallelGateway;
import org.activiti.bpmn.model.Process;
import org.activiti.bpmn.model.ReceiveTask;
import org.activiti.bpmn.model.ScriptTask;
import org.activiti.bpmn.model.SequenceFlow;
import org.activiti.bpmn.model.StartEvent;
import org.activiti.bpmn.model.UserTask;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.activity.ReceiveTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.ScriptTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.UserTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.event.EndEventConverter;
import com.jd.easyflow.flow.bpmn.converter.event.IntermediateCatchEventConverter;
import com.jd.easyflow.flow.bpmn.converter.event.StartEventConverter;
import com.jd.easyflow.flow.bpmn.converter.gateway.ExclusiveGatewayConverter;
import com.jd.easyflow.flow.bpmn.converter.gateway.InclusiveGatewayConverter;
import com.jd.easyflow.flow.bpmn.converter.gateway.ParallelGatewayConverter;
import com.jd.easyflow.flow.bpmn.converter.util.BpmnXmlConstants;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.definition.DefConstants;
import com.jd.easyflow.flow.util.JsonPrettyHelper;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class BpmnConverter {
    
    private static final Logger logger = LoggerFactory.getLogger(BpmnConverter.class);

    private static Map<Class, FlowNodeConverter> flowNodeConverterMap = new HashMap<>();
    
    private static String defaultFlowPrettyConfigPath = "/pretty/pretty-flow.json";
    
    private static String defaultFlowPrettyConfigStr = "{\"endNewLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\",\"newLine\":true},{\"key\":\"nodes\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\"},{\"key\":\"start\"},{\"key\":\"pre\"},{\"key\":\"action\"},{\"key\":\"post\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"},{\"key\":\"conditions\",\"subList\":[{\"key\":\"default\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"}]}]}]},{\"key\":\"properties\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]}]},{\"key\":\"listeners\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"filters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodeActionFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"properties\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"keyType\":\"OTHER\",\"newLine\":true}],\"default\":{\"newLine\":true,\"endNewLine\":true}},{\"keyType\":\"OTHER\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]}\r\n"
            + "";
    
    private static Map<String, Object> defaultFlowPrettyConfig;

    static {
        flowNodeConverterMap.put(StartEvent.class, new StartEventConverter());
        flowNodeConverterMap.put(IntermediateCatchEvent.class, new IntermediateCatchEventConverter());
        flowNodeConverterMap.put(EndEvent.class, new EndEventConverter());

        flowNodeConverterMap.put(ScriptTask.class, new ScriptTaskConverter());
        flowNodeConverterMap.put(UserTask.class, new UserTaskConverter());
        flowNodeConverterMap.put(ReceiveTask.class, new ReceiveTaskConverter());

        flowNodeConverterMap.put(ExclusiveGateway.class, new ExclusiveGatewayConverter());
        flowNodeConverterMap.put(InclusiveGateway.class, new InclusiveGatewayConverter());
        flowNodeConverterMap.put(ParallelGateway.class, new ParallelGatewayConverter());
        
        defaultFlowPrettyConfig = JsonUtil.parseObject(defaultFlowPrettyConfigStr, Map.class);
        //Steps: Modify pretty-flow.json，then use JSONUtil to convert to string.
//        try {
//            InputStream inputStream = BpmnFlowParser.class.getResourceAsStream(defaultFlowPrettyConfigPath);
//            if (inputStream == null) {
//                logger.warn("No pretty config:" + defaultFlowPrettyConfigPath + ",JSON format without prettying");
//            }
//            String flowPrettyConf = IOUtils.toString(inputStream);
//            defaultFlowPrettyConfig = JsonUtil.parseObject(flowPrettyConf, Map.class);
//        } catch (IOException e) {
//            throw new FlowException("Pretty config parse error," + e.getMessage(), e);
//        }
    }
    
    public static String convert(String bpmnXmlData) {
        return convert(bpmnXmlData, defaultFlowPrettyConfig);
    }
    
    /**
     * Convert BPMN Model to EasyFlow Model.
     * 
     * @param bpmnXmlData
     * @return
     */
    public static String convert(String bpmnXmlData, Map<String, Object> flowPrettyConfig) {
        Map<String, Object> model;
        try {
            model = convert(new ByteArrayInputStream(bpmnXmlData.getBytes("UTF-8")));
        } catch (UnsupportedEncodingException e) {
            throw new FlowException(e);
        }
        if (flowPrettyConfig == null) {
            return JsonUtil.toJsonString(model);
        } else {
            return JsonPrettyHelper.pretty(model, flowPrettyConfig);
        }
    }

    /**
     * Convert BPMN Model to EasyFlow Model.
     * 
     * @param inputStream
     * @return
     */
    public static Map<String, Object> convert(InputStream inputStream) {
        // Get xml data.
        XMLStreamReader reader = null;
        try {
            XMLInputFactory xmlInputFactory = XMLInputFactory.newFactory();
            xmlInputFactory.setProperty("javax.xml.stream.isCoalescing", true);
            reader = xmlInputFactory.createXMLStreamReader(inputStream);
        } catch (XMLStreamException | FactoryConfigurationError e) {
            throw new FlowException("BPMN解析异常", e);
        }
        // Convert to java BPMN model leveraging the activiti ability.
        BpmnXMLConverter bpmnXmlConverter = new BpmnXMLConverter();
        BpmnModel bpmnModel = bpmnXmlConverter.convertToBpmnModel(reader);
        // Convert java BPMN model to EasyFlow model.
        Process process = bpmnModel.getProcesses().get(0);
        Map<String, Object> flowDef = new HashMap<>();
        convertProcess(process, bpmnModel, flowDef);
        return flowDef;
    }

    /**
     * Convert process definition.
     * 
     * @param process
     * @param bpmnModel
     * @param flowDef
     */
    private static void convertProcess(Process process, BpmnModel bpmnModel, Map<String, Object> flowDef) {
        // ID、name and properties
        String processId = process.getId();
        String processName = process.getName();
        flowDef.put(DefConstants.COMMON_PROP_ID, processId);
        if (StringUtils.isNotEmpty(processName)) {
            flowDef.put(DefConstants.COMMON_PROP_NAME, processName);
        }
        Map<String, List<ExtensionElement>> extensionElementMap = process.getExtensionElements();

        // Process flow element，convert Gateway、Event、Activity to Node.
        List<Map<String, Object>> nodeList = new ArrayList<>();
        flowDef.put(DefConstants.FLOW_PROP_NODES, nodeList);
        for (FlowElement flowElement : process.getFlowElements()) {
            // Flow element
            if (flowElement instanceof FlowNode) {
                FlowNodeConverter nodeConverter = flowNodeConverterMap.get(flowElement.getClass());
                if (nodeConverter == null) {
                    throw new FlowException("Unsupported BPMN elmenet:ID" + flowElement.getId() + " TYPE:"
                            + flowElement.getClass().getCanonicalName());
                }
                Map<String, Object> node = nodeConverter.convert((FlowNode) flowElement, bpmnModel, flowDef);
                nodeList.add(node);
                // 顺序流
            } else if (flowElement instanceof SequenceFlow) {
                continue;
            } else {
                throw new FlowException(
                        "Unsupported BPMN element:ID" + flowElement.getId() + " TYPE:" + flowElement.getClass().getCanonicalName());
            }
        }

        // DSF

        // properties
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.PROPERTIES)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.PROPERTIES).get(0);
            String elementText = element.getElementText();
            Map<String, Object> map = JsonUtil.parseObject(elementText, Map.class);
            flowDef.put(DefConstants.COMMON_PROP_PROPERTIES, map);
        }
        // listeners
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.LISTENERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.LISTENERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_LISTENERS, list);
        }
        // filters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_FILTERS, list);
        }
        // nodeFilters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.NODE_FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.NODE_FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_NODE_FILTERS, list);
        }        
        // nodeActionFilters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.NODE_ACTION_FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.NODE_ACTION_FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_NODE_ACTION_FILTERS, list);
        }
        // runner
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.RUNNER)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.RUNNER).get(0);
            String elementText = element.getElementText();
            Map<String, Object> runner = JsonUtil.parseObject(elementText, Map.class);
            flowDef.put(DefConstants.FLOW_PROP_RUNNER, runner);
        }

    }

    private static Map<String, Object> getNodeDef(String nodeId, Map<String, Object> flowDef) {
        List<Map<String, Object>> list = (List<Map<String, Object>>) flowDef.get(DefConstants.FLOW_PROP_NODES);
        if (list == null) {
            return null;
        }
        for (Map<String, Object> node : list) {
            if (Objects.equals(nodeId, node.get(DefConstants.COMMON_PROP_ID))) {
                return node;
            }
        }
        return null;
    }

}

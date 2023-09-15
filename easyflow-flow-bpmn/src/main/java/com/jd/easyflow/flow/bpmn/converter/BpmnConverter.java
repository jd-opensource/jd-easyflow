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
import org.activiti.bpmn.model.CallActivity;
import org.activiti.bpmn.model.ComplexGateway;
import org.activiti.bpmn.model.DataObject;
import org.activiti.bpmn.model.DataStoreReference;
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
import org.activiti.bpmn.model.SubProcess;
import org.activiti.bpmn.model.UserTask;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.bpmn.converter.activity.CallActivityConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.ReceiveTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.ScriptTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.SubProcessConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.TransactionConverter;
import com.jd.easyflow.flow.bpmn.converter.activity.UserTaskConverter;
import com.jd.easyflow.flow.bpmn.converter.event.EndEventConverter;
import com.jd.easyflow.flow.bpmn.converter.event.IntermediateCatchEventConverter;
import com.jd.easyflow.flow.bpmn.converter.event.StartEventConverter;
import com.jd.easyflow.flow.bpmn.converter.gateway.ComplexGatewayConverter;
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
    
    public static String defaultFlowPrettyConfigStr = "{\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\",\"newLine\":true},{\"key\":\"pre\",\"newLine\":true},{\"key\":\"nodes\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\"},{\"key\":\"start\"},{\"key\":\"pre\"},{\"key\":\"action\",\"subList\":[{\"key\":\"flow\",\"subList\":[{\"key\":\"id\"},{\"key\":\"name\",\"newLine\":true},{\"key\":\"pre\",\"newLine\":true},{\"key\":\"nodes\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\"},{\"key\":\"start\"},{\"key\":\"pre\"},{\"key\":\"action\"},{\"key\":\"post\"},{\"key\":\"properties\"}]}]},{\"key\":\"post\",\"newLine\":true}]}]},{\"key\":\"post\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"},{\"key\":\"conditions\",\"subList\":[{\"key\":\"default\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"}]}]}]},{\"key\":\"properties\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]}]},{\"key\":\"post\",\"newLine\":true},{\"key\":\"listeners\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"filters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodeFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodePreHandlerFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodeActionFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodePostHandlerFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"properties\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"keyType\":\"OTHER\",\"newLine\":true}],\"default\":{\"newLine\":true,\"endNewLine\":true}},{\"key\":\"parseListeners\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"keyType\":\"OTHER\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]},{\"key\":\"id\"},{\"key\":\"name\",\"newLine\":true},{\"key\":\"pre\",\"newLine\":true},{\"key\":\"nodes\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\"},{\"key\":\"start\"},{\"key\":\"pre\"},{\"key\":\"action\",\"subList\":[{\"key\":\"flow\",\"subList\":[{\"key\":\"id\"},{\"key\":\"name\",\"newLine\":true},{\"key\":\"pre\",\"newLine\":true},{\"key\":\"nodes\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true,\"subList\":[{\"key\":\"id\"},{\"key\":\"name\"},{\"key\":\"start\"},{\"key\":\"pre\"},{\"key\":\"action\"},{\"key\":\"post\"},{\"key\":\"properties\"}]}]},{\"key\":\"post\",\"newLine\":true}]}]},{\"key\":\"post\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"},{\"key\":\"conditions\",\"subList\":[{\"key\":\"default\",\"subList\":[{\"key\":\"when\"},{\"key\":\"to\"}]}]}]},{\"key\":\"properties\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]}]},{\"key\":\"post\",\"newLine\":true},{\"key\":\"listeners\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"filters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodeFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodePreHandlerFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodeActionFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"nodePostHandlerFilters\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"key\":\"properties\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"keyType\":\"OTHER\",\"newLine\":true}],\"default\":{\"newLine\":true,\"endNewLine\":true}},{\"key\":\"parseListeners\",\"newLine\":true,\"endNewLine\":true,\"subList\":[{\"newLine\":true}]},{\"keyType\":\"OTHER\",\"default\":{\"newLine\":true,\"endNewLine\":true}}]}";
    
    private static Map<String, Object> defaultFlowPrettyConfig;

    static {
        flowNodeConverterMap.put(StartEvent.class, new StartEventConverter());
        flowNodeConverterMap.put(IntermediateCatchEvent.class, new IntermediateCatchEventConverter());
        flowNodeConverterMap.put(EndEvent.class, new EndEventConverter());

        flowNodeConverterMap.put(ScriptTask.class, new ScriptTaskConverter());
        flowNodeConverterMap.put(UserTask.class, new UserTaskConverter());
        flowNodeConverterMap.put(ReceiveTask.class, new ReceiveTaskConverter());
        flowNodeConverterMap.put(SubProcess.class, new SubProcessConverter());
        flowNodeConverterMap.put(CallActivity.class, new CallActivityConverter());
        flowNodeConverterMap.put(TransactionConverter.class, new TransactionConverter());

        flowNodeConverterMap.put(ExclusiveGateway.class, new ExclusiveGatewayConverter());
        flowNodeConverterMap.put(InclusiveGateway.class, new InclusiveGatewayConverter());
        flowNodeConverterMap.put(ParallelGateway.class, new ParallelGatewayConverter());
        flowNodeConverterMap.put(ComplexGateway.class, new ComplexGatewayConverter());
        
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
        List<Map<String, Object>> model;
        try {
            model = convert(new ByteArrayInputStream(bpmnXmlData.getBytes("UTF-8")));
        } catch (UnsupportedEncodingException e) {
            throw new FlowException(e);
        }
        if (flowPrettyConfig == null) {
            return JsonUtil.toJsonString(model.size() == 1 ? model.get(0) : model);
        } else {
            return JsonPrettyHelper.pretty(model.size() == 1 ? model.get(0) : model, flowPrettyConfig);
        }
    }

    /**
     * Convert BPMN Model to EasyFlow Model.
     * 
     * @param inputStream
     * @return
     */
    public static List<Map<String, Object>> convert(InputStream inputStream) {
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
        List<Map<String, Object>> flowDefList = new ArrayList<>();
        for (Process process : bpmnModel.getProcesses()) {
            Map<String, Object> flowDef = new HashMap<String, Object>();
            convertProcess(process, bpmnModel, flowDef);
            flowDefList.add(flowDef);
        }
        
        return flowDefList;
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
        
        // flow pre handler
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.PRE)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.PRE).get(0);
            String elementText = element.getElementText();
            Object preHandlerDef = JsonUtil.parseObject(elementText, Map.class);
            flowDef.put(DefConstants.FLOW_PROP_PRE, preHandlerDef);
        }

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
                // Sequence flow
            } else if (flowElement instanceof SequenceFlow) {
                continue;
            } else if (flowElement instanceof DataStoreReference || flowElement instanceof DataObject) {
                continue;
            } else {
                throw new FlowException(
                        "Unsupported BPMN element:ID" + flowElement.getId() + " TYPE:" + flowElement.getClass().getCanonicalName());
            }
        }

        // flow post handler
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.POST)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.POST).get(0);
            String elementText = element.getElementText();
            Object postHandlerDef = JsonUtil.parseObject(elementText, Map.class);
            flowDef.put(DefConstants.FLOW_PROP_POST, postHandlerDef);
        }

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
        // nodePreHandlerFilters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.NODE_PRE_HANDLER_FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.NODE_PRE_HANDLER_FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_NODE_PRE_HANDLER_FILTERS, list);
        }        
        // nodeActionFilters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.NODE_ACTION_FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.NODE_ACTION_FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_NODE_ACTION_FILTERS, list);
        }
        // nodePostHandlerFilters
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.NODE_POST_HANDLER_FILTERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.NODE_POST_HANDLER_FILTERS).get(0);
            String elementText = element.getElementText();
            List<Object> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_NODE_POST_HANDLER_FILTERS, list);
        }        
        // runner
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.RUNNER)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.RUNNER).get(0);
            String elementText = element.getElementText();
            Map<String, Object> runner = JsonUtil.parseObject(elementText, Map.class);
            flowDef.put(DefConstants.FLOW_PROP_RUNNER, runner);
        }
        // parseListeners
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.PARSE_LISTENERS)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.PARSE_LISTENERS).get(0);
            String elementText = element.getElementText();
            List<String> list = JsonUtil.parseObject(elementText, List.class);
            flowDef.put(DefConstants.FLOW_PROP_PARSE_LISTENERS, list);
        }   
        // logFlag
        if (extensionElementMap != null && extensionElementMap.containsKey(BpmnXmlConstants.LOG_FLAG)) {
            ExtensionElement element = extensionElementMap.get(BpmnXmlConstants.LOG_FLAG).get(0);
            String elementText = element.getElementText();
            if (StringUtils.isNotEmpty(elementText)) {
                flowDef.put(DefConstants.FLOW_PROP_LOG_FLAG, Boolean.valueOf(elementText));
            }
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

    public static Map<Class, FlowNodeConverter> getFlowNodeConverterMap() {
        return flowNodeConverterMap;
    }

}

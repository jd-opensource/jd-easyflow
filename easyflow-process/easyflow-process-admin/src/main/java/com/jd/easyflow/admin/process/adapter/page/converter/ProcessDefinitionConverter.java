package com.jd.easyflow.admin.process.adapter.page.converter;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.jd.easyflow.admin.process.adapter.page.dto.ProcessDefDTO;
import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessDefinitionConverter {
    
    private static final String BPMN_FORMAT = "FLOW-bpmn";

    private static final String EXT_DATA_BPMN_OF_JSON_KEY = "bpmnOfJson";

    public static ProcessDefinitionConverter INSTANCE = new ProcessDefinitionConverter();
    

    public ProcessDefDTO adapterConvert(ProcessDefinitionDTO processDefinition) {
        if (processDefinition == null) {
            return null;
        }
        ProcessDefDTO processDefDTO = new ProcessDefDTO();
        processDefDTO.setDefId(processDefinition.getDefId());
        processDefDTO.setFormat(processDefinition.getFormat());
        processDefDTO.setId(processDefinition.getId());
        processDefDTO.setDefSource(processDefinition.getDefSource());
        processDefDTO.setDefName(processDefinition.getName());
        processDefDTO.setBizType(processDefinition.getBizType());
        processDefDTO.setCategory(processDefinition.getCategory());
        processDefDTO.setDefVersion(processDefinition.getDefVersion());
        if (BPMN_FORMAT.equals(processDefinition.getFormat())) {
            processDefDTO.setBpmnXmlData(processDefinition.getContent());
            processDefDTO.setJsonData(processDefinition.getJsonContent());
        } else {
            processDefDTO.setJsonData(processDefinition.getContent());
            String extData = processDefinition.getExtData();
            if (StringUtils.isNotEmpty(extData)) {
                Map<String, Object> extDataMap = JSON.parseObject(extData, Map.class);
                String bpmnData = (String) extDataMap.get(EXT_DATA_BPMN_OF_JSON_KEY);
                processDefDTO.setBpmnXmlData(bpmnData);
            }
        }
        processDefDTO.setLatest(processDefinition.getLatest());
        processDefDTO.setCreatedBy(processDefinition.getCreatedBy());
        processDefDTO.setModifiedBy(processDefinition.getModifiedBy());
        processDefDTO.setCreatedDate(processDefinition.getCreatedDate());
        processDefDTO.setModifiedDate(processDefinition.getModifiedDate());
        return processDefDTO;
    }

    public ProcessDefinitionDTO adapterConvert(ProcessDefDTO processDef) {
        if (processDef == null) {
            return null;
        }
        ProcessDefinitionDTO processDefDTO = new ProcessDefinitionDTO();
        processDefDTO.setDefId(processDef.getDefId());
        processDefDTO.setFormat(processDef.getFormat());
        processDefDTO.setId(processDef.getId());
        processDefDTO.setName(processDef.getDefName());
        processDefDTO.setDefSource(processDef.getDefSource());
        processDefDTO.setBizType(processDef.getBizType());
        processDefDTO.setCategory(processDef.getCategory());
        processDefDTO.setDefVersion(processDef.getDefVersion());
        if (BPMN_FORMAT.equals(processDef.getFormat())) {
            processDefDTO.setContent(processDef.getBpmnXmlData());
            processDefDTO.setJsonContent(BpmnConverter.convert(processDef.getBpmnXmlData()));
        } else {
            processDefDTO.setContent(processDef.getJsonData());
            processDefDTO.setJsonContent(processDef.getJsonData());
            if (StringUtils.isNotEmpty(processDef.getBpmnXmlData())) {
                Map<String, Object> extData = new HashMap<>();
                extData.put(EXT_DATA_BPMN_OF_JSON_KEY, processDef.getBpmnXmlData());
                processDefDTO.setExtData(JSON.toJSONString(extData));
            }
        }
        processDefDTO.setCreatedBy(processDef.getCreatedBy());
        processDefDTO.setModifiedBy(processDef.getModifiedBy());
        processDefDTO.setLatest(processDef.getLatest());
        return processDefDTO;
    }
}

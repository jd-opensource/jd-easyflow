package com.jd.easyflow.process.client.flow;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessDefinitionExport;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class StdFlowEngineImpl extends FlowEngineImpl {
    
    private static final Logger log = LoggerFactory.getLogger(StdFlowEngineImpl.class);

    private static final String AUTO_REPORT = "AUTO_REPORT";

    private static final String FLOW_EASY = "FLOW-easy";
    private static final String FLOW_BPMN = "FLOW-bpmn";

    private static final String VERSION_PREFIX = "--V_";
    
    private static final String PARENT_FLOW_ID_KEY = "_parent_flow_id";
    
    private static final String MAIN_FLOW_ID_KEY = "_main_flow_id";

    /**
     * key:category
     */
    private static final String PROPERTY_KEY_CATEGORY = "category";
    /**
     * key:bizType
     */
    private static final String PROPERTY_KEY_BIZTYPE= "bizType";

    private ProcessDefinitionExport processDefinitionExport;

    @Value("${flow.local.load:true}")
    private boolean localLoad = true;
    
    @Value("${flow.local.push:false}")
    private boolean localPush = false;

    @Override
    protected void loadFlow() {
        if (localLoad) {
            super.loadFlow();
            localFlowVersioned();
        }
        if (localPush) {
            localFlowPush();
        }
    }

    @Override
    public Flow getFlow(String flowId) {
        String wrapperFlowId = null;
        if (flowId.contains(VERSION_PREFIX)) {
            wrapperFlowId = flowId;
        } else {
             ExportResponse<Integer> response = getProcessDefinitionExport().getLatestProcessDefVersionByDefId(new ExportRequest<>(flowId));
             if (ExportResponseCode.DATA_EMPTY.getCode().equals(response.getResCode())) {
                 wrapperFlowId = flowId + VERSION_PREFIX;
                 if (flowMap.containsKey(wrapperFlowId)) {
                     return flowMap.get(wrapperFlowId);
                 } else {
                     throw new RuntimeException("Flow definition " + flowId + " not exists");
                 }
             }
             
             Integer latestProcessDefVersion = ExportResponseUtil.unwrap(response);
             wrapperFlowId = wrapperFlowId(flowId, latestProcessDefVersion);
        }
        if (flowMap.containsKey(wrapperFlowId)) {
            return flowMap.get(wrapperFlowId);
        }

        ProcessDefinitionDTO processDef = ExportResponseUtil
                .unwrap(getProcessDefinitionExport().getVersionedProcessDefinition(new ExportRequest<>(wrapperFlowId)));
        if (processDef == null) {
            log.error("Flow definition not exists {}", flowId);
            throw new RuntimeException("Flow definition " + flowId + " not exists");
        }
        wrapperFlowId = wrapperFlowId(processDef.getDefId(), processDef.getDefVersion());
        List<Flow> flowList = getFlowParser().parse(processDef.getJsonContent());
        if (flowList.size() > 1) {
            for (int i = 1; i < flowList.size(); i++) {
                Flow flow = flowList.get(i);
                if (flow.getProperty(PARENT_FLOW_ID_KEY) != null) {
                    String wrapFlowId = flow.getId() + VERSION_PREFIX;
                    if (!flowMap.containsKey(wrapFlowId)) {
                        flowMap.put(wrapFlowId, flow);
                    }
                }
            }
        }
        Flow flow = flowList.get(0);
        flow.setId(wrapperFlowId);
        flowMap.put(wrapperFlowId, flow);
        return flow;
    }
    
    private void localFlowVersioned() {
        Map<String, Flow> versionedFlowMap = new HashMap<>();
        for (Flow flow : flowMap.values()) {
            if (flow.getId().contains(VERSION_PREFIX)) {
                throw new EasyFlowException("Flow ID:" + flow.getId() + " must not contain " + VERSION_PREFIX);
            }
            versionedFlowMap.put(flow.getId() + VERSION_PREFIX, flow);
        }
        flowMap.putAll(versionedFlowMap);
    }

    protected void localFlowPush() {
        for (Entry<String, Flow> entry : flowMap.entrySet()) {
            if (! entry.getKey().endsWith(VERSION_PREFIX)) {
                continue;
            }
            Flow flow = entry.getValue();
            boolean report = processDefReport(flow);
            if (!report) {
                if (log.isDebugEnabled()) {
                    log.debug("Flow {} has been reported", flow.getId());
                }
                continue;
            }
            String content = flowDefinitionMap.get(flow.getId());
            if (StringUtils.isBlank(content)) {
                if (log.isDebugEnabled()) {
                    log.debug("Flow definition content is blank:{}", flow.getId());
                }
                continue;
            }
            ProcessDefinitionDTO processDef = new ProcessDefinitionDTO();
            String bpmnString = BpmnFlowParser.bpmnStringify(flow);
            if (StringUtils.isNotBlank(bpmnString)) {
                processDef.setContent(bpmnString);
                processDef.setJsonContent(flow.stringify());
                processDef.setFormat(FLOW_BPMN);
            } else {
                processDef.setContent(content);
                processDef.setJsonContent(content);
                processDef.setFormat(FLOW_EASY);
            }
            processDef.setCategory(flow.getProperty(PROPERTY_KEY_CATEGORY));
            processDef.setBizType(flow.getProperty(PROPERTY_KEY_BIZTYPE));
            processDef.setDefId(flow.getId());
            processDef.setName(flow.getName());
            processDef.setContent(content);
            processDef.setDefSource(AUTO_REPORT);
            log.info("Start report flow definition:{}", processDef.getDefId());
            ExportResponse response = getProcessDefinitionExport().reportProcessDef(new ExportRequest<>(processDef));
            if (!response.isSuccess()) {
                log.error("Report local flow {} exception {}", processDef.getDefId(), JSON.toJSONString(response));
                throw new EasyFlowException("Report local flow exception");
            }
        }
    }

    private boolean processDefReport(Flow flow) {
        ProcessDefinitionDTO processDef = ExportResponseUtil
                .unwrap(getProcessDefinitionExport().getProcessDefinition(new ExportRequest<>(flow.getId())));
        if (processDef == null) {
            return true;
        }
        String definitionData = flowDefinitionMap.get(flow.getId());
        return !StringUtils.equals(definitionData, processDef.getContent());
    }

    private String wrapperFlowId(String flowId, Integer latestVersion) {
        if (latestVersion == null) {
            return flowId + VERSION_PREFIX;
        }
        return StringUtils.join(flowId, VERSION_PREFIX, latestVersion);
    }

    private ProcessDefinitionExport getProcessDefinitionExport() {
        if (processDefinitionExport == null) {
            processDefinitionExport = ObjectFactorys.getDefault().getObject(ProcessDefinitionExport.class);
        }
        return processDefinitionExport;
    }

    public void setProcessDefinitionService(ProcessDefinitionExport processDefinitionExport) {
        this.processDefinitionExport = processDefinitionExport;
    }

}

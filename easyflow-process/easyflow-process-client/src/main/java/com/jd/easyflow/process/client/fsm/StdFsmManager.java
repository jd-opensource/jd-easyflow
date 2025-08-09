package com.jd.easyflow.process.client.fsm;

import java.util.HashMap;
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
import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmManager;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessDefinitionExport;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class StdFsmManager extends FsmManager {
    
    private static final Logger log = LoggerFactory.getLogger(StdFsmManager.class);

    private static final String AUTO_REPORT = "AUTO_REPORT";

    private static final String FSM_EASY = "FSM-easy";

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

    @Value("${fsm.local.push:false}")
    private boolean localPush = false;

    @Override
    protected void loadFsm() {
        if (localLoad) {
            super.loadFsm();
            localFsmVersioned();
        }
        if (localPush){
            localFsmPush();
        }
    }

    @Override
    public Fsm getFsm(String fsmId) {
        String wrapperFsmId = null;
        if (fsmId.contains(StdFlowProcessConstants.VERSION_PREFIX)) {
            wrapperFsmId = fsmId;
        } else {
            ExportResponse<Integer> response = getProcessDefinitionExport().getLatestProcessDefVersionByDefId(new ExportRequest<>(fsmId));
            if (ExportResponseCode.DATA_EMPTY.getCode().equals(response.getResCode())) {
                wrapperFsmId = fsmId + StdFlowProcessConstants.VERSION_PREFIX;
                if (fsmMap.containsKey(wrapperFsmId)) {
                    return fsmMap.get(wrapperFsmId);
                } else {
                    throw new RuntimeException("Fsm definition " + fsmId + " not exists");
                }
            }
            Integer latestProcessDefVersion = ExportResponseUtil.unwrap(response);
            wrapperFsmId = wrapperFsmId(fsmId, latestProcessDefVersion);
        }
        
        if (fsmMap.containsKey(wrapperFsmId)){
            return fsmMap.get(wrapperFsmId);
        }
        ProcessDefinitionDTO latestProcessDef = ExportResponseUtil.unwrap(getProcessDefinitionExport()
                .getVersionedProcessDefinition(new ExportRequest<>(wrapperFsmId)));
        if (latestProcessDef == null){
            log.error("Fsm definition not exists:{}", fsmId);
            throw new RuntimeException("Fsm definition not exists");
        }
        wrapperFsmId = wrapperFsmId(latestProcessDef.getDefId(),latestProcessDef.getDefVersion());
        Fsm fsm = FsmParser.parse(latestProcessDef.getJsonContent());
        fsm.setId(wrapperFsmId);
        fsmMap.put(wrapperFsmId,fsm);
        return fsm;
    }
    
    private void localFsmVersioned() {
        Map<String, Fsm> versionedFsmMap = new HashMap<>();
        for (Fsm fsm : fsmMap.values()) {
            if (fsm.getId().contains(StdFlowProcessConstants.VERSION_PREFIX)) {
                throw new EasyFlowException("Fsm ID:" + fsm.getId() + " must not contain " + StdFlowProcessConstants.VERSION_PREFIX);
            }
            versionedFsmMap.put(fsm.getId() + StdFlowProcessConstants.VERSION_PREFIX, fsm);
        }
        fsmMap.putAll(versionedFsmMap);
    }

    protected void localFsmPush(){
        for (Entry<String, Fsm> entry : fsmMap.entrySet()){
            if (!entry.getKey().endsWith(StdFlowProcessConstants.VERSION_PREFIX)) {
                continue;
            }
            Fsm fsm = entry.getValue();
            boolean report = processDefReport(fsm);
            if (!report){
                log.warn("Current fsm definition has been reported:{}",fsm.getId());
                continue;
            }
            String content = fsmDefinitionMap.get(fsm.getId());
            if (StringUtils.isBlank(content)){
                log.warn("Fsm definition content is blank:{}",fsm.getId());
                continue;
            }
            ProcessDefinitionDTO processDef = new ProcessDefinitionDTO();
            processDef.setDefId(fsm.getId());
            processDef.setName(fsm.getName());
            processDef.setContent(content);
            processDef.setJsonContent(content);
            processDef.setCategory(fsm.getProperty(PROPERTY_KEY_CATEGORY));
            processDef.setBizType(fsm.getProperty(PROPERTY_KEY_BIZTYPE));
            processDef.setDefSource(AUTO_REPORT);
            processDef.setFormat(FSM_EASY);
            log.info("Fsm definition start report:{}", processDef.getDefId());
            ExportResponse response = getProcessDefinitionExport().reportProcessDef(new ExportRequest<>(processDef));
            if (!response.isSuccess()){
                log.error("Local fsm definition {} report exception {}",processDef.getDefId(), JSON.toJSONString(response));
                throw new EasyFlowException("Local fsm definition report exception");
            }
        }
    }

    private boolean processDefReport(Fsm fsm){
        ProcessDefinitionDTO processDef = ExportResponseUtil.unwrap(getProcessDefinitionExport()
                .getProcessDefinition(new ExportRequest<>(fsm.getId())));
        if (processDef == null){
            return true;
        }
        String jsonData = fsmDefinitionMap.get(fsm.getId());
        return !StringUtils.equals(jsonData,processDef.getContent());
    }


    private String wrapperFsmId(String fsmId,Integer latestVersion){
        if (latestVersion == null){
            return fsmId + StdFlowProcessConstants.VERSION_PREFIX;
        }
        return StringUtils.join(fsmId,StdFlowProcessConstants.VERSION_PREFIX,latestVersion);
    }

    public ProcessDefinitionExport getProcessDefinitionExport() {
        if (processDefinitionExport == null) {
            processDefinitionExport = ObjectFactorys.getDefault().getObject(ProcessDefinitionExport.class);
        }
        return processDefinitionExport;
    }


    public void setProcessDefinitionExport(ProcessDefinitionExport processDefinitionExport) {
        this.processDefinitionExport = processDefinitionExport;
    }
}

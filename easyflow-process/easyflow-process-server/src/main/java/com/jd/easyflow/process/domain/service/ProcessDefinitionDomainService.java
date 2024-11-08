package com.jd.easyflow.process.domain.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.cache.CacheService;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerResult;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.common.util.CommonErrorCode;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.parser.FlowParser;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.process.adapter.export.dto.definition.NodeDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryNodeReq;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.domain.constant.ProcessDefinitionConstants;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.vo.ProcessDefinitionForListVO;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.spring.MessageUtil;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
public class ProcessDefinitionDomainService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessDefinitionDomainService.class);


    private static final int MAX_PAGE_SIZE = 10000;

    public static final String CACHE_KEY_PREFIX = "PROCESS_DEF_VERSION_";

    private static final Integer INIT_VERSION = 0;

    public static final String PROCESS_DEFINITION_LOCK = "_MODIFY_PROCESS_DEFINITION_";
    
    private static final String DEF_VERSION_NONE = "none";

    private static final String DEF_VERSION_NULL = "null";
    
    @Autowired
    private ProcessRepository processRepository;
    @Resource(name = ProcessConstants.BEAN_CACHE_SERVICE)
    private CacheService cacheService;
    @Resource(name = ProcessConstants.BEAN_LOCKER)
    private Locker locker;
    @Resource(name = ProcessConstants.BEAN_NEW_TX_TEMPLATE)
    private TransactionTemplate transactionTemplate;

    private Map<String, Object> processMap = new ConcurrentHashMap<>();

    private FlowParser flowParser = new FlowParserImpl();

    public ProcessDefinitionEntity findLatestProcessDefinitionDetail(String defId) {
        return processRepository.findLatestProcessDefinition(defId);
    }

    public boolean existProcessDefinition(String defId) {
        AssertUtils.isNotBlank(defId, "defId must not be null");
        return processRepository.existProcessDefinition(defId);
    }

    public boolean existProcessDefinition(String defId, Integer defVersion) {
        AssertUtils.isNotBlank(defId, "defId must not be null");
        AssertUtils.isNotNull(defVersion, "defVersion must not be null");
        ProcessDefinitionEntity processDef = processRepository.findProcessDefinitionByDefIdAndVersion(defId,
                defVersion);
        return processDef != null;
    }

    private void innerAddProcessDefinition(ProcessDefinitionEntity entity) {
        processRepository.saveProcessDefinition(entity);
        if (entity.getLatest()) {
            updateDefLatestCacheVersion(entity.getDefId(), entity.getDefVersion());
        }
    }

    public void addProcessDefinition(ProcessDefinitionEntity processDefinition) {
        AssertUtils.isNotNull(processDefinition, "processDefinitionVO must not be null");
        AssertUtils.isNotBlank(processDefinition.getDefId(), "processDefinitionVO defId must not be blank");
        AssertUtils.isNotBlank(processDefinition.getFormat(), "processDefinitionVO format must not be blank");
        AssertUtils.isNotBlank(processDefinition.getContent(), "processDefinitionVO content must not be blank");
        AssertUtils.isNotBlank(processDefinition.getJsonContent(), "processDefinitionVO jsonContent must not be blank");
        locker.doInLock(PROCESS_DEFINITION_LOCK, processDefinition.getDefId(), () -> {
            boolean exist = existProcessDefinition(processDefinition.getDefId());
            if (exist) {
                log.error("exists process definition:{}", processDefinition.getDefId());
                throw new UserException(CommonErrorCode.E0000003.getCode(), CommonErrorCode.E0000003.getDesc());
            }
            processDefinition.setDefVersion(INIT_VERSION);
            processDefinition.setLatest(true);
            innerAddProcessDefinition(processDefinition);
            addOrUpdateAttachedProcesses(processDefinition);
            return null;
        });
    }

    public void reportProcessDef(ProcessDefinitionEntity processDefinitionEntity) {
        AssertUtils.isNotNull(processDefinitionEntity, "processDefinitionVO must not be null");
        AssertUtils.isNotBlank(processDefinitionEntity.getDefId(), "ProcessDefinition defId must not be blank");
        AssertUtils.isNotBlank(processDefinitionEntity.getFormat(), "ProcessDefinition format must not be blank");
        AssertUtils.isNotBlank(processDefinitionEntity.getContent(), "ProcessDefinition content must not be blank");
        AssertUtils.isNotBlank(processDefinitionEntity.getJsonContent(), "ProcessDefinition jsonContent must not be blank");
        ProcessDefinitionEntity processDefinition = processRepository.findProcessDefinitionByDefIdAndVersion(
                processDefinitionEntity.getDefId(), processDefinitionEntity.getDefVersion());
        if (processDefinition != null) {
            processDefinitionEntity.setId(processDefinition.getId());
            updateReportProcessDefinitionById(processDefinition, processDefinitionEntity);
            processDefinitionEntity.setLatest(processDefinition.getLatest());
            addOrUpdateAttachedProcesses(processDefinitionEntity);
        } else {
            locker.doInLock(PROCESS_DEFINITION_LOCK, processDefinitionEntity.getDefId(), () -> {
                String defId = processDefinitionEntity.getDefId();
                ProcessDefinitionEntity processDefEntity = processRepository
                        .findProcessDefinitionByDefIdAndVersion(defId, processDefinitionEntity.getDefVersion());
                if (processDefEntity != null) {
                    processDefinitionEntity.setId(processDefEntity.getId());
                    updateReportProcessDefinitionById(processDefEntity, processDefinitionEntity);
                    processDefinitionEntity.setLatest(processDefEntity.getLatest());
                } else {
                    addReportProcessDefinition(processDefinitionEntity);
                }
                addOrUpdateAttachedProcesses(processDefinitionEntity);
                return null;
            });
        }

    }
    
    public void addReportProcessDefinition(ProcessDefinitionEntity processDefinitionEntity) {

        ProcessDefinitionEntity latestProcessDefinition = processRepository
                .findLatestProcessDefinition(processDefinitionEntity.getDefId());
        processDefinitionEntity.setLatest(true);
        if (null != latestProcessDefinition && null != latestProcessDefinition.getDefVersion()) {
            processDefinitionEntity.setLatest(false);
        }
        innerAddProcessDefinition(processDefinitionEntity);
    }
    
    public void updateReportProcessDefinitionById(ProcessDefinitionEntity originProcessDef,ProcessDefinitionEntity targetProcessDef) {
        fixBizTypeAndCategory(originProcessDef,targetProcessDef);
        updateProcessDefinitionById(targetProcessDef);
    }

    private void fixBizTypeAndCategory(ProcessDefinitionEntity originProcessDef,ProcessDefinitionEntity targetProcessDef){
        if(null == originProcessDef){
            return;
        }
        if(StringUtils.isNotBlank(originProcessDef.getBizType())){
            targetProcessDef.setBizType(originProcessDef.getBizType());
        }
        if(StringUtils.isNotBlank(originProcessDef.getCategory())){
            targetProcessDef.setCategory(originProcessDef.getCategory());
        }

    }

    private void updateDefLatestCacheVersion(String defId, Integer latestVersion) {
        String latestVersionStr = null == latestVersion ? "null" : latestVersion.toString();
        String cacheKey = CACHE_KEY_PREFIX.concat(defId);
        cacheService.set(cacheKey, latestVersionStr);
    }

    private String getDefLatestCacheVersion(String defId) {
        String cacheKey = CACHE_KEY_PREFIX.concat(defId);
        return cacheService.get(cacheKey);
    }

    public void updateProcessDefinition(ProcessDefinitionEntity processDefinition) {
        AssertUtils.isNotNull(processDefinition, "processDefinition must not be null");
        AssertUtils.isNotBlank(processDefinition.getDefId(), "ProcessDefinition defId must not be null");
        locker.doInLock(PROCESS_DEFINITION_LOCK, processDefinition.getDefId(), () -> {
            ProcessDefinitionEntity currentProcessDefinition = processRepository.findProcessDefinitionByDefIdAndVersion(
                    processDefinition.getDefId(), processDefinition.getDefVersion());
            if (currentProcessDefinition == null) {
                log.error("find none process definition, defId:{}, defVersion:{}", processDefinition.getDefId(),
                        processDefinition.getDefVersion());
                throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.latestDefinitionNotFound"));
            }
            String extDataStr = currentProcessDefinition.getExtData();
            if (StringUtils.isNotEmpty(extDataStr)) {
                Map<String, Object> extData = JSON.parseObject(extDataStr, Map.class);
                if (extData != null && StringUtils.isNotEmpty((String) extData.get(ProcessDefinitionConstants.EXT_DATA_KEY_MAIN_PROCESS_ID))) {
                    throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.AttachedProcessDefinitionCannotUpdate", new Object[] { extData.get(ProcessDefinitionConstants.EXT_DATA_KEY_MAIN_PROCESS_ID)}));
                }
            }

            if (StringUtils.equals(currentProcessDefinition.getContent(), processDefinition.getContent())) {
                processDefinition.setId(currentProcessDefinition.getId());                
                updateProcessDefinitionById(processDefinition);
                return null;
            }
            transactionTemplate.executeWithoutResult(transactionStatus -> {
                processDefinition.setCreatedBy(processDefinition.getModifiedBy());
                addProcessDefinition4Update(processDefinition);
            });
            addOrUpdateAttachedProcesses(processDefinition);
            return null;
        });
    }

    public void updateProcessDefinitionById(ProcessDefinitionEntity processDefinition) {
        processRepository.updateProcessDefinitionById(processDefinition);
    }

    public void addProcessDefinition4Update(ProcessDefinitionEntity processDefinition) {
        ProcessDefinitionEntity latestProcessDefinition = processRepository
                .findLatestProcessDefinition(processDefinition.getDefId());
        processRepository.updateProcessDefinitionLatestById(latestProcessDefinition.getId());
        processDefinition.setDefVersion(latestProcessDefinition.getDefVersion() == null ? 0
                : latestProcessDefinition.getDefVersion() + 1);
        processDefinition.setLatest(true);
        innerAddProcessDefinition(processDefinition);

    }
    
    public void forceUpdateProcessDefinition(ProcessDefinitionEntity processDefinition) {
        AssertUtils.isNotNull(processDefinition, "processDefinition must not be null");
        AssertUtils.isNotBlank(processDefinition.getDefId(), "ProcessDefinition defId must not be empty");
        locker.doInLock(PROCESS_DEFINITION_LOCK, processDefinition.getDefId(), () -> {
            transactionTemplate.executeWithoutResult(transactionStatus -> {
            ProcessDefinitionEntity processDefEntity = processRepository.findProcessDefinitionByDefIdAndVersion(
                    processDefinition.getDefId(), processDefinition.getDefVersion());
            if (processDefEntity == null) {
                log.error("process definiton {} not found", processDefinition.getDefId());
                throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.matchedDefinitionNotFound"));
            }
            String extDataStr = processDefEntity.getExtData();
            if (StringUtils.isNotEmpty(extDataStr)) {
                Map<String, Object> extData = JSON.parseObject(extDataStr, Map.class);
                if (extData != null && StringUtils.isNotEmpty((String) extData.get(ProcessDefinitionConstants.EXT_DATA_KEY_MAIN_PROCESS_ID))) {
                    throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.AttachedProcessDefinitionCannotUpdate", new Object[] { extData.get(ProcessDefinitionConstants.EXT_DATA_KEY_MAIN_PROCESS_ID)}));
                }
            }
            processDefinition.setId(processDefEntity.getId());
            processRepository.updateProcessDefinitionById(processDefinition);

            });
            addOrUpdateAttachedProcesses(processDefinition);
            return null;
        });
    }

    public ProcessDefinitionEntity findLatestProcessDefinition(String defId) {
        return processRepository.findLatestProcessDefinition(defId);
    }

    public ProcessDefinitionEntity getLatestProcessDefinition(String defIdWithVersion) {
        Object[] info = parseDefId(defIdWithVersion);
        String defId = (String) info[0];
        return processRepository.findLatestProcessDefinition(defId);
    }

    public ProcessDefinitionEntity getVersionedProcessDefinition(String defIdWithVersion) {
        Object[] info = parseDefId(defIdWithVersion);
        String defId = (String) info[0];
        Integer version = (Integer) info[1];
        if (version == null) {
            return processRepository.findLatestProcessDefinition(defId);
        } else {
            if (version == -1) {
                version = null;
            }
            return processRepository.findProcessDefinitionByDefIdAndVersion(defId, version);
        }
    }

    public ProcessDefinitionEntity getProcessDefinition(String defIdWithVersion) {
        Object[] info = parseDefId(defIdWithVersion);
        String defId = (String) info[0];
        Integer version = (Integer) info[1];
        if (null != version && version == -1) {
            version = null;
        }
        return findProcessDefinitionByIdAndVersion(defId, version);
    }

    public Object[] parseDefId(String defIdWithVersion) {
        if (defIdWithVersion.startsWith(ProcessDefinitionConstants.FLOW_PREFIX)) {
            defIdWithVersion = defIdWithVersion.substring(ProcessDefinitionConstants.FLOW_PREFIX.length());
        } else if (defIdWithVersion.startsWith(ProcessDefinitionConstants.FSM_PREFIX)) {
            defIdWithVersion = defIdWithVersion.substring(ProcessDefinitionConstants.FSM_PREFIX.length());
        }
        String defId = null;
        Integer version = null;
        int pos = defIdWithVersion.indexOf(ProcessDefinitionConstants.VERSION_SEP);
        if (pos == -1) {
            defId = defIdWithVersion;
        } else {
            defId = defIdWithVersion.substring(0, pos);
            String versionStr = defIdWithVersion.substring(pos + ProcessDefinitionConstants.VERSION_SEP.length());
            if (versionStr.length() == 0) {
                version = -1;
            } else {
                version = Integer.valueOf(versionStr);
            }
        }
        return new Object[] { defId, version };
    }

    public String generateDefIdWithVersion(String defId, Integer version) {
        if (version == null) {
            return defId;
        }
        if (version == -1) {
            return defId + ProcessDefinitionConstants.VERSION_SEP;
        }
        return defId + ProcessDefinitionConstants.VERSION_SEP + version;
    }

    public PagerResult<ProcessDefinitionForListVO> pageQueryProcessDefinition(PagerCondition pagerQueryReq) {
        if (pagerQueryReq.getPageSize() > MAX_PAGE_SIZE) {
            throw new UserException("The max page size is " + MAX_PAGE_SIZE);
        }
        return processRepository.pageQueryProcessDefinition(pagerQueryReq);
    }

    public ProcessDefinitionEntity findProcessDefinitionByIdAndVersion(String defId, Integer defVersion) {
        return processRepository.findProcessDefinitionByDefIdAndVersion(defId, defVersion);
    }

    public Integer getLatestProcessDefVersionByDefId(String defId) {
        String cacheKey = CACHE_KEY_PREFIX.concat(defId);
        String defVersion = cacheService.get(cacheKey);
        if (defVersion == null) {
            ProcessDefinitionEntity latestProcessDef = processRepository.findLatestProcessDefinition(defId);
            if (latestProcessDef == null) {
                log.info("Process definition ID{} not exists", defId);
                defVersion = DEF_VERSION_NONE;
            } else {
                defVersion = latestProcessDef.getDefVersion() == null ? DEF_VERSION_NULL
                        : latestProcessDef.getDefVersion().toString();
            }
            cacheService.set(cacheKey, defVersion);
        }
        if (StringUtils.equals(defVersion, DEF_VERSION_NONE)) {
            return -1;
        }
        return StringUtils.equals(DEF_VERSION_NULL, defVersion) ? null : Integer.parseInt(defVersion);
    }

    public ProcessDTO getProcessProperties(String processId) {
        Object process = getProcess(processId);
        if (process instanceof Flow) {
            Flow flow = (Flow) process;
            ProcessDTO processDto = new ProcessDTO();
            processDto.setProperties(flow.getProperties());
            return processDto;
        } else {
            Fsm fsm = (Fsm) process;
            ProcessDTO processDto = new ProcessDTO();
            processDto.setProperties(fsm.getProperties());
            return processDto;
        }
    }

    public NodeDTO getNodeProperties(QueryNodeReq request) {
        Object process = getProcess(request.getProcessId());
        if (process instanceof Flow) {
            Flow flow = (Flow) process;
            FlowNode node = flow.getNode(request.getNodeId());
            NodeDTO nodeDto = new NodeDTO();
            nodeDto.setProperties(node.getProperties());
            return nodeDto;
        } else {
            Fsm fsm = (Fsm) process;
            State state = fsm.getState(request.getNodeId());
            NodeDTO nodeDto = new NodeDTO();
            nodeDto.setProperties(state.getProperties());
            return nodeDto;
        }
    }

    public ProcessDTO getProcessAndNodeProperties(String processId) {
        Object process = getProcess(processId);
        if (process instanceof Flow) {
            Flow flow = (Flow) process;
            ProcessDTO processDto = new ProcessDTO();
            processDto.setProperties(flow.getProperties());
            if (flow.getNodeList() != null) {
                List<NodeDTO> nodeList = new ArrayList<>(flow.getNodeList().size());
                for (FlowNode flowNode : flow.getNodeList()) {
                    NodeDTO node = new NodeDTO();
                    node.setProperties(flowNode.getProperties());
                    nodeList.add(node);
                }
                processDto.setNodeList(nodeList);
            }
            return processDto;
        } else {
            Fsm fsm = (Fsm) process;
            ProcessDTO processDto = new ProcessDTO();
            processDto.setProperties(fsm.getProperties());
            if (fsm.getStateList() != null) {
                List<NodeDTO> nodeList = new ArrayList<>(fsm.getStateList().size());
                for (State state : fsm.getStateList()) {
                    NodeDTO node = new NodeDTO();
                    node.setProperties(state.getProperties());
                    nodeList.add(node);
                }
                processDto.setNodeList(nodeList);
            }
            return processDto;
        }
    }

    private Object getProcess(String processId) {
        if (processId.endsWith(ProcessDefinitionConstants.VERSION_SEP)) {
            processId = processId.substring(0, processId.length() - ProcessDefinitionConstants.VERSION_SEP.length());
        }
        Object process = processMap.get(processId);
        if (process == null) {
            ProcessDefinitionEntity definition = getProcessDefinition(processId);
            if (definition == null) {
                log.warn("process definition :" + processId + " not exists");
                throw new UserException(MessageUtil.getMessage("easyflow.process.server.tip.processDefinitionNotExistsWithId", new Object[] {processId}));
            }
            if (ProcessDefinitionConstants.PROCESS_FORMAT_FSM_EASY.equals(definition.getFormat())) {
                process = FsmParser.parse(definition.getJsonContent(), false);
                processMap.put(processId, process);
            } else {
                process = flowParser.parse(definition.getJsonContent(), false).get(0);
                processMap.put(processId, process);
            }
        }
        return process;
    }

    private void addOrUpdateAttachedProcesses(ProcessDefinitionEntity definition) {
        String jsonContent = definition.getJsonContent();
        if (! jsonContent.trim().startsWith("[")) {
            return;
        }
        List<Map<String, Object>> list = JSON.parseObject(jsonContent, List.class);
        for (int i = 1; i < list.size(); i++) {
            Map<String, Object> def = list.get(i);
            addOrUpdateOneRemainProcess(definition, def);
        }
    }

    private void addOrUpdateOneRemainProcess(ProcessDefinitionEntity definition, Map<String, Object> def) {
        String flowId = (String) def.get("id");
        String name = (String) def.get("name");
        Integer defVersion = definition.getDefVersion();
        Boolean latest = definition.getLatest();
        transactionTemplate.executeWithoutResult(transactionStatus -> {
            ProcessDefinitionEntity existsDefinition = processRepository.findProcessDefinitionByDefIdAndVersion(flowId,
                    defVersion);
            ProcessDefinitionEntity latestDefinition = processRepository.findLatestProcessDefinition(flowId);
            String jsonContent = JSON.toJSONString(def);
            if (existsDefinition == null) {
                ProcessDefinitionEntity entity = new ProcessDefinitionEntity();
                entity.setBizType(definition.getBizType());
                entity.setContent(jsonContent);
                entity.setDefId(flowId);
                entity.setDefSource(definition.getDefSource());
                entity.setDefVersion(defVersion);
                entity.setFormat(ProcessDefinitionConstants.PROCESS_FORMAT_FLOW_EASY);
                entity.setJsonContent(jsonContent);
                entity.setLatest(latest);
                entity.setName(name);
                entity.setCreatedBy(definition.getCreatedBy());
                entity.setModifiedBy(definition.getModifiedBy());
                Map<String, Object> extData = new HashMap<String, Object>();
                extData.put(ProcessDefinitionConstants.EXT_DATA_KEY_MAIN_PROCESS_ID, definition.getDefId());
                entity.setExtData(JSON.toJSONString(extData));
                processRepository.saveProcessDefinition(entity);
                if (Boolean.TRUE.equals(latest) && latestDefinition != null) {
                    latestDefinition.setLatest(false);
                    processRepository.updateProcessDefinitionById(latestDefinition);
                }
            } else {
                existsDefinition.setBizType(definition.getBizType());
                existsDefinition.setContent(jsonContent);
                existsDefinition.setDefId(flowId);
                existsDefinition.setDefSource(definition.getDefSource());
                existsDefinition.setDefVersion(defVersion);
                existsDefinition.setFormat(ProcessDefinitionConstants.PROCESS_FORMAT_FLOW_EASY);
                existsDefinition.setJsonContent(jsonContent);
                existsDefinition.setLatest(latest);
                existsDefinition.setName(name);
                existsDefinition.setCreatedBy(definition.getCreatedBy());
                existsDefinition.setModifiedBy(definition.getModifiedBy());
                processRepository.updateProcessDefinitionById(existsDefinition);
                if (Boolean.TRUE.equals(latest) && latestDefinition != null
                        && !latestDefinition.getId().equals(existsDefinition.getId())) {
                    processRepository.updateProcessDefinitionLatestById(latestDefinition.getId());

                }
            }
        });
        if (latest) {
            updateDefLatestCacheVersion(flowId, definition.getDefVersion());
        }
    }

    public CacheService getCacheService() {
        return cacheService;
    }

    public void setCacheService(CacheService cacheService) {
        this.cacheService = cacheService;
    }

    public Locker getLocker() {
        return locker;
    }

    public void setLocker(Locker locker) {
        this.locker = locker;
    }

    public TransactionTemplate getTransactionTemplate() {
        return transactionTemplate;
    }

    public void setTransactionTemplate(TransactionTemplate transactionTemplate) {
        this.transactionTemplate = transactionTemplate;
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

}

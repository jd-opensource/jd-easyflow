package com.jd.easyflow.process.domain.service.impl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.common.client.dto.ClientResponseCode;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.domain.model.entity.ProcessDefinitionEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.service.ProcessDefinitionDomainService;
import com.jd.easyflow.process.domain.service.ProcessScheduleInvoker;
import com.jd.easyflow.process.spi.client.ProcessScheduleClientService;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteReq;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteRes;

/**
 * @author liyuliang5
 *
 */
public class ProcessScheduleSpiInvoker implements ProcessScheduleInvoker {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessScheduleSpiInvoker.class);


    @Autowired
    private ProcessDefinitionDomainService processDefinitionDomainService;
    @Autowired
    private ProcessRepository processRepository;

    @Override
    public ScheduleProcessResVO invoke(ScheduleProcessReqVO vo) {
        log.info("Start invoke by SPI, req:{}", vo);
        String runtimeSerivce = getRuntimeService(vo);
        log.info("Runtime Service:" + runtimeSerivce);
        ProcessScheduleClientService processScheduleClientService = null;
        if (runtimeSerivce == null || runtimeSerivce.isEmpty()) {
            processScheduleClientService = ObjectFactorys.getDefault().getObject(ProcessScheduleClientService.class);
        } else {
            String[] serviceInfo = runtimeSerivce.split(":");
            String providerId = serviceInfo.length == 1 ? null : serviceInfo[0];
            String serviceId = serviceInfo.length == 1 ? serviceInfo[0] : serviceInfo[1];
            processScheduleClientService = ObjectFactorys.getDefault().getObject(ProcessScheduleClientService.class,
                    providerId, serviceId);
        }
        ProcessExecuteReq req = new ProcessExecuteReq();
        req.setProcessId(vo.getProcessId());
        req.setNodeIds(vo.getNodeIds());
        req.setParam(vo.getParam());
        req.setDataMap(vo.getDataMap());
        log.info("Invoke biz client request:" + req);
        ClientResponse<ProcessExecuteRes> clientResponse = processScheduleClientService.execute(new ClientRequest(req));
        log.info("Invoke biz client response:" + clientResponse);
        if (!ClientResponseCode.SUCCESS.getCode().equals(clientResponse.getResCode())) {
            log.error("Invoke biz client exception:" + clientResponse);
            UserException userException = new UserException(clientResponse.getResCode(), clientResponse.getResDesc());
            if (clientResponse.getExt() != null) {
                userException.setData(clientResponse.getExt().get("errorData"));
            }
            throw userException;
        }
        log.info("End invoke by SPI");
        ScheduleProcessResVO res = new ScheduleProcessResVO();
        ProcessExecuteRes executeRes = clientResponse.getData();
        res.setProcessInstanceNo(executeRes == null ? null : executeRes.getProcessInstanceNo()); 
        res.setResult(executeRes == null ? null : executeRes.getResult()); 
        res.setDataMap(executeRes == null ? null : executeRes.getDataMap()); 
        return res;
    }

    /**
     * 
     * @param vo
     * @return
     */
    private String getRuntimeService(ScheduleProcessReqVO vo) {
        String processId = vo.getProcessId();
        if ((processId == null || processId.isEmpty()) && (vo.getProcessInstanceNo() != null && ! vo.getProcessInstanceNo().isEmpty())) {
            ProcessInstanceEntity entity = processRepository.getByProcessInstanceNo(vo.getProcessInstanceNo());
            processId = entity.getProcessDefId();
        }

        if (processId != null && ! processId.isEmpty()) {
            ProcessDefinitionEntity entity = processDefinitionDomainService.getLatestProcessDefinition(processId);
            String latestProcessId = processDefinitionDomainService.generateDefIdWithVersion(entity.getDefId(),
                    entity.getDefVersion());
            ProcessDTO process = processDefinitionDomainService.getProcessProperties(latestProcessId);
            if (process != null && process.getProperties() != null) {
                Map<String, Object> processProperties = (Map<String, Object>) process.getProperties().get("process");
                if (processProperties != null) {
                    return (String) processProperties.get("runtimeService");
                }
                return null;
            }
        }
        return null;
    }

    public ProcessDefinitionDomainService getProcessDefinitionDomainService() {
        return processDefinitionDomainService;
    }

    public void setProcessDefinitionDomainService(ProcessDefinitionDomainService processDefinitionDomainService) {
        this.processDefinitionDomainService = processDefinitionDomainService;
    }

    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
    }
    
    

}

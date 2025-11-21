package com.jd.easyflow.process.adapter.export;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.process.adapter.export.converter.ProcessInstanceConverter;
import com.jd.easyflow.process.adapter.export.converter.ProcessToolConverter;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeReq;
import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeRes;
import com.jd.easyflow.process.domain.model.entity.ProcessInstanceEntity;
import com.jd.easyflow.process.domain.model.entity.ProcessNodeInstanceEntity;
import com.jd.easyflow.process.domain.repository.ProcessRepository;
import com.jd.easyflow.process.domain.service.ProcessToolDomainService;

/**
 * @author liyuliang5
 */
public class ProcessToolExportImpl implements ProcessToolExport {
    
    @Autowired
    private ProcessRepository processRepository;
    @Autowired
    private ProcessToolDomainService processToolDomainService;
    
    @Action(code = "easyflow-process-rollbackNode", name = "rollbackNode")
    @Override
    public ExportResponse<RollbackNodeRes> rollbackNode(ExportRequest<RollbackNodeReq> request) {
        AssertUtils.isNotNull(request);
        AssertUtils.isNotNull(request.getData());
        AssertUtils.isNotNull(request.getData().getProcessInstanceNo());
        processToolDomainService.rollbackNode(ProcessToolConverter.INSTANCE.convert(request.getData()));
        return ExportResponse.build4Success(new RollbackNodeRes());
    }
    
    @Action(code = "easyflow-process-updateProcessInstance", name = "updateProcessInstance")
    @Override
    public ExportResponse updateProcessInstance(ExportRequest<ProcessInstanceDTO> request) {
        ProcessInstanceEntity entity = ProcessInstanceConverter.INSTANCE.convert(request.getData());
        getTransactionTemplate().executeWithoutResult(status -> {
            processRepository.updateProcessInstanceByNo(entity);
        });
        return ExportResponse.build4Success();
    }


    @Action(code = "easyflow-process-updateProcessNodeInstance", name = "updateProcessNodeInstance")
    @Override
    public ExportResponse updateProcessNodeInstance(ExportRequest<ProcessNodeInstanceDTO> request) {
        ProcessNodeInstanceEntity entity = ProcessInstanceConverter.INSTANCE.convert(request.getData());
        getTransactionTemplate().executeWithoutResult(status -> {
            processRepository.updateProcessNodeInstanceByNo(entity);
        });
        return ExportResponse.build4Success();
    }

    @Action(code = "easyflow-process-deleteProcessNodeInstance", name = "deleteProcessNodeInstance")
    @Override
    public ExportResponse deleteProcessNodeInstance(ExportRequest<String> request) {
        AssertUtils.isNotNull(request);
        AssertUtils.isNotNull(request.getData());
        getTransactionTemplate().executeWithoutResult(status -> {
            processRepository.deleteProcessNodeInstanceByNo(request.getData());
        });
        return ExportResponse.build4Success();
    }
    
    private TransactionTemplate getTransactionTemplate() {
        return processToolDomainService.getTransactionTemplate();
    }

    public ProcessRepository getProcessRepository() {
        return processRepository;
    }

    public void setProcessRepository(ProcessRepository processRepository) {
        this.processRepository = processRepository;
    }

    public ProcessToolDomainService getProcessToolDomainService() {
        return processToolDomainService;
    }

    public void setProcessToolDomainService(ProcessToolDomainService processToolDomainService) {
        this.processToolDomainService = processToolDomainService;
    }
}

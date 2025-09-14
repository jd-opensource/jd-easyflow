package com.jd.easyflow.process.adapter.export;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ThreadPoolExecutor;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.SmartApplicationListener;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.action.Action;
import com.jd.easyflow.alert.AlertUtil;
import com.jd.easyflow.codegenerator.client.CodeGenerateHelper;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.PersistDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeExecutionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskEventDTO;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdRes;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommand;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommandResult;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnRes;
import com.jd.easyflow.process.adapter.export.dto.transaction.command.BatchUpdateTxnCommand;
import com.jd.easyflow.process.adapter.export.dto.transaction.command.InterruptTxnCommand;
import com.jd.easyflow.process.domain.constant.ProcessConstants;
import com.jd.easyflow.process.domain.service.ProcessInstanceDomainService;
import com.jd.easyflow.process.domain.service.ProcessTaskDomainService;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessTransactionExportImpl implements ProcessTransactionExport, SmartApplicationListener {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessTransactionExportImpl.class);


    @Resource(name = ProcessConstants.BEAN_NEW_TX_TEMPLATE)
    private TransactionTemplate transactionTemplate;

    @Resource(name = "easyflow-process-messageSendService")
    private MessageSendService messageSendService;
    @Autowired
    private ProcessInstanceDomainService processInstanceDomainService;
    @Autowired
    private ProcessTaskDomainService processTaskDomainService;

    private ThreadPoolTaskExecutor executor;

    private Boolean isSelfInitial;


    @Action(code = "easyflow-process-nextObjectId", name = "nextObjectId")
    @Override
    public ExportResponse<String> nextObjectId(ExportRequest<String> request) {
        String type = request.getData();
        String objectId = nextObjectId(type);
        return ExportResponse.build4Success(objectId);
    }

    @Action(code = "easyflow-process-batchNextObjectId", name = "batchNextObjectId")
    @Override
    public ExportResponse<BatchObjectIdRes> batchNextObjectId(ExportRequest<BatchObjectIdReq> request) {
        String type = request.getData().getType();
        int batchSize = request.getData().getNum();
        String[] objectIds = batchNextObjectId(type, batchSize);
        BatchObjectIdRes res = new BatchObjectIdRes();
        res.setIds(objectIds);
        return ExportResponse.build4Success(res);
    }

    private String nextObjectId(String type) {
        String objectId = null;
        switch (type) {
        case ProcessTransactionConstants.TYPE_PROCESS: {
            objectId = CodeGenerateHelper.generateCode("PROCESS_INSTANCE", "PI");
            break;
        }
        case ProcessTransactionConstants.TYPE_NODE: {
            objectId = CodeGenerateHelper.generateCode("NODE_INSTANCE", "NI");
            break;
        }
        case ProcessTransactionConstants.TYPE_EXECUTION: {
            objectId = CodeGenerateHelper.generateCode("EXECUTION_INSTANCE", "NE");
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK: {
            objectId = CodeGenerateHelper.generateCode("PROCESS_TASK", "PT");
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK_ASSIGN: {
            objectId = CodeGenerateHelper.generateCode("PROCESS_TASK_ASSIGN", "PTA");
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK_EVENT: {
            objectId = CodeGenerateHelper.generateCode("PROCESS_TASK_EVENT", "PTE");
            break;
        }
        default: {
            throw new IllegalArgumentException("Illegal param:" + type);
        }
        }
        return objectId;
    }
    
    private String[] batchNextObjectId(String type, int batchSize) {
        String[] objectIds = null;
        switch (type) {
        case ProcessTransactionConstants.TYPE_PROCESS: {
            objectIds = CodeGenerateHelper.batchGenerateCode("PROCESS_INSTANCE", "PI", batchSize);
            break;
        }
        case ProcessTransactionConstants.TYPE_NODE: {
            objectIds = CodeGenerateHelper.batchGenerateCode("NODE_INSTANCE", "NI", batchSize);
            break;
        }
        case ProcessTransactionConstants.TYPE_EXECUTION: {
            objectIds = CodeGenerateHelper.batchGenerateCode("EXECUTION_INSTANCE", "NE", batchSize);
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK: {
            objectIds = CodeGenerateHelper.batchGenerateCode("PROCESS_TASK", "PT", batchSize);
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK_ASSIGN: {
            objectIds = CodeGenerateHelper.batchGenerateCode("PROCESS_TASK_ASSIGN", "PTA", batchSize);
            break;
        }
        case ProcessTransactionConstants.TYPE_TASK_EVENT: {
            objectIds = CodeGenerateHelper.batchGenerateCode("PROCESS_TASK_EVENT", "PTE", batchSize);
            break;
        }
        default: {
            throw new IllegalArgumentException("Illegal param:" + type);
        }
        }
        return objectIds;
    }

    @Action(code = "easyflow-process-doTransaction", name = "doTransaction")
    @Override
    public ExportResponse<TxnRes> doTransaction(ExportRequest<TxnReq> request) {
        List<Map<String, Object>> postActionList = new ArrayList<>();
        TxnRes response = transactionTemplate.execute(status -> {
            return executeTransaction(request.getData(), postActionList);
        });
        for (Map<String, Object> action : postActionList) {
            String type = (String) action.get("type");
            if (ProcessConstants.TXN_ACTION_MERGE_ASYNC.equals(type)) {
                List<Runnable> tasks = (List<Runnable>) action.get("tasks");
                executor.execute(() -> {
                    if (log.isDebugEnabled()) {
                        log.debug("Start async process");
                    }
                    try {
                        tasks.forEach(task -> task.run());
                        if (log.isDebugEnabled()) {
                            log.debug("End async process");
                        }
                    } catch (Exception e) {
                        AlertUtil.alert("Async process exception", e);
                    }
                });
            } else {
                String topic = (String) action.get("topic");
                String bizData = (String) action.get("bizData");
                messageSendService.sendMessage(UUID.randomUUID().toString(), topic, bizData);
            }
        }
        return ExportResponse.build4Success(response);
    }

    public TxnRes executeTransaction(TxnReq request, List<Map<String, Object>> postActionList) {
        List<TxnCommand> commandList = request.getCommandList();
        TxnRes res = new TxnRes();
        List<TxnCommandResult> resultList = new ArrayList<>();
        res.setResultList(resultList);
        for (TxnCommand command : commandList) {
            if (ProcessTransactionConstants.TXN_COMMAND_BATCH_UPDATE.equals(command.getCommandType())) {
                BatchUpdateTxnCommand batchUpdateCommand = (BatchUpdateTxnCommand) command;
                for (Object o : batchUpdateCommand.getObjects()) {
                    Object persistObj = o;
                    Integer persistOp = null;
                    if (o instanceof PersistDTO) {
                        persistObj = ((PersistDTO) o).getPersistObject();
                        persistOp = ((PersistDTO) o).getPersistOp();
                    }
                    if (persistObj instanceof ProcessInstanceDTO || persistObj instanceof ProcessNodeInstanceDTO
                            || persistObj instanceof ProcessNodeExecutionDTO) {
                        processInstanceDomainService.updateProcessObject(persistOp, persistObj,
                                batchUpdateCommand.getProcessContext(), postActionList);
                    } else if (persistObj instanceof ProcessTaskDTO || persistObj instanceof ProcessTaskAssignDTO
                            || persistObj instanceof ProcessTaskEventDTO) {
                        processTaskDomainService.updateTaskObject(persistOp, persistObj,
                                batchUpdateCommand.getProcessContext(), postActionList);
                    } else {
                        throw new UnsupportedOperationException("Unsupported object type:" + o.getClass().getName());
                    }
                }

            } else if (ProcessTransactionConstants.TXN_COMMAND_INTERRUPT.equals(command.getCommandType())) {
                InterruptTxnCommand interruptCommand = (InterruptTxnCommand) command;
                processInstanceDomainService.interruptOnRuntime(interruptCommand.getProcessInstanceNo(),
                        interruptCommand.getInterruptUser(), interruptCommand.getInterruptTime(),
                        interruptCommand.getProcessContext(), postActionList);
            } else {
                throw new UnsupportedOperationException("Unsupported operation type:" + command.getCommandType());
            }
        }
        return res;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public ThreadPoolTaskExecutor getExecutor() {
        return executor;
    }

    public void setExecutor(ThreadPoolTaskExecutor executor) {
        this.executor = executor;
    }

    @Override
    public boolean supportsEventType(Class<? extends ApplicationEvent> eventType) {
        return ContextClosedEvent.class.isAssignableFrom(eventType);
    }

    @Override
    public void onApplicationEvent(ApplicationEvent event) {
        if (executor != null && Boolean.TRUE.equals(isSelfInitial)) {
            executor.shutdown();
        }
    }

    @PostConstruct
    public void init(){
        if(executor == null){
            log.info("ProcessTransactionExportImpl set none thread pool, load default.");
            executor = initDefaultThreadPool();
        }
    }

    public ThreadPoolTaskExecutor initDefaultThreadPool() {
        ThreadPoolTaskExecutor threadPoolTaskExecutor = new ThreadPoolTaskExecutor();
        threadPoolTaskExecutor.setCorePoolSize(Runtime.getRuntime().availableProcessors() / 8 + 1);
        threadPoolTaskExecutor.setMaxPoolSize(100);
        threadPoolTaskExecutor.setQueueCapacity(500);
        threadPoolTaskExecutor.setKeepAliveSeconds(100);
        threadPoolTaskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        threadPoolTaskExecutor.setWaitForTasksToCompleteOnShutdown(true);
        threadPoolTaskExecutor.setAllowCoreThreadTimeOut(false);
        threadPoolTaskExecutor.setAwaitTerminationSeconds(3);
        threadPoolTaskExecutor.initialize();
        this.isSelfInitial = true;
        return threadPoolTaskExecutor;
    }
}

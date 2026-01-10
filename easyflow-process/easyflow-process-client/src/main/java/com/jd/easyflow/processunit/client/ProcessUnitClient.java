package com.jd.easyflow.processunit.client;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.util.UUIDUtil;
import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.net.NetUtils;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.processunit.adapter.export.ProcessUnitExport;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitUpdateRes;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.bean.ProcessUnitCreateAndExecuteReq;
import com.jd.easyflow.processunit.client.bean.ProcessUnitCreateAndExecuteRes;
import com.jd.easyflow.processunit.client.service.impl.SyncClientProcessUnitExecutor;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitClient {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitClient.class);


    private SyncClientProcessUnitExecutor executor;

    private ProcessUnitExport processUnitExport;
    
    private MessageSendService messageSendService;
    
    private String createMessageTopic;
    
    private String executeMessageTopic;
    
    public <T> T call(String unitCode, String requestContent, Function<ExecContext, T> invoker) {
        String bizNo = UUIDUtil.getSimpleUUID();
        String requestNo = UUIDUtil.getSimpleUUID();
        return call(unitCode, bizNo, requestNo, requestContent, invoker, ctx -> {
            if (!ctx.isExecuted()) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            if (ctx.getResult().getExecException() != null) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            return ProcessUnitConstants.RESULT_SUCCESS;
        }, ctx -> {
            return JSON.toJSONString(ctx.getResult().getExecResult());
        }, ctx -> {
            return null;
        });
    }

    public <T> T call(String unitCode, String bizNo, String requestContent, Function<ExecContext, T> invoker) {
        String requestNo = UUIDUtil.getSimpleUUID();
        return call(unitCode, bizNo, requestNo, requestContent, invoker, ctx -> {
            if (!ctx.isExecuted()) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            if (ctx.getResult().getExecException() != null) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            return ProcessUnitConstants.RESULT_SUCCESS;
        }, ctx -> {
            return JSON.toJSONString(ctx.getResult().getExecResult());
        }, ctx -> {
            return null;
        });
    }

    public <T> T call(String unitCode, String bizNo, String requestContent, Function<ExecContext, T> invoker,
            Function<ExecContext, String> resultFunction) {
        String requestNo = UUIDUtil.getSimpleUUID();
        return call(unitCode, bizNo, requestNo, requestContent, invoker, resultFunction, ctx -> {
            return JSON.toJSONString(ctx.getResult().getExecResult());
        }, ctx -> {
            return null;
        });
    }

    public <T> T call(String unitCode, String bizNo, String requestContent, String productCode,  Function<ExecContext, T> invoker,
                      Function<ExecContext, String> resultFunction) {
        String requestNo = UUIDUtil.getSimpleUUID();
        return call(unitCode, bizNo, requestNo, requestContent, productCode, invoker, resultFunction, ctx -> {
            return JSON.toJSONString(ctx.getResult().getExecResult());
        }, ctx -> {
            return null;
        });
    }

    public <T> T call(String unitCode, String bizNo, String requestNo, String requestContent,
            Function<ExecContext, T> invoker, Function<ExecContext, String> resultFunction,
            Function<ExecContext, String> responseContentFunction, Function<ExecContext, ?> oldResultFunction) {
        return call(unitCode, bizNo, requestNo, requestContent, null, invoker, resultFunction, responseContentFunction, oldResultFunction);
    }
    
    public <T> T call(String unitCode, String bizNo, String requestNo, String requestContent, String productCode,
            Function<ExecContext, T> invoker, Function<ExecContext, String> resultFunction,
            Function<ExecContext, String> responseContentFunction, Function<ExecContext, ?> oldResultFunction) {
        return call(unitCode, bizNo, requestNo, requestContent, productCode, null, invoker, resultFunction, responseContentFunction, oldResultFunction);
    }
    
    public <T> T call(String unitCode, String bizNo, String requestNo, String requestContent, String productCode, String parentNo,
            Function<ExecContext, T> invoker, Function<ExecContext, String> resultFunction,
            Function<ExecContext, String> responseContentFunction, Function<ExecContext, ?> oldResultFunction) {
        ExecParam param = new ExecParam();
        param.setUnitCode(unitCode);
        param.setBizNo(bizNo);
        param.setRequestNo(requestNo);
        param.setRequestContent(requestContent);
        param.setProductCode(productCode);
        param.setInvoker(invoker);
        param.setResultFunction(resultFunction);
        param.setResponseContentFunction(responseContentFunction);
        param.setOldResultFunction(oldResultFunction);
        param.setParentNo(parentNo);
        ExecResult result = executor.execute(param);
        return (T) result.getExecResult();
    }
    
    public ExecResult call(ExecParam param) {
        return executor.execute(param);
    }

    public SyncClientProcessUnitExecutor getExecutor() {
        return executor;
    }

    public void setExecutor(SyncClientProcessUnitExecutor executor) {
        this.executor = executor;
    }

    public ProcessUnitCreateRes create(String unitCode, String bizNo) {
        return create(unitCode, bizNo, UUIDUtil.getSimpleUUID(), null, null);
    }

    public ProcessUnitCreateRes create(String unitCode, String bizNo,
            String requestContent) {
        return create(unitCode, bizNo, UUIDUtil.getSimpleUUID(), requestContent, null);
    }
    
    
    public ProcessUnitCreateRes create(String unitCode, String bizNo,
            String requestNo, String requestContent) {
        return this.create(unitCode, bizNo, requestNo, requestContent, null);
    }
    
    public ProcessUnitCreateRes create(String unitCode, String bizNo,
            String requestNo, String requestContent, String productCode) {
        return create(unitCode, bizNo, requestNo, requestContent, productCode, null);
    }

    public ProcessUnitCreateRes create(String unitCode, String bizNo,
            String requestNo, String requestContent, String productCode, String parentNo) {
        ProcessUnitCreateReq req = new ProcessUnitCreateReq();
        req.setUnitCode(unitCode);
        req.setBizNo(bizNo);
        req.setRequestNo(requestNo);
        req.setRequestContent(requestContent);
        req.setParentNo(parentNo);
        req.setProductCode(productCode);
        return create(req);
    }
    
    public ProcessUnitCreateRes create(ProcessUnitCreateReq req) {
        if (req.getClientInfo() == null) {
            Map<String, String> clientInfo = new HashMap<String, String>();
            clientInfo.put("ip", NetUtils.getSysIp());
            req.setClientInfo(clientInfo);
        }        
        log.info("Create process unit request, {}", req);
        ExportResponse<ProcessUnitCreateRes> response = getProcessUnitExport().create(new ExportRequest(req));
        log.info("Create process unit respone:{}", response);
        ProcessUnitCreateRes res = ExportResponseUtil.unwrap(response);
        return res;
    }
    
    public void sendCreateMessage(String unitCode, String bizNo, String requestContent) {
        sendCreateMessage(unitCode, bizNo, UUIDUtil.getSimpleUUID(), requestContent);
    }
    
    public void sendCreateMessage(String unitCode, String bizNo, String requestNo, String requestContent) {
        this.sendCreateMessage(unitCode, bizNo, requestNo, requestContent, null);
    }
    
    public void sendCreateMessage(String unitCode, String bizNo, String requestNo, String requestContent, String productCode) {
        sendCreateMessage(unitCode, bizNo, requestNo, requestContent, productCode, null);
    }
    
    public void sendCreateMessage(String unitCode, String bizNo, String requestNo, String requestContent, String productCode, String parentNo) {
        ProcessUnitCreateReq req = new ProcessUnitCreateReq();
        req.setUnitCode(unitCode);
        req.setBizNo(bizNo);
        req.setRequestNo(requestNo);
        req.setRequestContent(requestContent);
        req.setProductCode(productCode);
        sendCreateMessage(req);
    }
    
    public void sendCreateMessage(ProcessUnitCreateReq req) {
        log.info("Create process unit message, {}", req);
        if (req.getClientInfo() == null) {
            Map<String, String> clientInfo = new HashMap<String, String>();
            clientInfo.put("ip", NetUtils.getSysIp());
            req.setClientInfo(clientInfo);
        }           
        getMessageSendService().sendMessage(req.getRequestNo(), createMessageTopic, JSON.toJSONString(req));
    }
    
    public ProcessUnitExecuteRes execute(ProcessUnitExecuteReq req) {
        if (req.getClientInfo() == null) {
            Map<String, String> clientInfo = new HashMap<String, String>();
            clientInfo.put("ip", NetUtils.getSysIp());
            req.setClientInfo(clientInfo);
        }        
        log.info("Execute process unit request, {}", req);
        ExportResponse<ProcessUnitExecuteRes> response = getProcessUnitExport().execute(new ExportRequest(req));
        log.info("Execute process unit respone:{}", response);
        ProcessUnitExecuteRes res = ExportResponseUtil.unwrap(response);
        return res; 
    }
    
    public void sendExecuteMessage(String instanceNo) {
        ProcessUnitExecuteReq req = new ProcessUnitExecuteReq();
        req.setProcessUnitInstanceNo(instanceNo);
        sendExecuteMessage(req);
    }
    
    public void sendExecuteMessage(String unitCode, String bizNo) {
        ProcessUnitExecuteReq req = new ProcessUnitExecuteReq();
        req.setUnitCode(unitCode);
        req.setBizNo(bizNo);
        sendExecuteMessage(req);
    }
    
    public void sendExecuteMessage(ProcessUnitExecuteReq req) {
        log.info("Execute process unit message, {}", req);
        getMessageSendService().sendMessage(req.getProcessUnitInstanceNo() + "-" + req.getUnitCode() + "-" + req.getBizNo() + "-" + UUID.randomUUID().toString(), executeMessageTopic, JSON.toJSONString(req));
    }
    
    public ProcessUnitUpdateRes update(String unitCode, String bizNo, String result, String responseContent) {
        return update(unitCode, bizNo, result, responseContent, null);
    }
    
    public ProcessUnitUpdateRes update(String unitCode, String bizNo, String result, String responseContent, Map<String, String> variables) {
        ProcessUnitUpdateReq req = new ProcessUnitUpdateReq();
        req.setUnitCode(unitCode);
        req.setBizNo(bizNo);
        req.setResult(result);
        req.setResponseContent(responseContent);
        req.setVariables(variables);
        String requestNo = UUIDUtil.getSimpleUUID();
        req.setRequestNo(requestNo);
        req.setResponseTime(new Date());
        req.setExecType(ProcessUnitConstants.EXEC_TYPE_SYNC);
        return update(req);
    }
    
    public ProcessUnitUpdateRes update(ProcessUnitUpdateReq req) {
        if (req.getClientInfo() == null) {
            Map<String, String> clientInfo = new HashMap<String, String>();
            clientInfo.put("ip", NetUtils.getSysIp());
            req.setClientInfo(clientInfo);
        }
        log.info("Update process unit request, {}", req);
        ExportResponse<ProcessUnitUpdateRes> response = getProcessUnitExport()
                .update(new ExportRequest<ProcessUnitUpdateReq>(req));
        log.info("Update process unit response, {}", req);
        ProcessUnitUpdateRes res = ExportResponseUtil.unwrap(response);
        return res;
    }
    
    public ProcessUnitCreateAndExecuteRes createAndExecuteAfterCommit(ProcessUnitCreateAndExecuteReq req) {
        ProcessUnitCreateAndExecuteRes res = new ProcessUnitCreateAndExecuteRes();
        ProcessUnitCreateRes createRes =create(req.getCreateReq());
        res.setCreateRes(createRes);
        req.getExecParam().setUnitCode(req.getCreateReq().getUnitCode());
        req.getExecParam().setBizNo(req.getCreateReq().getBizNo());
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                ExecResult result = executor.execute(req.getExecParam());
                res.setExecResult(result);
            }
        });
        return res;
    }
    
    
    

    private ProcessUnitExport getProcessUnitExport() {
        if (processUnitExport == null) {
            processUnitExport = ObjectFactorys.getDefault().getObject(ProcessUnitExport.class);
        }
        return processUnitExport;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public String getCreateMessageTopic() {
        return createMessageTopic;
    }

    public void setCreateMessageTopic(String createMessageTopic) {
        this.createMessageTopic = createMessageTopic;
    }
    
    

}

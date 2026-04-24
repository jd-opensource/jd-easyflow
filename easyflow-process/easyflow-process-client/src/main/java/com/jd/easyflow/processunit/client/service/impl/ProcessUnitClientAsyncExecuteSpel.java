package com.jd.easyflow.processunit.client.service.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.el.ElFactory;
import com.jd.easyflow.processunit.client.ProcessUnitClient;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;
import com.jd.easyflow.spel.SpelHelper;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitClientAsyncExecuteSpel {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitClientAsyncExecuteSpel.class);
    
    private Executor executor;
    
    private ProcessUnitClient processUnitClient;
    
    public ExecResult execute(String executeExp, ExecParam param) {
        Executor executor = getExecutor(param);
        executor.execute(() -> {
            log.info("Process unit client async exec start:{},{}", executeExp, param);
            ExecParam execParam = new ExecParam();
            execParam.setUnitCode(param.getUnitCode());
            execParam.setBizNo(param.getBizNo());
            execParam.setRequestNo("_E_" + param.getExecutionNo());
            param.setRequestContent(param.getRequestContent());
            param.setInvoker(context -> {
                Map<String, Object> spelContext = new HashMap<String, Object>();
                spelContext.put("param", param);
                ExecResult result = ElFactory.get().evalWithDefaultContext(executeExp, spelContext, false);
                context.getResult().setAutoRunFlag(result.getAutoRunFlag());
                context.getResult().setNextAutoRunTime(result.getNextAutoRunTime());
                return result;
            });
            param.setResultFunction(context -> {
                ExecResult res = (ExecResult) context.getResult().getExecResult();
                return res.getResult();
            });
            param.setResponseContentFunction(context -> {
                ExecResult res = (ExecResult) context.getResult().getExecResult();
                return res.getResponseContent();
            });
            try {
                processUnitClient.call(param);
                log.info("Process unit client async exec end");
            } catch (Throwable t) {
                log.error("Process unit client async execute excetpion:{},{}", executeExp, param, t);
            }
        });
        ExecResult result = new ExecResult();
        result.setResult(ProcessUnitConstants.RESULT_UNKNOWN);
        return result;
    }
    
    protected Executor getExecutor(ExecParam param) {
        return executor;
    }



    public ProcessUnitClient getProcessUnitClient() {
        return processUnitClient;
    }

    public void setProcessUnitClient(ProcessUnitClient processUnitClient) {
        this.processUnitClient = processUnitClient;
    }

    public Executor getExecutor() {
        return executor;
    }

    public void setExecutor(Executor executor) {
        this.executor = executor;
    }
    
    

}

package com.jd.easyflow.processunit.client.service.impl;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.net.NetUtils;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;
import com.jd.easyflow.processunit.spi.client.ProcessUnitClientService;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealReq;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealRes;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitClientServiceImpl implements ProcessUnitClientService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitClientServiceImpl.class);


    private static final String[] RESULTS = new String[] { ProcessUnitConstants.RESULT_DOING,
            ProcessUnitConstants.RESULT_EXCEPTION, ProcessUnitConstants.RESULT_FAIL,
            ProcessUnitConstants.RESULT_SUCCESS };

    private AsyncClientProcessUnitExecutor asyncClientProcessUnitExecutor;

    @Override
    public AsyncCallRealRes asyncCallReal(AsyncCallRealReq req) {
        log.info("Process unit client receive SPI request:" + req);
        ExecParam param = new ExecParam();
        param.setExecuteExp(req.getExecuteExp());
        param.setRequestContent(req.getRequestContent());
        param.setExecutionNo(req.getExecutionNo());
        param.setRequestContext(req.getRequestContext());
        param.setInstanceNo(req.getInstanceNo());
        param.setUnitCode(req.getUnitCode());
        param.setBizNo(req.getBizNo());
        ExecContext context = new ExecContext(param);
        context.setVariables(req.getVariables());
        param.setExecContext(context);
        AsyncCallRealRes res = new AsyncCallRealRes();
        try {
            ExecResult result = asyncClientProcessUnitExecutor.execute(param);
            log.info("Process unit client execute result:" + result);
            boolean contains = false;
            for (String s : RESULTS) {
                if (s.equals(result.getResult())) {
                    contains = true;
                    break;
                }
            }
            if (! contains) {
                throw new EasyFlowException("Process unit client result illegal");
            }
            res.setResult(result.getResult());
            res.setResponseContent(result.getResponseContent());
            Map<String, String> variables = null;
            if (result.getExecContext() != null) {
                variables = result.getExecContext().getVariables();
            } else {
                variables = context.getVariables();
            }
            res.setVariables(variables);
            res.setAutoRunFlag(result.getAutoRunFlag());
            res.setNextAutoRunTime(result.getNextAutoRunTime());
            res.setClientInfo(getClientInfo(context));
        } catch (Throwable t) {
            log.error("Process unit client execute exception, " + t.getMessage(), t);
            res.setResult(ProcessUnitConstants.RESULT_EXCEPTION);
            res.setVariables(context.getVariables());
            Map<String, Object> exceptionInfo = new HashMap<>();
            exceptionInfo.put("pu_exception", t.getClass());
            if (t.getMessage() != null) {
                exceptionInfo.put("pu_message",
                        t.getMessage().length() < 2000 ? t.getMessage() : t.getMessage().substring(0, 2000));
            }
            res.setResponseContent(JSON.toJSONString(exceptionInfo));
            res.setClientInfo(getClientInfo(context));
        }
        return res;
    }
    
    protected Map<String, String> getClientInfo(ExecContext context) {
        Map<String, String> clientInfo = new HashMap<String, String>();
        clientInfo.put("ip", NetUtils.getSysIp());
        return clientInfo;
    }

    public AsyncClientProcessUnitExecutor getAsyncClientProcessUnitExecutor() {
        return asyncClientProcessUnitExecutor;
    }

    public void setAsyncClientProcessUnitExecutor(AsyncClientProcessUnitExecutor asyncClientProcessUnitExecutor) {
        this.asyncClientProcessUnitExecutor = asyncClientProcessUnitExecutor;
    }

}

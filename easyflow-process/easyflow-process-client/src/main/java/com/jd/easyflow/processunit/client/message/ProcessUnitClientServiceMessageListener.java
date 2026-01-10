package com.jd.easyflow.processunit.client.message;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.message.BaseMessageListener;
import com.jd.easyflow.message.Message;
import com.jd.easyflow.processunit.client.ProcessUnitClient;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.spi.client.ProcessUnitClientService;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealReq;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealRes;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessUnitClientServiceMessageListener extends BaseMessageListener<AsyncCallRealReq> {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitClientServiceMessageListener.class);

    
    private List<String> unitCodeList;
    
    private ProcessUnitClientService processUnitClientService;
    
    private ProcessUnitClient processUnitClient;

    @Override
    public boolean validate(AsyncCallRealReq req, Message message) {
        if (unitCodeList == null || !unitCodeList.contains(req.getUnitCode())) {
            return false;
        }
        return true;
    }

    @Override
    public void process(AsyncCallRealReq req, Message message) {
        log.info("Process unit client receive async call real message:{}", req);
        ExecParam param = new ExecParam();
        param.setUnitCode(req.getUnitCode());
        param.setBizNo(req.getBizNo());
        param.setRequestNo("_E_" + req.getExecutionNo());
        param.setRequestContent(req.getRequestContent());
        param.setInvoker(context -> {
            AsyncCallRealRes res = processUnitClientService.asyncCallReal(req);
            context.setVariables(res.getVariables());
            context.getResult().setAutoRunFlag(res.getAutoRunFlag());
            context.getResult().setNextAutoRunTime(res.getNextAutoRunTime());
            return res;
        });
        param.setResultFunction(context -> {
            AsyncCallRealRes res = (AsyncCallRealRes) context.getResult().getExecResult();
            return res.getResult();
        });
        param.setResponseContentFunction(context -> {
            AsyncCallRealRes res = (AsyncCallRealRes) context.getResult().getExecResult();
            return res.getResponseContent();
        });
        processUnitClient.call(param);
    }

    public ProcessUnitClientService getProcessUnitClientService() {
        return processUnitClientService;
    }

    public void setProcessUnitClientService(ProcessUnitClientService processUnitClientService) {
        this.processUnitClientService = processUnitClientService;
    }

    public ProcessUnitClient getProcessUnitClient() {
        return processUnitClient;
    }

    public void setProcessUnitClient(ProcessUnitClient processUnitClient) {
        this.processUnitClient = processUnitClient;
    }

    public List<String> getUnitCodeList() {
        return unitCodeList;
    }

    public void setUnitCodeList(List<String> unitCodeList) {
        this.unitCodeList = unitCodeList;
    }
    

}

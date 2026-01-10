package com.jd.easyflow.processunit.infrastructure.gateway;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.domain.gateway.ProcessUnitClientGateway;
import com.jd.easyflow.processunit.domain.model.vo.ExecContext;
import com.jd.easyflow.processunit.spi.client.ProcessUnitClientService;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealReq;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealRes;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitClientGatewayImpl implements ProcessUnitClientGateway {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessUnitClientGatewayImpl.class);


    @Override
    public void ayncCallReal(ExecContext context) {
        AsyncCallRealReq req = new AsyncCallRealReq();
        req.setExecuteExp((String) context.getProcessUnit().getConfig(ProcessUnitConstants.CONF_ASYNC_RUN_EXP));
        req.setExecutionNo(context.getExecution().getExecutionNo());
        req.setRequestContent(context.getInstance().getRequestContent());
        req.setRequestContext(context.getParam().getRequestContext());
        req.setInstanceNo(context.getInstance().getInstanceNo());
        req.setUnitCode(context.getUnitCode());
        req.setBizNo(context.getBizNo());
        
        Map<String, String> variables = JSON.parseObject(context.getInstance().getVars(), Map.class);
        context.setVariables(variables);
        req.setVariables(variables);
        
        String serviceId = (String) context.getProcessUnit().getConfig(ProcessUnitConstants.CONF_ASYNC_SERVICE_ID);
        Map<String, Object> serviceConf = (Map<String, Object>) context.getProcessUnit().getConfig(ProcessUnitConstants.CONF_ASYNC_SERVICE_CONF);
        req.setServiceConf(serviceConf);
        log.info("Invoke client request is:{}, serviceId:{}", req, serviceId);
        try {
            ProcessUnitClientService processUnitClientService = getProcessUnitClientService(serviceId, context);
            AsyncCallRealRes res = processUnitClientService.asyncCallReal(req);
            log.info("Invoke client response :{}", res);
            context.getResult().setResult(res.getResult());
            context.getResult().setResponseContent(res.getResponseContent());
            context.setVariables(res.getVariables());
            context.getResult().setAutoRunFlag(res.getAutoRunFlag());
            context.getResult().setNextAutoRunTime(res.getNextAutoRunTime());
            context.setClientInfo(res.getClientInfo());
        } catch (Exception e) {
            log.error("Invoke client exception," + e.getMessage(), e);
            context.getResult().setResult(ProcessUnitConstants.RESULT_EXCEPTION);
            context.getResult().setResponseContent(null);
        }
    }
    
    protected ProcessUnitClientService getProcessUnitClientService(String serviceId, ExecContext context) {
        return ObjectFactorys.getDefault()
                .getObject(ProcessUnitClientService.class, serviceId);
    }

}

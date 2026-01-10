package com.jd.easyflow.processunit.client.service.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.net.NetUtils;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.processunit.adapter.export.ProcessUnitExport;
import com.jd.easyflow.processunit.adapter.export.dto.ExecPolicyDTO;
import com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallReq;
import com.jd.easyflow.processunit.adapter.export.dto.SyncAfterCallRes;
import com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallReq;
import com.jd.easyflow.processunit.adapter.export.dto.SyncBeforeCallRes;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecPolicy;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;

/**
 *
 * @author liyuliang5
 * 
 */
public class SyncClientProcessUnitExecutor extends BaseProcessUnitExecutor {
    
    private static final Logger log = LoggerFactory.getLogger(SyncClientProcessUnitExecutor.class);


    protected boolean disable = false;

    private ProcessUnitExport processUnitExport;

    @Override
    public ExecPolicy beforeCall(ExecContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Start before call");
        }
        if (disable) {
            return new ExecPolicy(ProcessUnitConstants.POLICY_REQUEST);
        }
        SyncBeforeCallReq req = new SyncBeforeCallReq();
        req.setUnitCode(context.getParam().getUnitCode());
        req.setBizNo(context.getParam().getBizNo());
        req.setRequestNo(context.getParam().getRequestNo());
        req.setRequestContent(context.getParam().getRequestContent());
        req.setProductCode(context.getParam().getProductCode());
        req.setParentNo(context.getParam().getParentNo());
        req.setClientInfo(getClientInfo(context));
        log.info("Sync before call request:{}", req);
        ExportResponse<SyncBeforeCallRes> response = getProcessUnitExport().syncBeforeCall(new ExportRequest(req));
        log.info("Sync before call response:{}", response);
        SyncBeforeCallRes res = ExportResponseUtil.unwrap(response);
        ExecPolicyDTO policyDto = res.getExecPolicy();
        ExecPolicy policy = new ExecPolicy();
        policy.setPolicyType(policyDto.getPolicyType());
        policy.setPolicyData(policyDto.getPolicyData());
        policy.setResult(policyDto.getResult());
        policy.setRequestContent(policyDto.getRequestContent());
        policy.setResponseContent(policyDto.getResponseContent());
        context.setUnitConf(res.getUnitConf());
        context.setExecutionNo(res.getExecutionNo());
        context.setBizNo(context.getParam().getBizNo());
        context.setInstanceNo(res.getInstanceNo());
        context.setServerContextData(res.getContextData());
        context.setUnitCode(context.getParam().getUnitCode());
        context.getResult().setInstanceNo(res.getInstanceNo());
        if (res.getVariables() != null) {
            context.setVariables(new ConcurrentHashMap<String, String>(res.getVariables()));
        }
        return policy;
    }
    
    protected Map<String, String> getClientInfo(ExecContext context) {
        Map<String, String> clientInfo = new HashMap<String, String>();
        clientInfo.put("ip", NetUtils.getSysIp());
        return clientInfo;
    }

    @Override
    public void call(ExecPolicy policy, ExecContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Start execute");
        }
        switch (policy.getPolicyType()) {
            case ProcessUnitConstants.POLICY_EXCEPTION: {
                throw new UnsupportedOperationException();
            }
            case ProcessUnitConstants.POLICY_OLD: {
                returnOldResult(context);
                break;
            }
            case ProcessUnitConstants.POLICY_REQUEST: {
                callReal(context);
                break;
            }
            default:
                throw new IllegalArgumentException("policy illegal:" + policy);
        }
        context.setExecuted(true);
        if (context.getResult().getExecException() != null
                && Boolean.FALSE.equals(context.getUnitConf(ProcessUnitConstants.CONF_CATCH_EXCEPTION))) {
            Throwable t = context.getResult().getExecException();
            sneakyThrow(t);
        }
    }

    @Override
    public void afterCall(ExecContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Start after call");
        }
        if (disable) {
            return;
        }
        if (context.getExecutionNo() == null) {
            log.warn("Execution no is null, return");
            return;
        }
        context.getResult().setResponseContent(context.getParam().getResponseContentFunction().apply(context));
        context.getResult().setResult(context.getParam().getResultFunction().apply(context));
        SyncAfterCallReq req = new SyncAfterCallReq();
        req.setExecutionNo(context.getExecutionNo());
        req.setResult(context.getResult().getResult());
        req.setResponseContent(context.getResult().getResponseContent());
        req.setInstanceNo(context.getInstanceNo());
        req.setBizNo(context.getBizNo());
        req.setUnitCode(context.getUnitCode());
        req.setContextData(context.getServerContextData());
        req.setVariables(context.getVariables());
        req.setAutoRunFlag(context.getResult().getAutoRunFlag());
        req.setNextAutoRunTime(context.getResult().getNextAutoRunTime());
        log.info("Sync after call request:{}", req);
        ExportResponse<SyncAfterCallRes> response = getProcessUnitExport().syncAfterCall(new ExportRequest(req));
        log.info("Sync after call response:{}", response);
        ExportResponseUtil.unwrap(response);

    }
    
    protected void callReal(ExecContext context) {
        if (log.isDebugEnabled()) {
            log.debug("Start call real");
        }
        try {
            Object result = context.getParam().getInvoker().apply(context);
            context.getResult().setExecResult(result);
            if (log.isDebugEnabled()) {
                log.debug("Call real result:{}", result);
            }
        } catch (Throwable t) {//NOSONAR
            log.error("Call real exception:" + t.getMessage(), t);
            context.getResult().setExecException(t);
        }
    }

    protected void returnOldResult(ExecContext context) {
        log.info("Start return old result");
        try {
            Object result = context.getParam().getOldResultFunction().apply(context);
            context.getResult().setExecResult(result);
            log.info("return old result:" + result);
        } catch (Throwable t) {//NOSONAR
            log.error("return old result exception" + t.getMessage(), t);
            context.getResult().setExecException(t);
        }
    }
    
    private ProcessUnitExport getProcessUnitExport() {
        if (processUnitExport == null) {
            processUnitExport = ObjectFactorys.getDefault().getObject(ProcessUnitExport.class);
        }
        return processUnitExport;
    }

    public static RuntimeException sneakyThrow(Throwable t) {
        if (t == null) {
            throw new NullPointerException("t");
        }
        return SyncClientProcessUnitExecutor.<RuntimeException>sneakyThrow0(t);
    }

    private static <T extends Throwable> T sneakyThrow0(Throwable t) throws T {
        throw (T) t;
    }

    public void setProcessUnitExport(ProcessUnitExport processUnitExport) {
        this.processUnitExport = processUnitExport;
    }
    
}

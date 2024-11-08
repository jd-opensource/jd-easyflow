package com.jd.easyflow.process.client.task.biz.impl;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.client.runtime.core.ProcessException;
import com.jd.easyflow.process.client.task.TaskConstants;
import com.jd.easyflow.process.client.task.TaskErrorCode;
import com.jd.easyflow.process.client.task.biz.TaskBizService;
import com.jd.easyflow.process.client.task.biz.dto.TaskBizParam;
import com.jd.easyflow.process.client.task.biz.dto.TaskBizResult;
import com.jd.easyflow.process.spi.client.ProcessTaskClientService;
import com.jd.easyflow.process.spi.client.dto.ProcessTaskCallReq;
import com.jd.easyflow.process.spi.client.dto.ProcessTaskCallRes;
import com.jd.easyflow.process.spi.client.enums.ProcessClientResponseCode;

/**
 * @author liyuliang5
 *
 */
public class CallTaskBizService implements TaskBizService {

    @Override
    public TaskBizResult execute(TaskBizParam param) {
        String callService = (String) param.getBizServiceParam().get(TaskConstants.TASK_BIZ_SERVICE_PARAM_CALL);
        String[] callServiceInfo = callService.split(":");
        String providerId = callServiceInfo.length == 1 ? null : callServiceInfo[0];
        String serviceId = callServiceInfo.length == 1 ? callServiceInfo[0] : callServiceInfo[1];
        ProcessTaskClientService processTaskClientService = ObjectFactorys.getDefault()
                .getObject(ProcessTaskClientService.class, providerId, serviceId);
        ProcessTaskCallReq req = convert(param);
        ClientResponse<ProcessTaskCallRes> response = processTaskClientService
                .call(new ClientRequest<ProcessTaskCallReq>(req));
        if (response.getResCode() != null
                && !ProcessClientResponseCode.SUCCESS.getCode().equals(response.getResCode())) {
            Map<String, Object> data = new HashMap<>();
            data.put("bizResCode", response.getResCode());
            data.put("bizResDesc", response.getResDesc());
            ProcessException exception = new ProcessException(TaskErrorCode.PTC_0101.name(),
                    TaskErrorCode.PTC_0101.getDesc());
            exception.setData(data);
            throw exception;
        }
        return null;
    }

    private ProcessTaskCallReq convert(TaskBizParam param) {
        ProcessTaskCallReq req = new ProcessTaskCallReq();
        req.setBizNo(param.getBizNo());
        Map<String, Object> bizServiceParam = param.getBizServiceParam();
        Map<String, Object> callParam = bizServiceParam == null ? null : (Map<String, Object>) bizServiceParam.get("callParam");
        req.setCallParam(callParam);
        req.setEvent(param.getEvent());
        req.setInstanceBizData(param.getInstanceBizData());
        req.setInstanceBizStatus(param.getInstanceBizStatus());
        req.setProcessInstanceNo(param.getProcessInstanceNo());
        req.setProcessType(param.getProcessType());
        req.setTaskBizCode(param.getTaskBizCode());
        return req;
    }

}

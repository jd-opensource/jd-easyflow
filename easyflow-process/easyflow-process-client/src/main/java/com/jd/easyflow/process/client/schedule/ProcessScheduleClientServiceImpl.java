package com.jd.easyflow.process.client.schedule;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.common.client.dto.ClientResponseCode;
import com.jd.easyflow.process.client.runtime.core.ProcessEngine;
import com.jd.easyflow.process.client.runtime.core.ProcessException;
import com.jd.easyflow.process.client.runtime.core.ProcessParam;
import com.jd.easyflow.process.client.runtime.core.ProcessResult;
import com.jd.easyflow.process.spi.client.ProcessScheduleClientService;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteReq;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteRes;

/**
 * @author liyuliang5
 *
 */
public class ProcessScheduleClientServiceImpl implements ProcessScheduleClientService {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessScheduleClientServiceImpl.class);


    private ProcessEngine processEngine;

    @Override
    public ClientResponse execute(ClientRequest<ProcessExecuteReq> request) {
        log.info("Start process schedule client execute, req:{}", request);
        ProcessExecuteReq req = request.getData();
        ProcessParam processParam = new ProcessParam();
        processParam.setProcessId(req.getProcessId());
        processParam.setNodeIds(req.getNodeIds());
        processParam.setParam(req.getParam());
        processParam.setDataMap(req.getDataMap());
        try {
            ProcessResult processResult = processEngine.execute(processParam);
            log.info("End process schedule client execute");
            ProcessExecuteRes res = new ProcessExecuteRes();
            res.setProcessInstanceNo(processResult.getProcessInstanceNo());
            return ClientResponse.build4Success(res);
        } catch (Exception e) {
            log.error("Process schedule client exception, " + e.getMessage(), e);
            String code = ClientResponseCode.EXCEPTION.getCode();
            String message = "Flow engine execute exception," + e.getMessage();
            Map<String, Object> ext = null;
            if (e instanceof ProcessException) {
                ProcessException processException = (ProcessException) e;
                if (processException.getCode() != null) {
                    code = processException.getCode();
                }
                message = ((ProcessException) e).getMessage();
                if (processException.getData() != null) {
                    ext = new HashMap<>();
                    ext.put("errorData", processException.getData());
                }
            }
            return ClientResponse.build4Failed(code, message, ext);
        }
    }

    public ProcessEngine getProcessEngine() {
        return processEngine;
    }

    public void setProcessEngine(ProcessEngine processEngine) {
        this.processEngine = processEngine;
    }

}

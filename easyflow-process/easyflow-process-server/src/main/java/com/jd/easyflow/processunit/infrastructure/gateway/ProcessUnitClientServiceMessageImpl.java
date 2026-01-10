package com.jd.easyflow.processunit.infrastructure.gateway;

import org.springframework.beans.factory.annotation.Value;

import com.jd.easyflow.message.MessageSendService;
import com.jd.easyflow.processunit.domain.constant.ProcessUnitConstants;
import com.jd.easyflow.processunit.spi.client.ProcessUnitClientService;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealReq;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealRes;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessUnitClientServiceMessageImpl implements ProcessUnitClientService {

    private MessageSendService messageSendService;

    @Value("${easyflow.processunit.clientExecuteMessageTopic:PROCESS_UNIT_CLIENT_EXECUTE}")
    private String defaultMessageTopic;

    @Override
    public AsyncCallRealRes asyncCallReal(AsyncCallRealReq req) {
        String topic = req.getServiceConf() == null ? null : (String) req.getServiceConf().get("messageTopic");
        if (topic == null) {
            topic = defaultMessageTopic;
        }
        messageSendService.sendMessage(req.getExecutionNo() + "-" + req.getInstanceNo() + "-" + req.getBizNo(), topic,
                JSON.toJSONString(req));
        AsyncCallRealRes res = new AsyncCallRealRes();
        res.setResult(ProcessUnitConstants.RESULT_UNKNOWN);
        res.setVariables(req.getVariables());
        return res;
    }

    public MessageSendService getMessageSendService() {
        return messageSendService;
    }

    public void setMessageSendService(MessageSendService messageSendService) {
        this.messageSendService = messageSendService;
    }

    public String getDefaultMessageTopic() {
        return defaultMessageTopic;
    }

    public void setDefaultMessageTopic(String defaultMessageTopic) {
        this.defaultMessageTopic = defaultMessageTopic;
    }

}

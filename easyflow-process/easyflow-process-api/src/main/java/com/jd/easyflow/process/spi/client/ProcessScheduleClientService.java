package com.jd.easyflow.process.spi.client;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteReq;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteRes;

/**
 * @author liyuliang5
 *
 */
public interface ProcessScheduleClientService {

    ClientResponse<ProcessExecuteRes> execute(ClientRequest<ProcessExecuteReq> req);
}

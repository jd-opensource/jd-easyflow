package com.jd.easyflow.process.spi.client;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.process.spi.client.dto.ProcessExecuteReq;

/**
 * @author liyuliang5
 *
 */
public interface ProcessScheduleClientService {

    ClientResponse execute(ClientRequest<ProcessExecuteReq> req);
}

package com.jd.easyflow.process.spi.client;

import com.jd.easyflow.common.client.dto.ClientRequest;
import com.jd.easyflow.common.client.dto.ClientResponse;
import com.jd.easyflow.process.spi.client.dto.ProcessTaskCallReq;
import com.jd.easyflow.process.spi.client.dto.ProcessTaskCallRes;

/**
 * @author liyuliang5
 *
 */
public interface ProcessTaskClientService {

    ClientResponse<ProcessTaskCallRes> call(ClientRequest<ProcessTaskCallReq> req);
}

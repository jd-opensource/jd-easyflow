package com.jd.easyflow.processunit.spi.client;

import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealReq;
import com.jd.easyflow.processunit.spi.client.dto.AsyncCallRealRes;

/**
 * 
 * @author liyuliang5
 *
 */
public interface ProcessUnitClientService {

    AsyncCallRealRes asyncCallReal(AsyncCallRealReq req);
}

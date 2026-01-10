 package com.jd.easyflow.processunit.domain.gateway;

import com.jd.easyflow.processunit.domain.model.vo.ExecContext;

/**
  * @author liyuliang5
  * 
  */
 public interface ProcessUnitClientGateway {

     void ayncCallReal(ExecContext context);
}

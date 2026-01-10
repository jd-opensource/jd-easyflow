package com.jd.easyflow.processunit.domain.service;

import com.jd.easyflow.processunit.domain.model.vo.ExecParam;
import com.jd.easyflow.processunit.domain.model.vo.ExecResult;

/**
 * @author liyuliang5
 * 
 */
public interface ProcessUnitExecutor {

    ExecResult execute(ExecParam param);
}

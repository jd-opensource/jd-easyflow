package com.jd.easyflow.processunit.client.service;

import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;

public interface ProcessUnitExecutor {

    ExecResult execute(ExecParam param);
}

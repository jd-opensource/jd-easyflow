package com.jd.easyflow.processunit.client.service.impl;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.el.ElFactory;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;

/**
 * @author liyuliang5
 *
 */
public class AsyncClientProcessUnitExecutor extends BaseProcessUnitExecutor {

    @Override
    public ExecResult execute(ExecParam param) {
        Map<String, Object> spelContext = new HashMap<String, Object>();
        spelContext.put("param", param);
        return ElFactory.get().evalWithDefaultContext(param.getExecuteExp(), spelContext, false);
    }
}

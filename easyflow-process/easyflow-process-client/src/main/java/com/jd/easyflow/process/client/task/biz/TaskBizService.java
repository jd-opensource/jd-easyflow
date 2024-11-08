package com.jd.easyflow.process.client.task.biz;

import com.jd.easyflow.process.client.task.biz.dto.TaskBizParam;
import com.jd.easyflow.process.client.task.biz.dto.TaskBizResult;

/**
 * @author liyuliang5
 *
 */
public interface TaskBizService {

    TaskBizResult execute(TaskBizParam param);
}

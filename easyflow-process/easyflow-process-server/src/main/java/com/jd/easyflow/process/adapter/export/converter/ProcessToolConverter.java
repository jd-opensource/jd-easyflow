package com.jd.easyflow.process.adapter.export.converter;

import com.jd.easyflow.process.adapter.export.dto.instance.RollbackNodeReq;
import com.jd.easyflow.process.domain.model.vo.RollbackNodeReqVO;

/**
 * @author liyuliang5
 */
public class ProcessToolConverter {
    
    public static final ProcessToolConverter INSTANCE = new ProcessToolConverter();
    
    public RollbackNodeReqVO convert(RollbackNodeReq req) {
        if (req == null) {
            return null;
        }
        RollbackNodeReqVO vo = new RollbackNodeReqVO();
        vo.setProcessInstanceNo(req.getProcessInstanceNo());
        vo.setRollbackSubProcess(req.isRollbackSubProcess());
        vo.setRollbackTask(req.isRollbackTask());
        vo.setTargetNodeId(req.getTargetNodeId());
        vo.setTargetNodeInstanceNo(req.getTargetNodeInstanceNo());
        return vo;
    }

}

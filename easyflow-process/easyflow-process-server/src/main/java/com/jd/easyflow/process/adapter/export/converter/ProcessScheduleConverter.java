package com.jd.easyflow.process.adapter.export.converter;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.process.adapter.export.dto.schedule.ScheduleProcessReq;
import com.jd.easyflow.process.adapter.export.dto.schedule.ScheduleProcessRes;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessReqVO;
import com.jd.easyflow.process.domain.model.vo.ScheduleProcessResVO;

/**
 * @author liyuliang5
 *
 */
public class ProcessScheduleConverter {

    public static final ProcessScheduleConverter INSTANCE = new ProcessScheduleConverter();


    public ScheduleProcessReqVO convert(ScheduleProcessReq req) {
        if ( req == null ) {
            return null;
        }

        ScheduleProcessReqVO scheduleProcessReqVO = new ScheduleProcessReqVO();

        Map<String, Object> map = req.getDataMap();
        if ( map != null ) {
            scheduleProcessReqVO.setDataMap(req.getDataMap());
        }
        String[] nodeIds = req.getNodeIds();
        if ( nodeIds != null ) {
            scheduleProcessReqVO.setNodeIds( Arrays.copyOf( nodeIds, nodeIds.length ) );
        }
        scheduleProcessReqVO.setParam( req.getParam() );
        scheduleProcessReqVO.setProcessId( req.getProcessId() );

        return scheduleProcessReqVO;
    }
    
    public ScheduleProcessRes convert(ScheduleProcessResVO resVo) {
        if (resVo == null) {
            return null;
        }
        ScheduleProcessRes res = new ScheduleProcessRes();
        res.setProcessInstanceNo(resVo.getProcessInstanceNo());
        res.setResult(resVo.getResult());
        Map<String, Object> map = resVo.getDataMap();
        if ( map != null ) {
            res.setDataMap( new HashMap<String, Object>( map ) );
        }
        return res;
    }
}
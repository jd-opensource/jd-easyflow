package com.jd.easyflow.action;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.aspectj.lang.ProceedingJoinPoint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.utils.json.JSON;

public class SimpleExportAopFunction implements Function<ActionInfo<?, ?>, Void> {

    private static final Logger log = LoggerFactory.getLogger(SimpleExportAopFunction.class);

    private static final String FORMAT_JSON = "json";

    private String format;

    @Override
    public Void apply(ActionInfo actionInfo) {
        try {
            // log param
            Object param = actionInfo.getParam();
            String paramStr = null;
            if (format == null) {
                paramStr = toString(param);
            } else if (FORMAT_JSON.equals(format)) {
                paramStr = JSON.toJSONString(actionInfo.getParam());
            }
            if (log.isInfoEnabled()) {
                log.info(actionInfo.getActionCode() + " param:" + paramStr);
            }

            // invoke
            ProceedingJoinPoint point = (ProceedingJoinPoint) actionInfo.get(ActionConstants.CTX_JOIN_POINT);
            Object result = point.proceed();
            actionInfo.setResult(result);

            // log result
            String resultStr = null;
            if (format == null) {
                resultStr = toString(result);
            } else {
                resultStr = JSON.toJSONString(result);
            }
            if (log.isInfoEnabled()) {
                log.info("result:" + resultStr);
            }
        } catch (Throwable t) {
            log.error("Process exception, " + t.getMessage(), t);
            ExportResponse<Object> response = new ExportResponse<>();
            String code = ExportResponseCode.EXCEPTION.getCode();
            String msg = ExportResponseCode.EXCEPTION.getDesc();
            if (t instanceof UserException) {
                code = ((UserException) t).getCode();
                if (code == null) {
                    code = ExportResponseCode.FAIL.getCode();
                }
                msg = ((EasyFlowException) t).getInfo();
                Object errorData = ((UserException) t).getData();
                if (errorData != null) {
                    Map<String, Object> ext = new HashMap<>();
                    ext.put("errorData", errorData);
                    response.setExt(ext);
                }
            }
            response.setResCode(code);
            response.setResDesc(msg);
            actionInfo.setResult(response);
        }
        return null;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }
    
    private String toString(Object o) {
        if (o == null) {
            return "null";
        }
        if (o instanceof Object[]) {
            return Arrays.toString((Object[]) o);
        }
        return o.toString();
    }
    

}

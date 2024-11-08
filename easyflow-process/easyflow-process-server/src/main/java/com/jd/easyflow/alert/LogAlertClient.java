package com.jd.easyflow.alert;

import java.util.Arrays;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class LogAlertClient implements AlertClient {

    private static final Logger logger = LoggerFactory.getLogger(AlertUtil.class);

    @Override
    public void alert(AlertParam alertParam) {
        StringBuilder builder = new StringBuilder("LogAlert: ");
        if (alertParam != null) {
            if (alertParam.getAlertCode() != null) {
                builder.append(" AlertCode:").append(alertParam.getAlertCode());
            }
            if (alertParam.getAlertKeys() != null) {
                builder.append(" AlertKeys:").append(Arrays.toString(alertParam.getAlertKeys()));
            }
            if (alertParam.getMessage() != null) {
                builder.append("AlertMessage:" + alertParam.getMessage());
            }
            if (alertParam.getAlertObject() != null) {
                builder.append(" AlertObject:").append(alertParam.getAlertObject());
            }
            if (alertParam.getData() != null) {
                builder.append(" AlertData:").append(alertParam.getData());
            }
            if (alertParam.getProductCode() != null) {
                builder.append(" AlertProductCode").append(alertParam.getProductCode());
            }
        }
        logger.error(builder.toString());
        if (alertParam.getThrowable() != null) {
            Throwable t = alertParam.getThrowable();
            logger.error("ExceptionAlert:" + t.getMessage(), t);
        }
    }

}

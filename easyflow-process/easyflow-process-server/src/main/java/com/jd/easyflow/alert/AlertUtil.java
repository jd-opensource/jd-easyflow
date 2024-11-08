package com.jd.easyflow.alert;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author liyuliang5
 * 
 */
public class AlertUtil {

    private static final Logger logger = LoggerFactory.getLogger(AlertUtil.class);

    private static AlertClient alertClient;

    public static void alert(Object... objects) {
        AlertParam param = new AlertParam();
        String message = concat(objects);
        param.setMessage(message);
        if (objects[objects.length - 1] instanceof Throwable) {
            Throwable t = (Throwable) objects[objects.length - 1];
            param.setThrowable(t);
        }
        getAlertClient().alert(param);
    }

    public static void doAlert(String alertCode, String message, Throwable t) {
        doAlert(alertCode, null, message, t, null, null, null);
    }
    
    public static void doAlert(String alertCode, String message, Throwable t, Object alertObject) {
        doAlert(alertCode, null, message, t, alertObject, null, null);
    }
    
    public static void doAlert(String alertCode, String message, Throwable t, Object alertObject, String productCode) {
        doAlert(alertCode, null, message, t, alertObject, null, productCode);
    }

    public static void doAlert(String alertCode, String[] alertKeys, String message, Throwable t, Object alertObject,
            Map<String, Object> data, String productCode) {
        AlertParam alertParam = new AlertParam();
        alertParam.setAlertCode(alertCode);
        alertParam.setMessage(message);
        alertParam.setAlertKeys(alertKeys);
        alertParam.setAlertObject(alertObject);
        alertParam.setProductCode(productCode);
        alertParam.setData(data);
        alertParam.setThrowable(t);
        getAlertClient().alert(alertParam);
    }

    private static String concat(Object... objects) {
        StringBuilder sb = new StringBuilder();
        concat(sb, objects);
        return sb.toString();
    }

    private static void concat(StringBuilder sb, Object... objects) {
        for (int i = 0; i < objects.length; i++) {
            if ((i + 1) == objects.length && objects[i] instanceof Throwable) {
                sb.append(" Exception info: " + ((Throwable) objects[i]).getMessage() + ";Exception: "
                        + ((Throwable) objects[i]).getClass().getName());
                break;
            }
            if (objects[i] != null && objects[i].getClass().isArray()) {
                concat(sb, (Object[]) objects[i]);
            } else {
                sb.append(objects[i]);
            }
        }
    }

    public static AlertClient getAlertClient() {
        if (alertClient == null) {
            alertClient = new LogAlertClient();
        }
        return alertClient;
    }

    /**
     * 
     * @param alertClient
     */
    public void setAlertClient(AlertClient alertClient) {
        AlertUtil.alertClient = alertClient;
    }

}

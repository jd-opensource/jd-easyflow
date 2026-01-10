package com.jd.easyflow.processunit.client;

import java.util.Map;
import java.util.function.Function;

import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitExecuteRes;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.bean.ProcessUnitCreateAndExecuteReq;
import com.jd.easyflow.processunit.client.bean.ProcessUnitCreateAndExecuteRes;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessUnitHelper {

    private static ProcessUnitClient processUnitClient;

    public static <T> T call(String unitCode, String requestContent, Function<ExecContext, T> invoker) {
        return getProcessUnitClient().call(unitCode, requestContent, invoker);
    }

    public static <T> T call(String unitCode, String bizNo, String requestContent, Function<ExecContext, T> invoker) {
        return getProcessUnitClient().call(unitCode, bizNo, requestContent, invoker);
    }

    public static <T> T call(String unitCode, String bizNo, String requestContent, Function<ExecContext, T> invoker,
            Function<ExecContext, String> resultFunction) {
        return getProcessUnitClient().call(unitCode, bizNo, requestContent, invoker, resultFunction);
    }

    public static <T> T call(String unitCode, String bizNo, String requestNo, String requestContent,
            Function<ExecContext, T> invoker, Function<ExecContext, String> resultFunction,
            Function<ExecContext, String> responseContentFunction, Function<ExecContext, ?> oldResultFunction) {
        return getProcessUnitClient().call(unitCode, bizNo, requestNo, requestContent, invoker, resultFunction,
                responseContentFunction, oldResultFunction);
    }


    public static <T> T call(String unitCode, String bizNo, String requestNo, String requestContent, String productCode,
            Function<ExecContext, T> invoker, Function<ExecContext, String> resultFunction,
            Function<ExecContext, String> responseContentFunction, Function<ExecContext, ?> oldResultFunction) {
        return getProcessUnitClient().call(unitCode, bizNo, requestNo, requestContent, productCode, invoker,
                resultFunction, responseContentFunction, oldResultFunction);
    }
    
    public static ExecResult call(ExecParam param) {
        return getProcessUnitClient().call(param);
    }

    public static ProcessUnitCreateRes create(String unitCode, String bizNo) {
        return getProcessUnitClient().create(unitCode, bizNo);
    }

    public static ProcessUnitCreateRes create(String unitCode, String bizNo, String requestContent) {
        return getProcessUnitClient().create(unitCode, bizNo, requestContent);
    }

    public static ProcessUnitCreateRes create(String unitCode, String bizNo, String requestNo, String requestContent) {
        return getProcessUnitClient().create(unitCode, bizNo, requestNo, requestContent);
    }


    public static ProcessUnitCreateRes create(String unitCode, String bizNo, String requestNo, String requestContent,
            String productCode) {
        return getProcessUnitClient().create(unitCode, bizNo, requestNo, requestContent, productCode);
    }
    
    public  static ProcessUnitCreateRes create(ProcessUnitCreateReq req) {
        return getProcessUnitClient().create(req);
    }


    public static void sendCreateMessage(String unitCode, String bizNo, String requestContent) {
        getProcessUnitClient().sendCreateMessage(unitCode, bizNo, requestContent);
    }


    public static void sendCreateMessage(String unitCode, String bizNo, String requestNo, String requestContent) {
        getProcessUnitClient().sendCreateMessage(unitCode, bizNo, requestNo, requestContent);
    }

    public static void sendCreateMessage(String unitCode, String bizNo, String requestNo, String requestContent,
            String productCode) {
        getProcessUnitClient().sendCreateMessage(unitCode, bizNo, requestNo, requestContent, productCode);
    }
    
    public static void update(String unitCode, String bizNo, String result, String responseContent) {
        getProcessUnitClient().update(unitCode, bizNo, result, responseContent);
    }
    
    public static void update(String unitCode, String bizNo, String result, String responseContent, Map<String, String> variables) {
        getProcessUnitClient().update(unitCode, bizNo, result, responseContent, variables);
    }
    
    public static ProcessUnitExecuteRes execute(ProcessUnitExecuteReq req) {
        return getProcessUnitClient().execute(req);
    }
    
    
    
    public static ProcessUnitCreateAndExecuteRes createAndExecuteAfterCommit(ProcessUnitCreateAndExecuteReq req) {
        return getProcessUnitClient().createAndExecuteAfterCommit(req);
    }

    public static ProcessUnitClient getProcessUnitClient() {
        if (processUnitClient == null) {
            processUnitClient = ObjectFactorys.getDefault().getObject(ProcessUnitClient.class);
        }
        return processUnitClient;
    }

    public static void setProcessUnitClient(ProcessUnitClient processUnitClient) {
        ProcessUnitHelper.processUnitClient = processUnitClient;
    }

}

package com.jd.easyflow.common.app;

import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.common.app.dto.AppResponse;
import com.jd.easyflow.common.util.CommonErrorCode;

/**
 * @author liyuliang5
 *
 */
public class AppResponseTransformer {

    public static <T> AppResponse<T> buildResponse4Fail(CommonErrorCode code) {
        return buildResponse(code.getCode(), code.getDesc(), null);
    }

    public static <T> AppResponse<T> buildResponse4Fail(String resCode, String info) {
        return buildResponse(resCode, info, null);
    }

    public static <T> AppResponse<T> buildResponse(String resCode, String info, T resData) {

        AppResponse<T> response = new AppResponse<T>();

        response.setResultMsg(info);
        response.setResultCode(resCode);
        response.setResultData(resData);
        return response;
    }


    public static <T> AppResponse<T> buildResponseNoLogin(String resCode, String info, String loginUrl) {

        AppResponse<T> response = new AppResponse<T>();

        response.setResultMsg(info);
        response.setResultCode(resCode);
        response.setExtData(loginUrl);
        return response;
    }

    public static <T> AppResponse<T> buildResponse4Succ(T resData) {
        return buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(), resData);
    }

    public static boolean isSuccess(AppResponse response) {
        return response != null && CommonErrorCode.E0000000.getCode().equals(response.getResultCode());
    }

    public static AppResponse buildResponseNoLogin(String loginUrl) {
        return buildResponseNoLogin(CommonErrorCode.E0000006.getCode(), CommonErrorCode.E0000006.getDesc(), loginUrl);
    }

    public static AppResponse buildResponse4FieldError(String field, String errDesc) {
        Map<String,String> fieldError = new HashMap<>();
        fieldError.put(field, errDesc);
        return buildResponse4FieldError(fieldError);
    }
    
    public static AppResponse buildResponse4FieldError(Map<String,String> fieldError) {
        AppResponse appResponse = new AppResponse(CommonErrorCode.E0000008.getCode(),CommonErrorCode.E0000008.getDesc());
        appResponse.setExtData(fieldError);
        return appResponse;
    }


}

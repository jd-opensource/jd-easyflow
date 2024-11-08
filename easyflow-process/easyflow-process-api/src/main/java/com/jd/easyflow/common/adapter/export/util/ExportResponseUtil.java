package com.jd.easyflow.common.adapter.export.util;

import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;

/**
 * @author liyuliang5
 *
 */
public class ExportResponseUtil {

    public static <T> T unwrap(ExportResponse<T> response) {
        if (ExportResponseCode.SUCCESS.getCode().equals(response.getResCode())) {
            return response.getData();
        }
        throw new RuntimeException("Response exception, code:" + response.getResCode() + " desc:" + response.getResDesc());
    }
}

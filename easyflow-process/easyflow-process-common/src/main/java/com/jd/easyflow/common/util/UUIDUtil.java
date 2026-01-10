package com.jd.easyflow.common.util;

import java.util.UUID;

/**
 * @author liyuliang5
 *
 */
public class UUIDUtil {


    public static String getUUID() {
        return UUID.randomUUID().toString().trim();
    }

    public static String getSimpleUUID() {
        return UUID.randomUUID().toString().trim().replaceAll("-", "");
    }
}

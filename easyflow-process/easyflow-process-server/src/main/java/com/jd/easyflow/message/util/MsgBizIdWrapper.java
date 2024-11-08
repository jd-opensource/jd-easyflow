package com.jd.easyflow.message.util;

import java.util.Set;

import org.apache.commons.lang3.StringUtils;

/**
 * 
 * @author liyuliang5
 *
 */
public class MsgBizIdWrapper {

    private static MsgBizIdWrapper DEFAULT_INSTANCE = new MsgBizIdWrapper();

    private Set<String> wrapTopics;

    private String bizIdPrefix;

    public static MsgBizIdWrapper getDefaultInstance() {
        return DEFAULT_INSTANCE;
    }

    public String wrap(String bizId, String topic) {
        if (bizId == null) {
            return null;
        }
        if (StringUtils.isNotEmpty(bizIdPrefix) && wrapTopics != null && wrapTopics.contains(topic)) {
            return bizIdPrefix + bizId;
        }
        return bizId;
    }

    public boolean wrapCheck(String bizId, String topic) {
        if (bizId == null) {
            return true;
        }
        if (StringUtils.isNotEmpty(bizIdPrefix) && wrapTopics != null && wrapTopics.contains(topic)) {
            if (!bizId.startsWith(bizIdPrefix)) {
                return false;
            }
        }
        return true;

    }

    public String unwrap(String bizId, String topic) {
        if (bizId == null) {
            return null;
        }
        if (StringUtils.isNotEmpty(bizIdPrefix) && wrapTopics != null && wrapTopics.contains(topic)) {
            if (!bizId.startsWith(bizIdPrefix)) {
                return null;
            }
            return bizId.substring(bizIdPrefix.length());
        }
        return bizId;

    }

    public Set<String> getWrapTopics() {
        return wrapTopics;
    }

    public void setWrapTopics(Set<String> wrapTopics) {
        this.wrapTopics = wrapTopics;
    }

    public String getBizIdPrefix() {
        return bizIdPrefix;
    }

    public void setBizIdPrefix(String bizIdPrefix) {
        this.bizIdPrefix = bizIdPrefix;
    }

}

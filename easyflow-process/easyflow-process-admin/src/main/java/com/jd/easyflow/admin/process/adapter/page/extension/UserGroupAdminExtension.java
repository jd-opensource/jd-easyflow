package com.jd.easyflow.admin.process.adapter.page.extension;

import java.util.List;
import java.util.Map;

/**
 * 
 * @author liyuliang5
 *
 */
public interface UserGroupAdminExtension {

    String getCurrentUser(Map<String, Object> extData);

    List<String> getUserGroupList(String user, Map<String, Object> extData);

    List<String> getUserGroup2List(String user, Map<String, Object> extData);
}

package com.jd.easyflow.integration.all.admin.process;

import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.jd.easyflow.admin.process.adapter.page.extension.UserGroupAdminExtension;
import com.jd.easyflow.flow.util.FlowIOUtil;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 *
 */
@Service
public class SampleUserGroupAdminExtensionImpl implements UserGroupAdminExtension {

    private Map<String, Object> userGroupData;

    @PostConstruct
    public void init() throws Exception {
        String content = FlowIOUtil.toString(this.getClass().getResourceAsStream("/config/sampleUserGroup.json"));
        userGroupData = JSON.parseObject(content, Map.class);
    }

    @Override
    public String getCurrentUser(Map<String, Object> extData) {
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes())
                .getRequest();
        String user = (String) request.getSession().getAttribute("user");
        return user;
    }

    @Override
    public List<String> getUserGroupList(String user, Map<String, Object> extData) {
        if (userGroupData != null) {
            List<Map<String, Object>> users = (List<Map<String, Object>>) userGroupData.get("userList");
            if (users != null) {
                for (Map<String, Object> u : users) {
                    if (user.equals(u.get("user"))) {
                        return (List<String>) u.get("groupList");
                    }
                }
            }
        }
        return null;
    }

    @Override
    public List<String> getUserGroup2List(String user, Map<String, Object> extData) {
        if (userGroupData != null) {
            List<Map<String, Object>> users = (List<Map<String, Object>>) userGroupData.get("userList");
            if (users != null) {
                for (Map<String, Object> u : users) {
                    if (user.equals(u.get("user"))) {
                        return (List<String>) u.get("group2List");
                    }
                }
            }
        }
        return null;
    }
    
}

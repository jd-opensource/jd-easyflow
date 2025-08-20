package com.jd.easyflow.common.adapter.page;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.context.ServletContextAware;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.PagerConditionHelper;

/**
 * @author liyuliang5
 *
 */
public class BasePageController implements ServletContextAware {

    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    public static final int MAX_PAGE_SIZE = 1000;

    private static final String SORT_KEY = "sortName";
    private static final String ORDER_KEY = "sortOrder";
    private static final String PAGE_SIZE_KEY = "pageSize";
    private static final String PAGE_NUMBER_KEY = "pageNumber";

    private ServletContext servletContext;

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        binder.registerCustomEditor(String.class, new StringTrimmerEditor(true));
    }

    @Override
    public void setServletContext(ServletContext servletContext) {
        this.servletContext = servletContext;
    }

    public HttpServletRequest getRequest() {
        return ((ServletRequestAttributes)RequestContextHolder.getRequestAttributes()).getRequest();
    }

    public HttpServletResponse getResponse() {
        return ((ServletRequestAttributes)RequestContextHolder.getRequestAttributes()).getResponse();
    }

    public HttpSession getSession() {
        return getRequest().getSession();
    }

    public ServletContext getServletContext() {
        return servletContext;
    }

    protected PagerCondition getPagerCondition() {
        return fillPagerCondition(new PagerCondition());
    }

    protected PagerCondition fillPagerCondition(PagerCondition condition) {
        return fillPagerCondition(getRequest(), condition, null, null);
    }

    protected PagerCondition fillPagerCondition(HttpServletRequest request, PagerCondition condition) {
        return fillPagerCondition(request, condition, null, null);
    }

    protected PagerCondition fillPagerCondition(PagerCondition condition, String[] conditionFields,
        String[] sortFields) {
        return fillPagerCondition(getRequest(), condition, conditionFields, sortFields);
    }

    protected PagerCondition fillPagerCondition(HttpServletRequest request, PagerCondition condition,
        String[] conditionFields, String[] sortFields) {
        Enumeration<String> names = request.getParameterNames();
        Set<String> conditionFieldSet = null;
        if (conditionFields != null) {
            conditionFieldSet = new HashSet<>();
            for (String s : conditionFields) {
                conditionFieldSet.add(s);
            }
        }

        while (names.hasMoreElements()) {
            String name = names.nextElement();
            if (conditionFieldSet != null && !conditionFieldSet.contains(name)) {
                continue;
            }
            if (condition.getPageIndex() == -1 && PAGE_NUMBER_KEY.equals(name)) {
                condition.setPageIndex(Integer.parseInt(request.getParameter(PAGE_NUMBER_KEY)));
                continue;
            }

            if (condition.getPageSize() == -1 && PAGE_SIZE_KEY.equals(name)) {
                int pageSize = Integer.parseInt(request.getParameter(PAGE_SIZE_KEY));
                if (pageSize > MAX_PAGE_SIZE) {
                    pageSize = MAX_PAGE_SIZE;
                }
                condition.setPageSize(pageSize);
                continue;
            }

            if ("_".equals(name) || ORDER_KEY.equals(name)) {
                continue;
            }
            if (SORT_KEY.equals(name)) {
                String value = request.getParameter(name);
                String order = request.getParameter(ORDER_KEY);
                if ("desc".equals(order) || "asc".equals(order)) {
                    condition.addSortField(value, 1, order);
                }
                continue;

            }
            String[] values = request.getParameterValues(name);
            if (values.length == 1) {
                if (values[0] != null && values[0].length() > 0) {
                    condition.addField(new FieldEntry(name, values[0].trim()));
                }
            } else if (values.length > 1) {
                List<String> valueList = new ArrayList<>();
                for (String val : values) {
                    if (val != null && val.length() > 0) {
                        valueList.add(val.trim());
                    }
                }
                if (valueList.size() > 0) {
                    values = values.length == valueList.size() ? values : valueList.toArray(new String[] {});
                    condition.addField(new FieldEntry(name, values));
                }
            }
        }
        if (condition.getSortList() != null) {
            Collections.sort(condition.getSortList());
            Collections.reverse(condition.getSortList());
        }
        return condition;
    }

    /**
     * 
     * @param condition
     */
    protected void checkPageParams(PagerCondition condition) {
        if (condition.getPageIndex() < 0L) {
            condition.setPageIndex(1L);
        }
        if (condition.getPageSize() < 0) {
            condition.setPageSize(20);
        }
        FieldEntry createdDateEnd = condition.getField("createdDateEnd");
        if (createdDateEnd != null) {
            createdDateEnd.setValue(createdDateEnd.getValue() + " 23:59:59");
        }
        FieldEntry endDate = condition.getField("endDate");
        if (endDate != null) {
            endDate.setValue(endDate.getValue() + " 23:59:59");
        }
        FieldEntry modifiedDateEnd = condition.getField("modifiedDateEnd");
        if (modifiedDateEnd != null) {
            modifiedDateEnd.setValue(modifiedDateEnd.getValue() + " 23:59:59");
        }
        FieldEntry checkDateEnd = condition.getField("checkDateEnd");
        if (checkDateEnd != null) {
            checkDateEnd.setValue(checkDateEnd.getValue() + " 23:59:59");
        }
        PagerConditionHelper.setValueType(condition, "productCodeList", List.class);
    }
    
    protected String getRootPath() {
        return "";
    }

}


package com.jd.easyflow.integration.all.admin.filter;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Service;
import org.springframework.web.filter.GenericFilterBean;

/**
 * @author liyuliang5
 *
 */
@Service
public class SampleAuthFilter extends GenericFilterBean {

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        String user = request.getParameter("user");
        if (user != null) {
            ((HttpServletRequest) request).getSession().setAttribute("user", user);
        }
        user = (String) ((HttpServletRequest) request).getSession().getAttribute("user");
        if (user == null) {
            user = "zhangsan";
            ((HttpServletRequest) request).getSession().setAttribute("user", user);;
        }
        chain.doFilter(request, response);
        
    }

}

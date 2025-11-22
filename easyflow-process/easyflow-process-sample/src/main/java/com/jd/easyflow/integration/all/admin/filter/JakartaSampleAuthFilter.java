package com.jd.easyflow.integration.all.admin.filter;

import java.io.IOException;

import org.springframework.stereotype.Service;

import com.jd.easyflow.integration.all.admin.sample.SampleUserHolder;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

/**
 * @author liyuliang5
 *
 */
@Service
public class JakartaSampleAuthFilter implements Filter {
    
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
        try {
            SampleUserHolder.set(user);
        chain.doFilter(request, response);
        } finally {
            SampleUserHolder.remove();
        }
        
    }

}

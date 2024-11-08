package com.jd.easyflow.spring;

import java.lang.reflect.Field;
import java.util.Map;

import org.springframework.aop.framework.AdvisedSupport;
import org.springframework.aop.framework.AopProxy;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.spel.SpelHelper;

/**
 * @author liyuliang5
 *
 */
public class ContextUtil implements ApplicationContextAware {

    private static volatile ApplicationContext context;

    public static void load(String... locations) {
        context = new ClassPathXmlApplicationContext(locations);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        doSetApplicationContext(applicationContext);
        SpelHelper.setApplicationContext(applicationContext);
    }

    public static ApplicationContext getApplicationContext() {
        return context;
    }

    @SuppressWarnings("unchecked")
    public static <T> T getBean(String name) {
        return (T) context.getBean(name);
    }

    @SuppressWarnings("unchecked")
    public static <T> T getBean(Class clazz) {
        return (T) context.getBean(clazz);
    }

    public static <T> Map<String, T> getBeansOfType(Class<T> clazz) {
        return context.getBeansOfType(clazz);
    }

    private synchronized static void doSetApplicationContext(ApplicationContext applicationContext) {
        context = applicationContext;
    }


    public static Object getProxyTargetObject(Object proxy) {
        if (!AopUtils.isAopProxy(proxy)) {
            return proxy;
        }
        return AopUtils.isJdkDynamicProxy(proxy) ? getJdkDynamicProxyTargetObject(proxy) : getCglibProxyTargetObject(proxy);
    }


    private static Object getCglibProxyTargetObject(Object proxy) {
        try {
            Field h = proxy.getClass().getDeclaredField("CGLIB$CALLBACK_0");
            h.setAccessible(true);
            Object dynamicAdvisedInterceptor = h.get(proxy);
            Field advised = dynamicAdvisedInterceptor.getClass().getDeclaredField("advised");
            advised.setAccessible(true);
            Object target = ((AdvisedSupport) advised.get(dynamicAdvisedInterceptor)).getTargetSource().getTarget();
            return target;
        } catch (Throwable e) { //NOSONAR
            throw new EasyFlowException("CGLIB proxied class get error", e);
        }
    }



    private static Object getJdkDynamicProxyTargetObject(Object proxy) {
        try {
            Field h = proxy.getClass().getSuperclass().getDeclaredField("h");
            h.setAccessible(true);
            AopProxy aopProxy = (AopProxy) h.get(proxy);
            Field advised = aopProxy.getClass().getDeclaredField("advised");
            advised.setAccessible(true);
            Object target = ((AdvisedSupport) advised.get(aopProxy)).getTargetSource().getTarget();
            return target;
        } catch (Throwable e) { //NOSONAR
            throw new EasyFlowException("JDK proxied class get error", e);
        }
    }
    
    public static boolean inited() {
        return context != null;
    }
}


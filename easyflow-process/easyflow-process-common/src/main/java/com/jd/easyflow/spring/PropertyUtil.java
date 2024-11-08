package com.jd.easyflow.spring;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;
import org.springframework.core.env.PropertySources;
import org.springframework.core.env.PropertySourcesPropertyResolver;
import org.springframework.core.env.SystemEnvironmentPropertySource;

/**
 * 
 * @author liyuliang5
 *
 */
public class PropertyUtil {

    public static final Logger logger = LoggerFactory.getLogger(PropertyUtil.class);

    private static Environment env;

    private static PropertySourcesPlaceholderConfigurer configurer;

    private static PropertySourcesPropertyResolver resolver;

    @Autowired
    public void setEnvironment(Environment env) {
        logger.info("set env");
        doSetEnvironment(env);
        printProperties(((ConfigurableEnvironment) env).getPropertySources());
    }

    public void setConfigurer(PropertySourcesPlaceholderConfigurer configurer) {
        logger.info("set configure");
        PropertyUtil.configurer = configurer;
        PropertySources sources = configurer.getAppliedPropertySources();
        PropertyUtil.resolver = new PropertySourcesPropertyResolver(sources);
        printProperties(sources);
    }

    /**
     * 
     * @param propertySources
     */
    private void printProperties(PropertySources propertySources) {
        StringBuilder builder = new StringBuilder("\n config source:");
        // print all
        Set<String> keys = new TreeSet<>();
        for (Iterator it = propertySources.iterator(); it.hasNext();) {
            PropertySource propertySource = (PropertySource) it.next();
            builder.append(propertySource.getName() + "\n");
            if (propertySource instanceof EnumerablePropertySource) {
                if (propertySource instanceof SystemEnvironmentPropertySource
                        || "systemProperties".equals(propertySource.getName())) {
                    continue;
                }
                for (String key : ((EnumerablePropertySource) propertySource).getPropertyNames()) {
                    keys.add(key);
                }
            }
        }
        builder.append("\n config info:\n");
        for (String key : keys) {
            String value = env.getProperty(key);
            builder.append(key + "=" + value + "\n");
        }
        logger.info(builder.toString());
    }

    public static String get(String key) {
        if (resolver != null) {
            return resolver.getProperty(key);
        }
        if (env == null) {
            logger.info("Get env from ContextUtil");
            env = ContextUtil.getBean(Environment.class);
        }
        return env.getProperty(key);
    }

    private synchronized static void doSetEnvironment(Environment env) {
        PropertyUtil.env = env;
    }

    public static boolean inited() {
        return env != null;
    }

}

package com.jd.easyflow.flow.engine.impl;

import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.util.SpelHelper;

/**
 * FlowEngineImpl. Adding spring integration based on CoreFlowEngine.
 * @author liyuliang5
 *
 */
public class FlowEngineImpl extends CoreFlowEngine implements SmartLifecycle {

    public static final Logger logger = LoggerFactory.getLogger(FlowEngineImpl.class);

    @Autowired
    private ApplicationContext applicationContext;

    private  int phase = Integer.MIN_VALUE;
    
    private boolean autoStartup = true;

    private volatile boolean isRunning = false;
    
    public void init() {
        if (inited) {
            return;
        }
        if (applicationContext != null) {
            SpelHelper.setApplicationContext(applicationContext);
        }
        super.init();
        inited = true;
    }

    protected void loadFlow() {
        if (flowPath == null) {
            return;
        }
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        String[] flowPaths = flowPath.split(",");
        for (String path : flowPaths) {
            try {
                resources = resolver.getResources(path.trim());
                for (Resource resource : resources) {
                    if (logger.isInfoEnabled()) {
                        logger.info("Start parsing definition files:" + resource.getURI());
                    }
                    try (InputStream is = resource.getInputStream()) {
                        loadFlowInputStream(is);
                    }
                }
            } catch (IOException e) {
                throw new RuntimeException("Flow definition file parse exception", e);
            }
        }
    }


    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }


    @Override
    public void start() {
        init();
        isRunning = true;
    }

    @Override
    public void stop() {
        isRunning = false;
        destroy();
    }
    
    @Override
    public boolean isAutoStartup() {
        return autoStartup;
    }
    
    @Override
    public void stop(Runnable callback) {
        stop();
        callback.run();
    }

    @Override
    public boolean isRunning() {
        return isRunning;
    }
    
    @Override
    public int getPhase() {
        return phase;
    }

    public void setPhase(int phase) {
        this.phase = phase;
    }

    public void setAutoStartup(boolean autoStartup) {
        this.autoStartup = autoStartup;
    }
    
}

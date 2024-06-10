package com.jd.easyflow.fsm;

import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.fsm.util.SpelHelper;

/**
 * Fsm Manager. Adding spring integration based on CoreFsmManager.
 * @author liyuliang5
 *
 */
public class FsmManager extends CoreFsmManager implements SmartLifecycle, ApplicationContextAware {

    public static final Logger logger = LoggerFactory.getLogger(FsmManager.class);

    private ApplicationContext applicationContext;

    private int phase = Integer.MIN_VALUE;

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

    /**
     * 
     * Loan fsm.
     *
     */
    protected void loadFsm() {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        try {
            resources = resolver.getResources(fsmPath);
            for (Resource resource : resources) {
                logger.info("Start parse fsm definition file:" + resource.getURI());
                try (InputStream is = resource.getInputStream()) {
                   loadFsmInputStream(is);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Fsm definition file load exception", e);
        }
    }

    public ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    @Override
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
    public boolean isRunning() {
        return isRunning;
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

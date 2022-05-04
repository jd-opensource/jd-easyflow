package com.jd.easyflow.flow.bpmn.cases.converter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BpmnTestService {
    
    private static final Logger logger = LoggerFactory.getLogger(BpmnTestService.class);

    public void doScriptTask1() {
        logger.info("Script task1 execute");
    }
    
    public void doScriptTask2() {
        logger.info("Script task2 execute");
    }
}

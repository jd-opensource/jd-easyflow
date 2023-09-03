package com.jd.easyflow.flow.bpmn.ext.cases.chain;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.ext.chain.BaseChainPlugin;
import com.jd.easyflow.flow.model.NodeContext;

public class Plugin2 extends BaseChainPlugin {
    
    private static final Logger logger = LoggerFactory.getLogger(Plugin2.class);
    

    @Override
    public boolean preHandle(NodeContext nodeContext, FlowContext context) {
        logger.info("Plugin2 pre handle");
        return true;
    }

    @Override
    public void postHandleNormal(NodeContext nodeContext, FlowContext context) {
        logger.info("Plugin2 post handle normal");
    }

    @Override
    public void postHandleException(Throwable t, NodeContext nodeContext, FlowContext context) {
        logger.info("Plugin2 post handle exception");
    }

}

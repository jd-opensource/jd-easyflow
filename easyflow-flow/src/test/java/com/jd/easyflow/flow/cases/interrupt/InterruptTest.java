package com.jd.easyflow.flow.cases.interrupt;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowUtil;

/**
 * 
 * @author liyuliang5
 */
public class InterruptTest {
    
    private static final Logger logger = LoggerFactory.getLogger(InterruptTest.class);
    
    
    

    /**
     * Test interrupt.
     * node001 sleep 1000ms return false.
     * node0021 sleep 2000ms return true -> node0022 sleep 1000ms return false.
     * node003 sleep 3000ms return false.
     * expect false.
     * 
     */
    @Test
    public void testInterrupt001() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/interrupt/interrupt_001.json");
        flowEngine.init();
        
        
        FlowParam param = new FlowParam("interrupt_001");
        AtomicInteger successBizCount = new AtomicInteger(0);
        param.putContextData("successBizCount", successBizCount);
        
        FlowResult result = flowEngine.execute(param);
        
        logger.info("successBizCount:" + successBizCount.get());
        logger.info("final result:" + (successBizCount.get() == result.getContext().getFlow().getNodeList().size()));
        
        // sleep to see node002 and node003 log.
        //Thread.sleep(5000);
    }
    
 
    /**
     * Test interrupt.
     * node001 sleep 1000ms return true.
     * node002 sleep 2000ms return true.
     * node003 sleep 3000ms return true.
     * expect true.
     * 
     */
    @Test
    public void testInterrupt002() throws Exception {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/interrupt/interrupt_002.json");
        flowEngine.init();
        
        
        FlowParam param = new FlowParam("interrupt_002");
        AtomicInteger successBizCount = new AtomicInteger(0);
        param.putContextData("successBizCount", successBizCount);
        
        FlowResult result = flowEngine.execute(param);
        
        logger.info("successBizCount:" + successBizCount.get());
        logger.info("final result:" + ((successBizCount.get() == result.getContext().getFlow().getNodeList().size())));
        
    }
    
    

}

class TestInterruptBizNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(TestInterruptBizNodeAction.class);
    
    private long sleepMills;
    
    private boolean result;
    
    public TestInterruptBizNodeAction(long sleepMills, boolean result) {
        this.sleepMills = sleepMills;
        this.result = result;
    }
    
    @Override
    public Boolean execute(NodeContext nodeContext, FlowContext context) {
        try {
            logger.info(FlowUtil.node(nodeContext, context).getId() + " start");
            // biz1 use 1000ms, return result.
            Thread.sleep(sleepMills);
            if (result) {
                ((AtomicInteger) context.get("successBizCount")).incrementAndGet();
            } else {
                context.setInterrupted();
            }
            logger.info(FlowUtil.node(nodeContext, context).getId() + " end");
            return result;
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
    
}






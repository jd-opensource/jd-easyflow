package com.jd.easyflow.flow.ext.cases.interrupt;

import static org.junit.Assert.assertEquals;

import java.util.concurrent.Executors;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.engine.impl.ReusableThreadFlowRunner;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowUtil;

/**
 * @author liyuliang5
 */
public class ExtInterruptTest {
    
    private static final Logger logger = LoggerFactory.getLogger(ExtInterruptTest.class);

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
        flowEngine.setFlowPath("classpath:flow/cases/interrupt/ext_interrupt_001.json");
        flowEngine.init();
        
        
        FlowParam param = new FlowParam("ext_interrupt_001");
        AtomicInteger successBizCount = new AtomicInteger(0);
        param.putContextData("successBizCount", successBizCount);
        
        FlowResult result = flowEngine.execute(param);
        
        logger.info("successBizCount:" + successBizCount.get());
        logger.info("final result:" + (successBizCount.get() == result.getContext().getFlow().getNodeList().size()));
        assertEquals(0, successBizCount.get());
        // sleep to see node002 and node003 log.
        //Thread.sleep(5000);
    }
    
  
}


 class ExtTestReusableThreadRunner extends ReusableThreadFlowRunner {

    public ExtTestReusableThreadRunner() {
        this.executor = new ThreadPoolExecutor(0, Integer.MAX_VALUE, 60L, TimeUnit.SECONDS,
                new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new AbortPolicy());
    }
    
    public ExtTestReusableThreadRunner(long timeout) {
        this.executor = new ThreadPoolExecutor(0, Integer.MAX_VALUE, 60L, TimeUnit.SECONDS,
                new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new AbortPolicy());
        this.timeout = timeout;
    }    

}

 class ExtTestInterruptBizNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(ExtTestInterruptBizNodeAction.class);
    
    private long sleepMills;
    
    private boolean result;
    
    public ExtTestInterruptBizNodeAction(long sleepMills, boolean result) {
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
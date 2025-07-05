package com.jd.easyflow.flow.ext.cases.check;

import java.util.Arrays;

import org.junit.Test;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.ext.check.impl.InfiniteLoopCheckFlowParseListener;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowIOUtil;

/**
 * @author liyuliang5
 */
public class InfiniteLoopCheckTest {

    @Test(expected = FlowException.class)
    public void testInfiniteLoop001() throws Exception {
        FlowParserImpl flowParser = new FlowParserImpl();
        flowParser.setPostListeners(Arrays.asList(new InfiniteLoopCheckFlowParseListener()));
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_infinite_loop_001.json"))).get(0);
        
    }
    
    @Test(expected = FlowException.class)
    public void testInfiniteLoop002() throws Exception {
        FlowParserImpl flowParser = new FlowParserImpl();
        flowParser.setPostListeners(Arrays.asList(new InfiniteLoopCheckFlowParseListener()));
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_infinite_loop_002.json"))).get(0);
        
    }
    
    @Test
    public void testInfiniteLoop003() throws Exception {
        FlowParserImpl flowParser = new FlowParserImpl();
        flowParser.setPostListeners(Arrays.asList(new InfiniteLoopCheckFlowParseListener()));
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_infinite_loop_003.json"))).get(0);
        
    }
}

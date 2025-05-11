package com.jd.easyflow.flow.ext.cases.check;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.ext.check.CheckErrorItem;
import com.jd.easyflow.flow.ext.check.CheckParam;
import com.jd.easyflow.flow.ext.check.CheckResult;
import com.jd.easyflow.flow.ext.check.impl.FlowNodeLinkCheckConfig;
import com.jd.easyflow.flow.ext.check.impl.FlowNodeLinkCheckFlowParseListener;
import com.jd.easyflow.flow.ext.check.impl.FlowNodeLinkChecker;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParser;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.FlowIOUtil;

/**
 * @author liyuliang5
 */
public class FlowNodeLinkCheckTest {

    @Test
    public void testCheckNodeIsolated001() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_isolated_001.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckNodeIsolated(true);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        CheckErrorItem item1 = result.getErrorItemList().get(0);
        assertEquals(FlowNodeLinkChecker.ERROR_TYPE_NODE_ISOLATED, item1.getErrorType());
    }
    
    @Test
    public void testCheckNodeIsolated002() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_isolated_002.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckNodeIsolated(true);
        CheckResult result = checker.check(param);
        assertEquals(0, result.getErrorItemList().size());
    }
    
    @Test
    public void testCheckNotExists() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_notexists_001.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckNextNodesNotExists(true);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        CheckErrorItem item1 = result.getErrorItemList().get(0);
        assertEquals(FlowNodeLinkChecker.ERROR_TYPE_NEXT_NODES_NOT_EXISTS, item1.getErrorType());
    }   
    
    @Test
    public void testCheckPreCheckNodesNotExists() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_preCheckNodesNotExists_001.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckPreCheckNodesNotExists(true);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        CheckErrorItem item1 = result.getErrorItemList().get(0);
        assertEquals(FlowNodeLinkChecker.ERROR_TYPE_PRE_CHECK_NODES_NOT_EXISTS, item1.getErrorType());
    } 
    
    @Test
    public void testCheckNonStartNoPrevious() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_nonStart_001.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckNonStartNodeNoPrevious(true);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        CheckErrorItem item1 = result.getErrorItemList().get(0);
        assertEquals(FlowNodeLinkChecker.ERROR_TYPE_NON_START_NODE_NO_PREVIOUS, item1.getErrorType());
    } 
    
    @Test
    public void testCheckNonEndNoNext() throws Exception {
        FlowParser flowParser = new FlowParserImpl();
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_nonEnd_001.json"))).get(0);
        FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
        CheckParam param = new CheckParam();
        param.setFlow(flow);
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        param.setConfig(config);
        config.setCheckNonEndNodeNoNext(true);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        CheckErrorItem item1 = result.getErrorItemList().get(0);
        assertEquals(FlowNodeLinkChecker.ERROR_TYPE_NON_END_NODE_NO_NEXT, item1.getErrorType());
    } 
    
    @Test(expected = FlowException.class)
    public void testCheckListener() throws Exception {
        FlowParserImpl flowParser = new FlowParserImpl();
        FlowNodeLinkCheckFlowParseListener listener = new FlowNodeLinkCheckFlowParseListener();
        listener.setCheckNodeIsolatedPolicy(FlowNodeLinkCheckFlowParseListener.CHECK_POLICY_EXCEPTION);
        flowParser.setPreListeners(Arrays.asList(listener));
        
        Flow flow = flowParser.parse(FlowIOUtil.toString(this.getClass().getResourceAsStream("/flow/cases/check/check_isolated_001.json"))).get(0);
    } 
    
}

package com.jd.easyflow.fsm.cases.check;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.exception.FsmException;
import com.jd.easyflow.fsm.model.check.CheckParam;
import com.jd.easyflow.fsm.model.check.CheckResult;
import com.jd.easyflow.fsm.model.check.impl.FsmStateLinkCheckConfig;
import com.jd.easyflow.fsm.model.check.impl.FsmStateLinkCheckFsmParseListener;
import com.jd.easyflow.fsm.model.check.impl.FsmStateLinkChecker;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.fsm.util.FsmIOUtil;

/**
 * @author liyuliang5
 */
public class FsmStateLinkCheckTest {

    @Test
    public void testNodeIsolated() throws Exception {
        Fsm fsm = FsmParser.parse(FsmIOUtil.toString(this.getClass().getResourceAsStream("/fsm/cases/check/check_isolated_001.json")));
        
        FsmStateLinkChecker checker = new FsmStateLinkChecker();
        CheckParam param = new CheckParam();
        param.setFsm(fsm);
        FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
        config.setCheckStateIsolated(true);
        param.setConfig(config);
        CheckResult result = checker.check(param);
        assertEquals(2, result.getErrorItemList().size());
        assertEquals(FsmStateLinkChecker.ERROR_TYPE_STATE_ISOLATED, result.getErrorItemList().get(0).getErrorType());
    }
    
    @Test
    public void testNextStateNotExists() throws Exception {
        Fsm fsm = FsmParser.parse(FsmIOUtil.toString(this.getClass().getResourceAsStream("/fsm/cases/check/check_notExists_001.json")));
        
        FsmStateLinkChecker checker = new FsmStateLinkChecker();
        CheckParam param = new CheckParam();
        param.setFsm(fsm);
        FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
        config.setCheckNextStatesNotExists(true);
        param.setConfig(config);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        assertEquals(FsmStateLinkChecker.ERROR_TYPE_NEXT_STATES_NOT_EXISTS, result.getErrorItemList().get(0).getErrorType());
    }
    
    @Test
    public void testNonStartNoPrevious() throws Exception {
        Fsm fsm = FsmParser.parse(FsmIOUtil.toString(this.getClass().getResourceAsStream("/fsm/cases/check/check_nonStartNoPrevious_001.json")));
        
        FsmStateLinkChecker checker = new FsmStateLinkChecker();
        CheckParam param = new CheckParam();
        param.setFsm(fsm);
        FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
        config.setCheckNonStartStateNoPrevious(true);
        param.setConfig(config);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        assertEquals(FsmStateLinkChecker.ERROR_TYPE_NON_START_STATE_NO_PREVIOUS, result.getErrorItemList().get(0).getErrorType());
    }    
    
    @Test
    public void testNonEndStateNoNext() throws Exception {
        Fsm fsm = FsmParser.parse(FsmIOUtil.toString(this.getClass().getResourceAsStream("/fsm/cases/check/check_nonEndNoNext_001.json")));
        
        FsmStateLinkChecker checker = new FsmStateLinkChecker();
        CheckParam param = new CheckParam();
        param.setFsm(fsm);
        FsmStateLinkCheckConfig config = new FsmStateLinkCheckConfig();
        config.setCheckNonEndStateNoNext(true);
        param.setConfig(config);
        CheckResult result = checker.check(param);
        assertEquals(1, result.getErrorItemList().size());
        assertEquals(FsmStateLinkChecker.ERROR_TYPE_NON_END_STATE_NO_NEXT, result.getErrorItemList().get(0).getErrorType());
    } 
    
    @Test(expected = FsmException.class)
    public void testCheckListener() throws Exception {
        FsmStateLinkCheckFsmParseListener listener = new FsmStateLinkCheckFsmParseListener();
        listener.setCheckStateIsolatedPolicy(FsmStateLinkCheckFsmParseListener.CHECK_POLICY_EXCEPTION);
        FsmParser.setPreListeners(Arrays.asList(listener));
        Fsm fsm = FsmParser.parse(FsmIOUtil.toString(this.getClass().getResourceAsStream("/fsm/cases/check/check_isolated_001.json")));
             
    }
    
    
}

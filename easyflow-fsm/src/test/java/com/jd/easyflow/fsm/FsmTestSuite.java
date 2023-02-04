package com.jd.easyflow.fsm;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.fsm.cases.mockbiz.FsmMockBizTest;
import com.jd.easyflow.fsm.cases.parser.FsmParserTest;
import com.jd.easyflow.fsm.quickstart.FsmQuickStartTest;

/**
 * 
 * @author liyuliang5
 *
 */
@RunWith(Suite.class)
@SuiteClasses ({
    FsmMockBizTest.class,
    FsmQuickStartTest.class,
    FsmParserTest.class})
public class FsmTestSuite {

}

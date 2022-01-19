package com.jd.easyflow.fsm;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.fsm.mockbiz.FsmMockBizTest;
import com.jd.easyflow.fsm.quickstart.FsmQuickStartTest;

/**
 * 
 * @author liyuliang5
 *
 */
@RunWith(Suite.class)
@SuiteClasses ({
    FsmMockBizTest.class,
    FsmQuickStartTest.class})
public class FsmTestSuite {

}

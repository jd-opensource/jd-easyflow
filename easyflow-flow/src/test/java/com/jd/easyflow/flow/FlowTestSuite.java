package com.jd.easyflow.flow;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.event.EventFlowTest;
import com.jd.easyflow.flow.flowengine.FlowEngineImplTest;
import com.jd.easyflow.flow.index.FlowIndexTest;
import com.jd.easyflow.flow.mockbiz.MockLoanTest;
import com.jd.easyflow.flow.parser.FlowParserTest;
import com.jd.easyflow.flow.quickstart.QuickStartTest;
import com.jd.easyflow.flow.runner.MultiThreadTest;

/**
 * 
 * @author liyuliang5
 *
 */
@RunWith(Suite.class)
@SuiteClasses ({
    QuickStartTest.class,
    MockLoanTest.class,
    FlowEngineImplTest.class,
    MultiThreadTest.class,
    EventFlowTest.class,
    FlowIndexTest.class,
    FlowParserTest.class
})
public class FlowTestSuite {

}

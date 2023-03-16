package com.jd.easyflow.flow;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.cases.event.EventFlowTest;
import com.jd.easyflow.flow.cases.flowengine.FlowEngineImplTest;
import com.jd.easyflow.flow.cases.mockbiz.MockLoanTest;
import com.jd.easyflow.flow.cases.parallel.ParallelTest;
import com.jd.easyflow.flow.cases.parser.FlowParserTest;
import com.jd.easyflow.flow.cases.posthandler.FlowIndexTest;
import com.jd.easyflow.flow.cases.posthandler.NodePostHandlerTest;
import com.jd.easyflow.flow.cases.posthandler.PostHandlerFilterTest;
import com.jd.easyflow.flow.cases.runner.MultiThreadTest;
import com.jd.easyflow.flow.cases.subflow.SubFlowTest;
import com.jd.easyflow.flow.quickstart.QuickStartTest;

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
    FlowParserTest.class,
    NodePostHandlerTest.class,
    PostHandlerFilterTest.class,
    SubFlowTest.class,
    ParallelTest.class
})
public class FlowTestSuite {

}

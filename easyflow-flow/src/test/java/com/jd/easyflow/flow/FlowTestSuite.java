package com.jd.easyflow.flow;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.cases.action.ActionTest;
import com.jd.easyflow.flow.cases.action.LoopNodeActionTest;
import com.jd.easyflow.flow.cases.action.MultipleActionTest;
import com.jd.easyflow.flow.cases.event.EventFlowTest;
import com.jd.easyflow.flow.cases.filter.FilterTest;
import com.jd.easyflow.flow.cases.flowengine.FlowEngineImplTest;
import com.jd.easyflow.flow.cases.inclusive.InclusiveTest;
import com.jd.easyflow.flow.cases.interrupt.InterruptTest;
import com.jd.easyflow.flow.cases.listener.InterruptFlowListenerTest;
import com.jd.easyflow.flow.cases.logflag.LogFlagTest;
import com.jd.easyflow.flow.cases.mockbiz.MockLoanTest;
import com.jd.easyflow.flow.cases.parallel.ParallelTest;
import com.jd.easyflow.flow.cases.parser.FlowParserTest;
import com.jd.easyflow.flow.cases.performance.PerformanceTest;
import com.jd.easyflow.flow.cases.posthandler.ConditionalPostHandlerTest;
import com.jd.easyflow.flow.cases.posthandler.FlowIndexTest;
import com.jd.easyflow.flow.cases.posthandler.NodePostHandlerTest;
import com.jd.easyflow.flow.cases.posthandler.PostHandlerFilterTest;
import com.jd.easyflow.flow.cases.prehandler.PreHandlerTest;
import com.jd.easyflow.flow.cases.pretty.FlowDefPrettyHelperTest;
import com.jd.easyflow.flow.cases.runner.MultiThreadTest;
import com.jd.easyflow.flow.cases.runner.ReusableThreadTest;
import com.jd.easyflow.flow.cases.spring.SpringFlowTest;
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
    ReusableThreadTest.class,
    EventFlowTest.class,
    FlowIndexTest.class,
    FlowParserTest.class,
    NodePostHandlerTest.class,
    PostHandlerFilterTest.class,
    SubFlowTest.class,
    ParallelTest.class,
    PreHandlerTest.class,
    FlowDefPrettyHelperTest.class,
    LoopNodeActionTest.class,
    ActionTest.class,
    LoopNodeActionTest.class,
    InclusiveTest.class,
    InterruptFlowListenerTest.class,
    FilterTest.class,
    InterruptTest.class,
    LogFlagTest.class,
    PerformanceTest.class,
    ConditionalPostHandlerTest.class,
    PreHandlerTest.class,
    FlowDefPrettyHelperTest.class,
    SpringFlowTest.class,
    MultipleActionTest.class
})
public class FlowTestSuite {

}

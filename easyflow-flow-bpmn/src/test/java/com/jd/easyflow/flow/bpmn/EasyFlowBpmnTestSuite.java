package com.jd.easyflow.flow.bpmn;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.bpmn.cases.callactivity.BpmnCallActivityTest;
import com.jd.easyflow.flow.bpmn.cases.converter.BpmnConverterTest;
import com.jd.easyflow.flow.bpmn.cases.defaultflow.DefaultFlowTest;
import com.jd.easyflow.flow.bpmn.cases.extension.BpmnExtensionTest;
import com.jd.easyflow.flow.bpmn.cases.inclusive.BpmnInclusiveTest;
import com.jd.easyflow.flow.bpmn.cases.logflag.LogFlagTest;
import com.jd.easyflow.flow.bpmn.cases.parallel.BpmnParallelTest;
import com.jd.easyflow.flow.bpmn.cases.subprocess.BpmnSubProcessTest;
import com.jd.easyflow.flow.bpmn.cases.terminate.BpmnTerminateTest;
import com.jd.easyflow.flow.bpmn.cases.throwevent.BpmnThrowEventTest;
import com.jd.easyflow.flow.bpmn.ext.cases.chain.ChainTest;

@RunWith(Suite.class)
@SuiteClasses({
    BpmnConverterTest.class,
    BpmnExtensionTest.class,
    BpmnParallelTest.class,
    BpmnSubProcessTest.class,
    BpmnInclusiveTest.class,
    BpmnExtensionTest.class,
    BpmnCallActivityTest.class,
    BpmnSubProcessTest.class,
    ChainTest.class,
    BpmnThrowEventTest.class,
    BpmnCallActivityTest.class,
    DefaultFlowTest.class,
    LogFlagTest.class,
    BpmnTerminateTest.class
})
public class EasyFlowBpmnTestSuite {

}

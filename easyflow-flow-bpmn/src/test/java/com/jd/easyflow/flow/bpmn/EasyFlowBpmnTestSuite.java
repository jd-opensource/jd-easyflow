package com.jd.easyflow.flow.bpmn;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.bpmn.cases.chain.ChainTest;
import com.jd.easyflow.flow.bpmn.cases.converter.BpmnConverterTest;
import com.jd.easyflow.flow.bpmn.cases.extension.BpmnExtensionTest;

@RunWith(Suite.class)
@SuiteClasses({
    BpmnConverterTest.class,
    BpmnExtensionTest.class,
    ChainTest.class
})
public class EasyFlowBpmnTestSuite {

}

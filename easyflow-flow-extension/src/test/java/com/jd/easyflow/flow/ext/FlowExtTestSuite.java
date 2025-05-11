package com.jd.easyflow.flow.ext;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.jd.easyflow.flow.ext.cases.chain.ChainTest;
import com.jd.easyflow.flow.ext.cases.check.FlowNodeLinkCheckTest;
import com.jd.easyflow.flow.ext.cases.funcall.FunCallTest;
import com.jd.easyflow.flow.ext.cases.session.SessionFlowTest;

/**
 * 
 * @author liyuliang5
 *
 */
@RunWith(Suite.class)
@SuiteClasses ({
    SessionFlowTest.class,
    ChainTest.class,
    FunCallTest.class,
    FlowNodeLinkCheckTest.class
})
public class FlowExtTestSuite {

}

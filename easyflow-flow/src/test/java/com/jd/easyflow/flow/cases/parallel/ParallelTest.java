package com.jd.easyflow.flow.cases.parallel;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class ParallelTest {

    /**
     * test parallel nodes.
     */
    @Test
    public void testParallel001() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/parallel/flow_parallel001.json");
        flowEngine.init();
        FlowParam param = new FlowParam("flow_parallel001");
        FlowResult result = flowEngine.execute(param);
        List<NodeContext> previousNodes = result.getContext().getEndNodes().stream()
                .map(node -> (List<NodeContext>)node.get(FlowConstants.NODECTX_PREVIOUS_NODES)).filter(list -> list != null).findFirst()
                .get();
        assertEquals(2, previousNodes.size());
    }
}

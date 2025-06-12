package com.jd.easyflow.flow.cases.filter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ReflectionUtils;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.filter.impl.BaseFlowEngineFilter;
import com.jd.easyflow.flow.engine.impl.FlowEngineImpl;
import com.jd.easyflow.flow.filter.BaseFilter;
import com.jd.easyflow.flow.filter.Filter;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.filter.FlowFilterManager;
import com.jd.easyflow.flow.model.filter.impl.BaseFlowFilter;
import com.jd.easyflow.flow.model.filter.impl.BaseNodeFilter;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.flow.util.Triple;

/**
 * @author liyuliang5
 * 
 */
public class FilterTest {
    
    @Test
    public void testInnerNodeActionFilter() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/filter/inner_node_action_filter_001.json");
        flowEngine.init();
        // false false
        FlowParam param = new FlowParam("innerNodeActionFilter001");
        FlowResult result = flowEngine.execute(param);
        assertEquals("node002", result.getContext().getEndNodes().get(0).getNodeId());
    }
    

    
    @Test
    public void testInnerFlowEngineFilter() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/filter/inner_flow_engine_filter_001.json");
        TestInnerFlowEngineFilter filter = new TestInnerFlowEngineFilter();
        filter.setOrder(-1);
        flowEngine.setFilters(Arrays.asList(filter));
        flowEngine.init();
        // false false
        FlowParam param = new FlowParam("innerFlowEngineFilter001");
        FlowResult result = flowEngine.execute(param);
        assertEquals(true, param.get("flowEngineFilter"));
    }
    
    @Test
    public void testInnerFlowFilter() {
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/cases/filter/inner_flow_filter_001.json");
        flowEngine.init();
        // false false
        FlowParam param = new FlowParam("innerFlowFilter001");
        FlowResult result = flowEngine.execute(param);
        assertEquals(true, param.get("flowFilter"));
        assertEquals(true, param.get("nodeFilter"));
        assertEquals("node002", result.getContext().getEndNodes().get(0).getNodeId());

    }
    
    @Test
    public void testAddFilter() throws Exception {
        Method method = FlowFilterManager.class.getDeclaredMethod("addFilter", Filter.class, List.class, List.class);
        method.setAccessible(true);
        List<TestEmptyFilter> innerList = new ArrayList<TestEmptyFilter>();
        List<TestEmptyFilter> outerList = new ArrayList<TestEmptyFilter>();
        
        /*0 {} {}*/
        outerList.clear();
        innerList.clear();
        Filter filter = new TestEmptyFilter("", 0);
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 1);
        assertTrue(innerList.size() == 0);
        /*1 {} {}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("", 1);
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 1);
        assertTrue(innerList.size() == 0);
        
        /*-1 {} {}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("", -1);
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 0);
        assertTrue(innerList.size() == 1);
        
        /*0 {} {0}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", 0);
        outerList.add(new TestEmptyFilter("0", 0));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 2);
        assertTrue(innerList.size() == 0);
        assertTrue(outerList.get(1).id.equals("new"));
        
        /*2 {} {1}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", 2);
        outerList.add(new TestEmptyFilter("0", 1));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 2);
        assertTrue(innerList.size() == 0);
        assertTrue(outerList.get(0).id.equals("new"));
        
        /*2 {} {3}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", 2);
        outerList.add(new TestEmptyFilter("0", 3));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 2);
        assertTrue(innerList.size() == 0);
        assertTrue(outerList.get(1).id.equals("new"));
        
        /*-2 {-2} {}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", -2);
        innerList.add(new TestEmptyFilter("0", -2));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 0);
        assertTrue(innerList.size() == 2);
        assertTrue(innerList.get(1).id.equals("new"));
        
        /*-2 {-1} {}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", -2);
        innerList.add(new TestEmptyFilter("0", -1));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 0);
        assertTrue(innerList.size() == 2);
        assertTrue(innerList.get(1).id.equals("new"));
        
        /*-2 {-3} {}*/
        outerList.clear();
        innerList.clear();
        filter = new TestEmptyFilter("new", -2);
        innerList.add(new TestEmptyFilter("0", -3));
        ReflectionUtils.invokeMethod(method, new FlowFilterManager(), filter, innerList, outerList);
        assertTrue(outerList.size() == 0);
        assertTrue(innerList.size() == 2);
        assertTrue(innerList.get(0).id.equals("new"));
        
    }
    
    static class TestEmptyFilter implements Filter {
        
        private int order;
        
        private String id;
        
        public TestEmptyFilter(String id, int order) {
            this.id = id;
            this.order = order;
        }
        
        public int getOrder() {
            return order;
        }

        @Override
        public Object doFilter(Object request, FilterChain chain) {
            return null;
        }
        
    }
    
    public static class TestNodeActionFilter extends BaseFilter<Pair<NodeContext, FlowContext>, Object> {
        
        private static final Logger logger = LoggerFactory.getLogger(TestNodeActionFilter.class);
        
        @Override
        public Object doFilter(Pair<NodeContext, FlowContext> request,
                FilterChain<Pair<NodeContext, FlowContext>, Object> chain) {
            Object result = chain.doFilter(request);
            logger.info("result:{}, nodeActionResult:{}", result, request.getLeft().getActionResult());
            return 1;
        }
        
    }
    
    static class TestInnerFlowEngineFilter extends BaseFlowEngineFilter {
        
        @Override
        public FlowResult doFilter(Pair<FlowParam, FlowEngine> request,
                FilterChain<Pair<FlowParam, FlowEngine>, FlowResult> chain) {
            request.getLeft().put("flowEngineFilter", true);
            return chain.doFilter(request);
        }
    }
    
    public static class TestInnerFlowFilter extends BaseFlowFilter {
        
        public TestInnerFlowFilter(int order) {
            this.order = order;
        }

        @Override
        public FlowResult doFilter(FlowContext request, FilterChain<FlowContext, FlowResult> chain) {
            request.getParam().put("flowFilter", true);;
            return chain.doFilter(request);
        }
        
    }
    
    public static class TestInnerNodeFilter extends BaseNodeFilter {
        
        @Override
        public NodeContext doFilter(Triple<FlowNode, NodeContext, FlowContext> request,
                FilterChain<Triple<FlowNode, NodeContext, FlowContext>, NodeContext> chain) {
            if (request.getLeft().getId().equals("node002")) {
                return chain.doFilter(request);
            }
            
            request.getRight().getParam().put("nodeFilter", true);
            NodeContext nodeContext = chain.doFilter(request);
            NodeContextAccessor.setNextNodeIds(nodeContext, new String[]{"node002"});
            return nodeContext;
        }
        
    }

}

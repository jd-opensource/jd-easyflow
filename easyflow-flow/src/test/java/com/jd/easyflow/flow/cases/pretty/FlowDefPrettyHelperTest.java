package com.jd.easyflow.flow.cases.pretty;

import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.util.FlowIOUtil;
import com.jd.easyflow.flow.util.JsonPrettyHelper;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * flow definition pretty helper
 * 
 * @author liyuliang5
 *
 */
public class FlowDefPrettyHelperTest {

    private static final Logger logger = LoggerFactory.getLogger(FlowDefPrettyHelperTest.class);
    
    @Test
    public void testPrettyFile() throws Exception {
        String origin = FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/flow/cases/pretty/pretty_test.json"));
        String flowPrettyConf =  FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
        logger.info("Pretty conf:" + flowPrettyConf);
        logger.info("Input:" + JsonUtil.toJsonString(JsonUtil.parseObject(flowPrettyConf, Map.class)));
        String prettyStr = JsonPrettyHelper.pretty(origin, flowPrettyConf);
        logger.info("Output:\n" + prettyStr);
    }

    @Test
    public void testPretty1() throws Exception {
        String origin = FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/flow/cases/pretty/pretty_test.json"));
        String flowPrettyConf =  FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
        String prettyStr = JsonPrettyHelper.pretty(origin, flowPrettyConf);
        logger.info("Pretty result:\n" + prettyStr);
        logger.info("Start comparing result:");
        String originJsonStr = JsonUtil.toJsonString(JsonUtil.parseObject(origin, Map.class));
        String prettyJsonStr = JsonUtil.toJsonString(JsonUtil.parseObject(prettyStr, Map.class));
        logger.info("Original result:" + originJsonStr);
        logger.info("Pretty result:" + prettyJsonStr);
        Assert.assertEquals(originJsonStr, prettyJsonStr);
    }
    
    @Test
    public void testPrettyList() throws Exception {
        String origin = FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/flow/cases/pretty/pretty_multiple_test.json"));
        String flowPrettyConf =  FlowIOUtil.toString(FlowDefPrettyHelperTest.class.getResourceAsStream("/pretty/pretty-flow.json"));
        logger.info("Pretty conf:" + flowPrettyConf);
        logger.info("Input:" + JsonUtil.toJsonString(JsonUtil.parseObject(flowPrettyConf, Map.class)));
        String prettyStr = JsonPrettyHelper.pretty(origin, flowPrettyConf);
        logger.info("Output:\n" + prettyStr);
    }
}

package com.jd.easyflow.flow.unit.util;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class JsonTest {

	public static final Logger logger = LoggerFactory.getLogger(JsonTest.class);

	@Test
	public void test2St() {
		logger.info("null:" + JsonUtil.toJsonString(null));
		logger.info("1" + JsonUtil.toJsonString(1));
		logger.info("123:" + JsonUtil.toJsonString("123"));
		logger.info("date:" + JsonUtil.toJsonString(new Date()));
		logger.info("map:" + JsonUtil.toJsonString( new HashMap<>()));
		logger.info("object:" + JsonUtil.toJsonString(new Object()));

	}
	
	@Test
	public void test2Object() {
		logger.info("null:" + JsonUtil.parseObject("null", Object.class));
		logger.info("1" + JsonUtil.parseObject("1", Integer.class));
		logger.info("123:" + JsonUtil.parseObject("123", String.class));
		logger.info("date:" + JsonUtil.parseObject("1591418665721", Date.class));
		logger.info("map:" + JsonUtil.parseObject("{}", Map.class));
		logger.info("object:" + JsonUtil.parseObject("{}", Object.class));
	}
	
	@Test
	public void testJsonIgnore() {
	    FlowParam param = new FlowParam();
	    FlowContextImpl context = new FlowContextImpl();
	    param.setContext(context);
	    context.setParam(param);
	    logger.info(JsonUtil.toJsonString(context));
	    
	}
	
    @Test
    public void testJsonIgnore2() {
        FlowResult result = new FlowResult();
        FlowContextImpl context = new FlowContextImpl();
        result.setContext(context);
        context.setResult(result);
        logger.info(JsonUtil.toJsonString(result));

    }
	
}

package com.jd.easyflow.flow.parser;

import java.io.InputStream;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * 
 * 
 * @author liyuliang5
 * @version 1.0
 * @since 1.0
 */

public class FlowParserTest {

	private static final Logger logger = LoggerFactory.getLogger(FlowParserTest.class);

	@Test
	public void testParseFlow() throws Exception {
		PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
		Resource[] resources;
		InputStream is = null;
		resources = resolver.getResources("classpath:flow/parser/parser_test_001.json");
		for (Resource resource : resources) {
			try {
				logger.info("Start parse flow definition:" + resource.getURI());
				is = resource.getInputStream();
				String flowConfigStr = IOUtils.toString(is);
				List<Flow> flowList = new FlowParserImpl().parse(flowConfigStr);
				logger.info("Parse end，model:" + JsonUtil.toJsonString(flowList));
			} finally {
				if (is != null) {
					IOUtils.closeQuietly(is);
				}
			}
		}
	}
}

package com.jd.easyflow.fsm.cases.parser;

import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.fsm.util.JsonUtil;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmParserTest {
    
    private static final Logger logger = LoggerFactory.getLogger(FsmParserTest.class);

    @Test
    public void testParseFsm() throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        InputStream is = null;
        resources = resolver.getResources("classpath:fsm/cases/parser/fsm_parser_test.json");
        for (Resource resource : resources) {
            try {
                logger.info("Start parse definition file:" + resource.getURI());
                is = resource.getInputStream();
                String fsmConfigStr = IOUtils.toString(is);
                Fsm fsm = FsmParser.parse(fsmConfigStr);
                logger.info("Parse end, model is:" + JsonUtil.toJsonString(fsm));
            } finally {
                if (is != null) {
                    IOUtils.closeQuietly(is);
                }
            }
        }
    }
    
    @Test
    public void testParseCreateExp() throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        Resource[] resources;
        InputStream is = null;
        resources = resolver.getResources("classpath:fsm/cases/parser/fsm_parser_test_create_exp.json");
        for (Resource resource : resources) {
            try {
                logger.info("Start parse definition file:" + resource.getURI());
                is = resource.getInputStream();
                String fsmConfigStr = IOUtils.toString(is);
                Fsm fsm = FsmParser.parse(fsmConfigStr);
                Assert.assertNotNull(fsm.getTransitionList().get(0));
                logger.info("Parse end, model is:" + JsonUtil.toJsonString(fsm));
            } finally {
                if (is != null) {
                    IOUtils.closeQuietly(is);
                }
            }
        }       
    }
}

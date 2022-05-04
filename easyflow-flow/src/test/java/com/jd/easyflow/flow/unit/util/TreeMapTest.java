package com.jd.easyflow.flow.unit.util;

import java.util.Comparator;
import java.util.TreeMap;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class TreeMapTest {
    
    private static final Logger logger = LoggerFactory.getLogger(TreeMapTest.class);

    @Test
    public void testTreeMap1() {
        TreeMap<Integer, String> map = new TreeMap<>();
        map.put(1, "1");
        map.put(0, "0");
        map.put(-1, "-1");
        map.put(2, "2");
        map.put(-2, "-2");
        logger.info("ascending order:");
        for (Integer i : map.keySet()) {
            logger.info(i + "");
        }
        logger.info("reverse order:");
        for (Integer i : map.descendingKeySet()) {
            logger.info(i + "");
        }
    }
    
    @Test
    public void testTreeMap2() {
        TreeMap<Integer, String> map = new TreeMap<>(Comparator.reverseOrder());
        map.put(1, "1");
        map.put(0, "0");
        map.put(-1, "-1");
        map.put(2, "2");
        map.put(-2, "-2");
        logger.info("ascending order:");
        for (Integer i : map.keySet()) {
            logger.info(i + "");
        }
        logger.info("reverse order:");
        for (Integer i : map.descendingKeySet()) {
            logger.info(i + "");
        }
    }
}

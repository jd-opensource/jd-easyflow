package com.jd.easyflow.flow.ext.serialize.impl;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Test;

import com.jd.easyflow.flow.engine.FlowParam;

/**
 * Unit tests for JsonFlowParamSerializer
 * 
 * @author liyuliang5
 */
public class JsonFlowParamSerializerTest {

    private JsonFlowParamSerializer serializer;

    @Before
    public void setUp() {
        serializer = new JsonFlowParamSerializer();
    }

    @Test
    public void testSerializeAndDeserializeBasicFlowParam() {
        // Create basic FlowParam
        FlowParam originalParam = new FlowParam();
        originalParam.setFlowId("test-flow-001");
        originalParam.setNodeIds(new String[]{"node1", "node2"});
        originalParam.setParam("test-param");
        originalParam.setLogFlag(true);
        originalParam.put("key1", "value1");
        originalParam.put("key2", 123);

        // Serialize
        String serializedJson = serializer.serialize(originalParam, null);
        assertNotNull("Serialized result should not be null", serializedJson);
        assertFalse("Serialized result should not be empty", serializedJson.isEmpty());

        // Deserialize
        FlowParam deserializedParam = serializer.deserialize(serializedJson, null);
        
        // Verify deserialized result
        assertNotNull("Deserialized result should not be null", deserializedParam);
        assertEquals("FlowId should be equal", originalParam.getFlowId(), deserializedParam.getFlowId());
        assertArrayEquals("NodeIds should be equal", originalParam.getNodeIds(), deserializedParam.getNodeIds());
        assertEquals("LogFlag should be equal", originalParam.getLogFlag(), deserializedParam.getLogFlag());
        assertEquals("DataMap key1 should be equal", (String) originalParam.get("key1"), deserializedParam.get("key1"));
        assertEquals("DataMap key2 should be equal", (Integer) originalParam.get("key2"), deserializedParam.get("key2"));
    }

}
package com.jd.easyflow.flow.ext.serialize.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Base64;
import java.util.Map;

import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.util.ExceptionUtil;

/**
 * @author liyuliang5
 */
public class JavaFlowParamSerializer extends BaseFlowParamSerializer {

    @Override
    public String serialize(FlowParam flowParam, Map<String, Object> config) {
        try {
            FlowParam newFlowParam = super.createFlowParamToSerialize(flowParam, config);
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            ObjectOutputStream stream = new ObjectOutputStream(byteArrayOutputStream);
            stream.writeObject(newFlowParam);
            String result = Base64.getEncoder().encodeToString(byteArrayOutputStream.toByteArray());
            return result;
        } catch (IOException e) {
            throw ExceptionUtil.throwException(e);
        }
    }

    @Override
    public FlowParam deserialize(String s, Map<String, Object> config) {
        try {
            ObjectInputStream stream = new ObjectInputStream(new ByteArrayInputStream(Base64.getDecoder().decode(s)));
            FlowParam flowParam = (FlowParam) stream.readObject();
            return flowParam;
        } catch (Exception e) {
            throw ExceptionUtil.throwException(e);
        }
    }

}

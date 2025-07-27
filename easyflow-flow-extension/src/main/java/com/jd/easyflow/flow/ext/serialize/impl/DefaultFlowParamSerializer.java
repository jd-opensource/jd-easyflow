package com.jd.easyflow.flow.ext.serialize.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.ext.serialize.FlowParamSerializer;
import com.jd.easyflow.flow.util.ExceptionUtil;
import com.jd.easyflow.flow.util.JsonUtil;

/**
 * @author liyuliang5
 */
public class DefaultFlowParamSerializer implements FlowParamSerializer {

    @Override
    public String serialize(FlowParam flowParam, Map<String, Object> config) {
        try {
            FlowParam newFlowParam = flowParam.getClass().newInstance();
            newFlowParam.setFlowId(flowParam.getFlowId());
            newFlowParam.setLogFlag(flowParam.getLogFlag());
            newFlowParam.setNodeIds(flowParam.getNodeIds());
            newFlowParam.setParam(flowParam.getParam());
            newFlowParam.setDataMapFrom(flowParam);

            if (flowParam.getContext() != null) {
                FlowContext flowContext = flowParam.getContext();
                FlowContext newFlowContext;

                newFlowContext = flowContext.getClass().newInstance();

                newFlowContext.setContext(flowContext.getContext());
                newFlowContext.getData().putAll(flowContext.getData());
                newFlowContext.setLogFlag(flowContext.getLogFlag());
                newFlowParam.setContext(newFlowContext);
            }

            Map<String, Object> map = new HashMap<String, Object>();
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            ObjectOutputStream stream = new ObjectOutputStream(byteArrayOutputStream);
            stream.writeObject(newFlowParam);
            map.put("javaBase64Str", Base64.getEncoder().encodeToString(byteArrayOutputStream.toByteArray()));
            map.put("jsonObj", newFlowParam);
            return JsonUtil.toJsonString(map);
        } catch (InstantiationException | IllegalAccessException | IOException e) {
            throw ExceptionUtil.throwException(e);
        }

    }

    @Override
    public FlowParam deserialize(String s, Map<String, Object> config) {
        try {
            Map<String, Object> map = JsonUtil.parseObject(s, Map.class);
            String javaBase64Str = (String) map.get("javaBase64Str");
            ObjectInputStream stream;
            stream = new ObjectInputStream(new ByteArrayInputStream(Base64.getDecoder().decode(javaBase64Str)));
            FlowParam flowParam = (FlowParam) stream.readObject();
            return flowParam;
        } catch (Exception e) {
            throw ExceptionUtil.throwException(e);
        }
    }

}

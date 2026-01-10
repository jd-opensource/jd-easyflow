package com.jd.easyflow.process.client.flow.asyncnode;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.ext.serialize.FlowParamAssembleData;
import com.jd.easyflow.flow.ext.serialize.FlowParamAssembleManager;
import com.jd.easyflow.flow.ext.serialize.FlowParamSerializeManager;
import com.jd.easyflow.flow.ext.serialize.impl.DefaultFlowParamAssembler;
import com.jd.easyflow.flow.ext.serialize.impl.JsonFlowParamSerializer;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowConstants;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.common.PropertiesUtil;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.flow.util.StdProcessFlowUtil;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateRes;
import com.jd.easyflow.processunit.client.ProcessUnitHelper;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 */
public class CreateAsyncProcessUnitNodeAction implements NodeAction {
    
    private static final String DEFAULT_UNIT_CODE = "EASYFLOW_NODE_ASYNC";
    
    private static final String NEXT_AUTO_RUN_TIME_NOW = "-1";
    
    private String defaultFlowParamSerializerClass = JsonFlowParamSerializer.class.getName();
    
    @Override
    public ProcessUnitCreateRes execute(NodeContext nodeContext, FlowContext context) {
        Map<String, Object> nodeProperties =  context.getFlow().getNode(nodeContext.getNodeId()).getProperties();
        Map<String, Object> nodeAsyncConfig = (Map<String, Object>) nodeProperties.get("async");
        Map<String, Object> flowAsyncConfig = (Map<String, Object>) context.getFlow().getProperty("async");
        
        ProcessUnitCreateReq req = new ProcessUnitCreateReq();
        // unitCode
        String unitCode = PropertiesUtil.get("unitCode", nodeAsyncConfig, flowAsyncConfig);
        if (unitCode == null) {
            unitCode = DEFAULT_UNIT_CODE;
        }
        req.setUnitCode(unitCode);
        req.setAutoRunFlag(true);
        req.setRequestNo(UUID.randomUUID().toString());
        // nextAutoRunTime
        Date nextAutoRunTime = null;
        String nextAutoRunTimeStr = (String) PropertiesUtil.get("nextAutoRuntime", nodeAsyncConfig, flowAsyncConfig);
        if (nextAutoRunTimeStr != null) {
            if (nextAutoRunTimeStr.equals(NEXT_AUTO_RUN_TIME_NOW)) {
                nextAutoRunTime = new Date();
            } else {
                try {
                    nextAutoRunTime = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(nextAutoRunTimeStr);
                } catch (ParseException e) {
                    throw new IllegalArgumentException("nextAutoRunTime illegal " + nextAutoRunTimeStr);
                }
            }
        }
        if (nextAutoRunTime == null) {
            Integer nextAutoRunTimeDelaySeconds = PropertiesUtil.get("nextAutoRunTimeDelaySeconds", nodeAsyncConfig, flowAsyncConfig);
            if (nextAutoRunTimeDelaySeconds != null) {
                nextAutoRunTime = new Date(System.currentTimeMillis() + nextAutoRunTimeDelaySeconds);
            }
        }
        if (nextAutoRunTime == null) {
            String nextAutoRunTimeExp = PropertiesUtil.get("nextAutoRunTimeExp", nodeAsyncConfig, flowAsyncConfig);
            nextAutoRunTime = context.getElEvaluator().eval(nextAutoRunTimeExp, nodeContext, context, null);
        }
        if (nextAutoRunTime == null) {
            throw new IllegalArgumentException("nextAutoRunTime is null");
        }
        // productCode
        ProcessInstanceDTO processInstance = StdProcessFlowUtil.getCachedProcessInstance(context);
        if (processInstance != null) {
            req.setProductCode(processInstance.getProductCode());
        } else {
            req.setProductCode(context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE));
        }
        // bizNo
        String bizNo = null;
        String bizNoExp = PropertiesUtil.get("bizNoExp", nodeAsyncConfig, flowAsyncConfig);
        if (bizNoExp != null) {
            bizNo = context.getElEvaluator().eval(bizNoExp, nodeContext, context, null);
        } else {
           ProcessNodeInstanceDTO processNodeInstance = StdProcessFlowUtil.getCachedNodeInstance(nodeContext, context);
           if (processNodeInstance != null) {
               bizNo = processNodeInstance.getNodeInstanceNo();
           } else {
               bizNo = req.getRequestNo();
           }
        }
        req.setBizNo(bizNo);
        // requestContent.
        Map<String, Object> contentMap = new HashMap<String, Object>();
        if (processInstance != null) {
            contentMap.put("instanceNo", processInstance.getInstanceNo());
            contentMap.put("processType", processInstance.getProcessType());
            contentMap.put("bizNo", processInstance.getBizNo());
        }
        
        contentMap.put("nodeId", nodeContext.getNodeId());
        contentMap.put("flowId", context.getFlowId());
        
        
        
        String flowParamAssemblerClass = PropertiesUtil.get("flowParamAssemblerClass", nodeAsyncConfig, flowAsyncConfig);
        if (flowParamAssemblerClass == null) {
            flowParamAssemblerClass = DefaultFlowParamAssembler.class.getName();
        }
        Map<String, Object> assembleConfig = PropertiesUtil.get("flowParamAssembleConfig", nodeAsyncConfig, flowAsyncConfig);
        FlowParamAssembleData data = new FlowParamAssembleData();
        data.setFlowContext(context);
        data.setNodeContext(nodeContext);
        FlowParam flowParam = FlowParamAssembleManager.getInstance().assemble(flowParamAssemblerClass, data, assembleConfig);
        flowParam.put(FlowConstants.PARAM_DATA_EVENT, "ASYNC");

        
        String flowParamSerializerClass = PropertiesUtil.get("flowParamSerializerClass", nodeAsyncConfig, flowAsyncConfig);
        if (flowParamSerializerClass == null) {
            flowParamSerializerClass = defaultFlowParamSerializerClass;
        }
        Map<String, Object> serializeConfig = PropertiesUtil.get("flowParamSerializeConfig", nodeAsyncConfig, flowAsyncConfig);
        String flowParamStr = FlowParamSerializeManager.getInstance().serialize(flowParamSerializerClass, flowParam, serializeConfig);
        contentMap.put("flowParamStr", flowParamStr);
        contentMap.put("flowParamSerializerClass", flowParamSerializerClass);
        contentMap.put("flowParamSerializeConfig", serializeConfig);
        
        req.setRequestContent(JSON.toJSONString(contentMap));
        req.setParentNo(null);
        req.setVariables(null);
        ProcessUnitCreateRes res = ProcessUnitHelper.create(req);
        return res;
    }

    public String getDefaultFlowParamSerializerClass() {
        return defaultFlowParamSerializerClass;
    }

    public void setDefaultFlowParamSerializerClass(String defaultFlowParamSerializerClass) {
        this.defaultFlowParamSerializerClass = defaultFlowParamSerializerClass;
    }
    

}

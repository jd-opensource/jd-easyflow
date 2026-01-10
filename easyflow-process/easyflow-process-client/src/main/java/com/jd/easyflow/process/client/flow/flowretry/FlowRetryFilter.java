package com.jd.easyflow.process.client.flow.flowretry;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.ext.serialize.FlowParamSerializeManager;
import com.jd.easyflow.flow.ext.serialize.impl.JsonFlowParamSerializer;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.filter.impl.BaseFlowFilter;
import com.jd.easyflow.flow.util.JsonUtil;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.processunit.client.ProcessUnitHelper;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;

/**
 * @author liyuliang5
 */
public class FlowRetryFilter extends BaseFlowFilter {
    
    private String defaultFlowParamSerializerClass = JsonFlowParamSerializer.class.getName();
    
    @Override
    public FlowResult doFilter(FlowContext context, FilterChain<FlowContext, FlowResult> chain) {
        if (Boolean.TRUE.equals(context.getParam().get("_retry"))) {
            return chain.doFilter(context);
        }
        Map<String, Object> requestContent = new HashMap<String, Object>();
        Map<String, Object> retryConfig = context.getFlow().getProperty("retry");
        String flowParamSerializerClass = (String) retryConfig.get("flowParamSerializerClass");
        if (flowParamSerializerClass == null) {
            flowParamSerializerClass = defaultFlowParamSerializerClass;
        }
        Map<String, Object> serializeConfig = (Map<String, Object>) retryConfig.get("flowParamSerializeConfig");
        String flowParamStr = FlowParamSerializeManager.getInstance().serialize(flowParamSerializerClass, context.getParam(), serializeConfig);
        requestContent.put("flowParamStr", flowParamStr);
        requestContent.put("flowParamSerializerClass", flowParamSerializerClass);
        requestContent.put("flowParamSerializeConfig", serializeConfig);
        
        Function invoker = ctx -> {
            return chain.doFilter(context);
        };
        Function<ExecContext, String> resultFunction = ctx -> {
            if (! ctx.isExecuted()) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            if (ctx.getResult().getExecException() != null) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            FlowResult flowResult = (FlowResult) ctx.getResult().getExecResult();
            boolean end = false;
            List<NodeContext> list = flowResult.getContext().getEndNodes();
            for (NodeContext nc : list) {
                if (Boolean.TRUE.equals(flowResult.getContext().getFlow().getNode(nc.getNodeId())
                        .getProperty(StdProcessConstants.PROP_END))) {
                    end = true;
                    break;
                }
            }
            return end ? ProcessUnitConstants.RESULT_SUCCESS : ProcessUnitConstants.RESULT_DOING;
        };

        String unitCode = (String) retryConfig.get("unitCode");
        String bizNo = null;
        String bizNoExp = (String) retryConfig.get("bizNoExp");
        if (bizNoExp != null) {
            bizNo = context.getElEvaluator().eval(bizNoExp, null, context, null);
        } else {
            bizNo = context.get(StdFlowProcessConstants.FLOW_CTX_BIZNO);
        }
        if (bizNo == null) {
            bizNo = UUID.randomUUID().toString();
        }
        String productCode = context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE);
        
        ExecParam param = new ExecParam();
        param.setBizNo(bizNo);
        param.setInvoker(invoker);
        param.setProductCode(productCode);
        param.setRequestContent(JsonUtil.toJsonString(requestContent));
        param.setRequestNo(UUID.randomUUID().toString());
        param.setUnitCode(unitCode);
        param.setResultFunction(resultFunction);
        param.setResponseContentFunction(ctx -> {return null;});
        
        ExecResult result = ProcessUnitHelper.call(param);
        return (FlowResult) result.getExecResult();
    }

    public String getDefaultFlowParamSerializerClass() {
        return defaultFlowParamSerializerClass;
    }

    public void setDefaultFlowParamSerializerClass(String defaultFlowParamSerializerClass) {
        this.defaultFlowParamSerializerClass = defaultFlowParamSerializerClass;
    }
    

}

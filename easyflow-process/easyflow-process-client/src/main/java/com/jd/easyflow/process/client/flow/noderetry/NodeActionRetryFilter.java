package com.jd.easyflow.process.client.flow.noderetry;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeContextAccessor;
import com.jd.easyflow.flow.model.filter.impl.BaseNodeActionFilter;
import com.jd.easyflow.flow.util.FlowUtil;
import com.jd.easyflow.flow.util.JsonUtil;
import com.jd.easyflow.flow.util.Pair;
import com.jd.easyflow.process.client.flow.StdFlowProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.processunit.client.ProcessUnitHelper;
import com.jd.easyflow.processunit.client.bean.ExecContext;
import com.jd.easyflow.processunit.client.bean.ExecParam;
import com.jd.easyflow.processunit.client.bean.ExecResult;
import com.jd.easyflow.processunit.client.util.ProcessUnitConstants;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 */
public class NodeActionRetryFilter extends BaseNodeActionFilter {

    @Override
    public Object doFilter(Pair<NodeContext, FlowContext> request,
                           FilterChain<Pair<NodeContext, FlowContext>, Object> chain) {
        FlowContext context = request.getRight();
        Map<String, Object> flowRetryConfig = context.getFlow().getProperty("nodeRetry");
        Map<String, Object> nodeRetryConfig = FlowUtil.nodeProperty("nodeRetry", request.getLeft(), context);
        
        if (nodeRetryConfig == null || !Boolean.TRUE.equals(nodeRetryConfig.get("skipWhenFinish"))) {
            return chain.doFilter(request);
        }
        Map<String, Object> requestContent = new HashMap<>();
        String processType = context.get(StdProcessConstants.PROP_PROCESS_TYPE);
        String flowBizNo = context.get(StdProcessConstants.PROP_BIZNO);
        String bizNo = processType + "-" + flowBizNo + "-" + request.getLeft().getNodeId();
        Function invoker = ctx -> {
            return chain.doFilter(request);
        };
        Function<ExecContext, String> resultFunction = ctx -> {
            if (!ctx.isExecuted()) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            if (ctx.getResult().getExecException() != null) {
                return ProcessUnitConstants.RESULT_EXCEPTION;
            }
            Object result = ctx.getResult().getExecResult();
            if (result == null) {
                return ProcessUnitConstants.RESULT_DOING;
            }
            return ProcessUnitConstants.RESULT_SUCCESS;
        };
        Function<ExecContext, String> responseContentFunction = ctx -> {
            Map<String, Object> response = new HashMap<>();
            Object result = ctx.getResult().getExecResult();
            response.put("actionResult", result);
            return JSON.toJSONString(response);
        };
        Function<ExecContext, ?> oldResultFunction = ctx -> {
           Map<String, Object> response = JSON.parseObject(ctx.getPolicy().getResponseContent(), Map.class);
           Object actionResult = response.get("actionResult");
           NodeContextAccessor.setActionResult(request.getLeft(), actionResult);
            return actionResult;
        };
        ExecParam param = new ExecParam();
        param.setBizNo(bizNo);
        param.setInvoker(invoker);
        String productCode = context.get(StdFlowProcessConstants.FLOW_CTX_PRODUCT_CODE);
        param.setProductCode(productCode);
        param.setRequestContent(JsonUtil.toJsonString(requestContent));
        param.setRequestNo(UUID.randomUUID().toString());
        String unitCode = (String) flowRetryConfig.get("unitCode");
        param.setUnitCode(unitCode);
        param.setResultFunction(resultFunction);
        param.setResponseContentFunction(responseContentFunction);
        
        ExecResult result = ProcessUnitHelper.call(param);
        return (FlowResult) result.getExecResult();

    }

}

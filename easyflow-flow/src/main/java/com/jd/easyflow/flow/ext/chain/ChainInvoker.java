package com.jd.easyflow.flow.ext.chain;

import java.util.function.Function;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.NodeExecutor;
import com.jd.easyflow.flow.util.ExceptionUtil;
import com.jd.easyflow.flow.util.FlowConstants;

/**
 * 
 * @author liyuliang5
 *
 */
public class ChainInvoker {

    private FlowEngine flowEngine;
    
    public <T, R>R invoke(String chainFlowId, T param, Function<T, R> targetAction) {
        FlowParam flowParam = new FlowParam(chainFlowId, param);
        flowParam.put(FlowConstants.PARAM_ACTION_EXECUTOR, new NodeExecutor<Object>() {
            @Override
            public Object execute(NodeContext nodeContext, FlowContext context) {
                try {
                    return targetAction.apply(context.getParam().getParam());
                } catch (Exception e) {
                    throw ExceptionUtil.throwException(e);
                }
            }
        });
        FlowResult result = flowEngine.execute(flowParam);
        Throwable exception = result.getContext().get(ChainConstants.EXCEPTION);
        if (exception != null) {
            throw ExceptionUtil.throwException(exception);
        }
        return result.getResult();
    }

    public FlowEngine getFlowEngine() {
        return flowEngine;
    }

    public void setFlowEngine(FlowEngine flowEngine) {
        this.flowEngine = flowEngine;
    }
    
    
}

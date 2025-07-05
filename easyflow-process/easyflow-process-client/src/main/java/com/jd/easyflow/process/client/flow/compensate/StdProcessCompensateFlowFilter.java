package com.jd.easyflow.process.client.flow.compensate;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.filter.FilterChain;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.model.action.compensate.CompensateFlowFilter;
import com.jd.easyflow.flow.model.action.compensate.CompensateHelper;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.client.flow.util.StdProcessFlowUtil;
import com.jd.easyflow.process.client.runtime.ProcessRuntimeManager;
import com.jd.easyflow.process.client.runtime.StdProcessConstants;
import com.jd.easyflow.process.client.runtime.StdProcessContext;
import com.jd.easyflow.process.client.util.StdProcessUtil;

/**
 * @author liyuliang5
 */
public class StdProcessCompensateFlowFilter extends CompensateFlowFilter {
    
    @Autowired
    private ProcessRuntimeManager processRuntimeManager;

    public StdProcessCompensateFlowFilter() {
        
    }
    
    public StdProcessCompensateFlowFilter(int order) {
        this.order = order;
    }

    @Override
    public FlowResult doFilter(FlowContext context, FilterChain<FlowContext, FlowResult> chain) {
        StdProcessContext processContext = StdProcessFlowUtil.getStdProcessContext(context);
        if (Boolean.TRUE.toString().equals(processContext.getVariable(StdProcessConstants.PROCESS_VAR_COMPENSATE_FLAG))) {
            CompensateHelper.compensate(context);
        } else if (CompensateHelper.isCompensating(context)) {
            processContext.putVariable(StdProcessConstants.PROCESS_VAR_COMPENSATE_FLAG, Boolean.TRUE.toString());
            List<ProcessNodeInstanceDTO> openNodeInstances = processRuntimeManager.findOpenNodeInstances(processContext);
            List<NodeContext> startNodes = new ArrayList<NodeContext>(openNodeInstances.size());
            for (ProcessNodeInstanceDTO openNodeInstance : openNodeInstances) {
                NodeContext nodeContext = StdProcessCompensateHelper.createCompensateNode(openNodeInstance.getNodeInstanceNo(), processContext, processRuntimeManager);
                startNodes.add(nodeContext);
            }
            context.setStartNodes(startNodes);
            
        }
        FlowResult result = chain.doFilter(context);
        if (CompensateHelper.isCompensating(context)) {
            List<ProcessNodeInstanceDTO> list = processRuntimeManager.findAllNodeInstances(processContext);
            boolean allCompensated = true;
            for (ProcessNodeInstanceDTO nodeInstance : list) {
                boolean compenateNodeFlag = Boolean.TRUE.toString().equals(StdProcessUtil.getNodeInstanceVar(StdProcessConstants.NODE_VAR_COMPENSATE_NODE_FLAG, nodeInstance));
                boolean compensatedFlag = Boolean.TRUE.toString().equals(StdProcessUtil.getNodeInstanceVar(StdProcessConstants.NODE_VAR_COMPENSATED_FLAG, nodeInstance));
                if (! compenateNodeFlag && ! compensatedFlag) {
                    allCompensated = false;
                    break;
                }
            }
            if (allCompensated) {
                processContext.putVariable(StdProcessConstants.PROCESS_VAR_COMPENSATED_FLAG, Boolean.TRUE.toString());
            }
        }
        return result;
    }

    public ProcessRuntimeManager getProcessRuntimeManager() {
        return processRuntimeManager;
    }

    public void setProcessRuntimeManager(ProcessRuntimeManager processRuntimeManager) {
        this.processRuntimeManager = processRuntimeManager;
    }
    
    

}

package com.jd.easyflow.flow.cases.flowengine;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.engine.FlowEngine;
import com.jd.easyflow.flow.engine.FlowParam;
import com.jd.easyflow.flow.engine.FlowResult;
import com.jd.easyflow.flow.engine.impl.FlowContextImpl;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * This is only a POC demo of FlowEngine.
 * @author liyuliang5
 */
public abstract class PocFlowEngineImpl implements FlowEngine {

    protected Map<String, Flow> flowMap = new ConcurrentHashMap<>();

    /**
     * Start flow engine, exeucte flow.
     */
    @Override
    public FlowResult execute(FlowParam param) {
        // init flow context
        FlowContext context = new FlowContextImpl();
        context.setParam(param);
        FlowResult result = new FlowResult();
        context.setResult(result);
        result.setContext(context);
        context.setFlow(flowMap.get(param.getFlowId()));

        // init start nodes
        String[] nodeIds = context.getParam().getNodeIds();
        NodeContext[] nodes = new NodeContext[nodeIds.length];
        for (int i = 0; i < nodeIds.length; i++) {
            nodes[i] = new NodeContext(nodeIds[i]);
        }
        ((FlowContextImpl) context).addNodes(nodes);

        // run
        NodeContext currentNode;
        FlowContextImpl contextImpl = (FlowContextImpl) context;
        // Loop execute.
        while ((currentNode = contextImpl.getNextNode()) != null) {
            FlowNode node = context.getFlow().getNode(currentNode.getNodeId());
            NodeContext[] nextNodes = null;
            // @see PocNodeImpl
            node.execute(currentNode, context);
            // get next nodes
            nextNodes = currentNode.getNextNodes();
            if (nextNodes != null) {
                contextImpl.addNodes(nextNodes);
            }
        }
        
        // return result
        return context.getResult();
    }

}

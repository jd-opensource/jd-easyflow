package com.jd.easyflow.flow.ext.check.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.SmartLifecycle;

import com.jd.easyflow.flow.engine.impl.CoreFlowEngine;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.ext.check.CheckErrorItem;
import com.jd.easyflow.flow.ext.check.CheckParam;
import com.jd.easyflow.flow.ext.check.CheckResult;
import com.jd.easyflow.flow.model.Flow;

/**
 * @author liyuliang5
 */
public class AllFlowNodeLinkChecker implements SmartLifecycle {
    
    private static final Logger logger = LoggerFactory.getLogger(AllFlowNodeLinkChecker.class);
    
    private static final String VERSION_PREFIX = "--V_";


    public static final int CHECK_POLICY_OFF = 0;
    public static final int CHECK_POLICY_WARN = 1;
    public static final int CHECK_POLICY_EXCEPTION = 2;

    private boolean check = true;

    private int checkNodeIsolatedPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNextNodesNotExistsPolicy = CHECK_POLICY_EXCEPTION;
    private int checkPreCheckNodesNotExistsPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonStartNodeNoPreviousPolicy = CHECK_POLICY_EXCEPTION;
    private int checkNonEndNodeNoNextPolicy = CHECK_POLICY_OFF;

    private FlowNodeLinkChecker checker = new FlowNodeLinkChecker();
    
    private Set<String> flowWhitelist;
    

    private  int phase = Integer.MIN_VALUE + 1;
    
    private boolean autoStartup = true;

    private volatile boolean isRunning = false;
    
    private CoreFlowEngine flowEngine;
    
    public void check() {
        List<CheckErrorItem> itemList = doCheck();
        boolean exception = false;
        for (CheckErrorItem item : itemList) {
            logger.warn("Flow check error, type:" + item.getErrorType() + " flowId:" + item.getFlowId() + " nodeId:"
                    + item.getNodeId() + " flow:\n" + item.getFlow().stringify());
            int policy;
            switch (item.getErrorType()) {
            case FlowNodeLinkChecker.ERROR_TYPE_NODE_ISOLATED: {
                policy = checkNodeIsolatedPolicy;
                break;
            }
            case FlowNodeLinkChecker.ERROR_TYPE_NEXT_NODES_NOT_EXISTS: {
                policy = checkNextNodesNotExistsPolicy;
                break;
            }
            case FlowNodeLinkChecker.ERROR_TYPE_PRE_CHECK_NODES_NOT_EXISTS: {
                policy = checkPreCheckNodesNotExistsPolicy;
                break;
            }
            case FlowNodeLinkChecker.ERROR_TYPE_NON_START_NODE_NO_PREVIOUS: {
                policy = checkNonStartNodeNoPreviousPolicy;
                break;
            }
            case FlowNodeLinkChecker.ERROR_TYPE_NON_END_NODE_NO_NEXT: {
                policy = checkNonEndNodeNoNextPolicy;
                break;
            }
            default: {
                policy = CHECK_POLICY_OFF;
            }
            }
            if (!exception && policy == CHECK_POLICY_EXCEPTION) {
                exception = true;
            }
        }
        if (exception) {
            throw new FlowException("Flow check error!");
        }
    }
    
    public List<CheckErrorItem> doCheck() {
        List<Flow> allFlowList = getAllFlowList();
        return doCheck(allFlowList);
    }
    
    public List<CheckErrorItem> doCheck(List<Flow> allFlowList) {
        List<CheckErrorItem> list = new ArrayList<CheckErrorItem>();
        if (! check) {
            logger.info("check flag is false, return.");
            return list;
        }
        logger.info("start do flow check");
        CheckParam param = new CheckParam();
        FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
        config.setCheckNodeIsolated(checkNodeIsolatedPolicy != CHECK_POLICY_OFF);
        config.setCheckNextNodesNotExists(checkNextNodesNotExistsPolicy != CHECK_POLICY_OFF);
        config.setCheckPreCheckNodesNotExists(checkPreCheckNodesNotExistsPolicy != CHECK_POLICY_OFF);
        config.setCheckNonStartNodeNoPrevious(checkNonStartNodeNoPreviousPolicy != CHECK_POLICY_OFF);
        config.setCheckNonEndNodeNoNext(checkNonEndNodeNoNextPolicy != CHECK_POLICY_OFF);
        param.setConfig(config);
        for (Flow flow : allFlowList) {
            if (flowWhitelist != null && flowWhitelist.contains(flow.getId())) {
                continue;
            }
            param.setFlow(flow);
            CheckResult checkResult = checker.check(param);
            if (checkResult.getErrorItemList() != null && checkResult.getErrorItemList().size() > 0) {
                list.addAll(checkResult.getErrorItemList());
            }
        }
        logger.info("end do flow check, flow size:" + allFlowList.size() + ", error size:" + list.size());
        return list;
    }
    
    protected List<Flow> getAllFlowList() {
        List<Flow> list = new ArrayList<Flow>();
        for (Entry<String, Flow> entry : flowEngine.getFlowMap().entrySet()) {
            if (entry.getKey().endsWith(VERSION_PREFIX)) {
                continue;
            }
            list.add(entry.getValue());
        }
        return list;
    }
    

    @Override
    public void start() {
        check();
        isRunning = true;
    }

    @Override
    public void stop() {
        isRunning = false;
    }
    
    @Override
    public boolean isAutoStartup() {
        return autoStartup;
    }
    
    @Override
    public void stop(Runnable callback) {
        stop();
        callback.run();
    }

    @Override
    public boolean isRunning() {
        return isRunning;
    }
    
    @Override
    public int getPhase() {
        return phase;
    }

    public void setPhase(int phase) {
        this.phase = phase;
    }

    public void setAutoStartup(boolean autoStartup) {
        this.autoStartup = autoStartup;
    }


    public boolean isCheck() {
        return check;
    }


    public void setCheck(boolean check) {
        this.check = check;
    }


    public int getCheckNodeIsolatedPolicy() {
        return checkNodeIsolatedPolicy;
    }


    public void setCheckNodeIsolatedPolicy(int checkNodeIsolatedPolicy) {
        this.checkNodeIsolatedPolicy = checkNodeIsolatedPolicy;
    }


    public int getCheckNextNodesNotExistsPolicy() {
        return checkNextNodesNotExistsPolicy;
    }


    public void setCheckNextNodesNotExistsPolicy(int checkNextNodesNotExistsPolicy) {
        this.checkNextNodesNotExistsPolicy = checkNextNodesNotExistsPolicy;
    }


    public int getCheckPreCheckNodesNotExistsPolicy() {
        return checkPreCheckNodesNotExistsPolicy;
    }


    public void setCheckPreCheckNodesNotExistsPolicy(int checkPreCheckNodesNotExistsPolicy) {
        this.checkPreCheckNodesNotExistsPolicy = checkPreCheckNodesNotExistsPolicy;
    }


    public int getCheckNonStartNodeNoPreviousPolicy() {
        return checkNonStartNodeNoPreviousPolicy;
    }


    public void setCheckNonStartNodeNoPreviousPolicy(int checkNonStartNodeNoPreviousPolicy) {
        this.checkNonStartNodeNoPreviousPolicy = checkNonStartNodeNoPreviousPolicy;
    }


    public int getCheckNonEndNodeNoNextPolicy() {
        return checkNonEndNodeNoNextPolicy;
    }


    public void setCheckNonEndNodeNoNextPolicy(int checkNonEndNodeNoNextPolicy) {
        this.checkNonEndNodeNoNextPolicy = checkNonEndNodeNoNextPolicy;
    }


    public FlowNodeLinkChecker getChecker() {
        return checker;
    }


    public void setChecker(FlowNodeLinkChecker checker) {
        this.checker = checker;
    }


    public Set<String> getFlowWhitelist() {
        return flowWhitelist;
    }


    public void setFlowWhitelist(Set<String> flowWhitelist) {
        this.flowWhitelist = flowWhitelist;
    }

    public CoreFlowEngine getFlowEngine() {
        return flowEngine;
    }

    public void setFlowEngine(CoreFlowEngine flowEngine) {
        this.flowEngine = flowEngine;
    }

}

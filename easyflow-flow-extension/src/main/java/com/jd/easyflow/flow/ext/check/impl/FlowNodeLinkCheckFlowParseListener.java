package com.jd.easyflow.flow.ext.check.impl;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.ext.check.CheckErrorItem;
import com.jd.easyflow.flow.ext.check.CheckParam;
import com.jd.easyflow.flow.ext.check.CheckResult;
import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;

/**
 * @author liyuliang5
 */
public class FlowNodeLinkCheckFlowParseListener implements FlowParseEventListener {

    private static final Logger logger = LoggerFactory.getLogger(FlowNodeLinkCheckFlowParseListener.class);

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
    
    private boolean checkOnlyParseEl = false;

    @Override
    public void on(FlowParseEvent event) {
        if (!check) {
            return;
        }
        switch (event.getType()) {
        case FlowParseEventTypes.INIT_FLOW_END: {
            if (checkOnlyParseEl && ! event.isParseEl()) {
                return;
            }
            if (logger.isDebugEnabled()) {
                logger.debug("Start node link check, flow ID:" + event.getFlow().getId());
            }
            if (flowWhitelist != null && flowWhitelist.contains(event.getFlow().getId())) {
                return;
            }
            CheckParam param = new CheckParam();
            param.setFlow(event.getFlow());
            FlowNodeLinkCheckConfig config = new FlowNodeLinkCheckConfig();
            config.setCheckNodeIsolated(checkNodeIsolatedPolicy != CHECK_POLICY_OFF);
            config.setCheckNextNodesNotExists(checkNextNodesNotExistsPolicy != CHECK_POLICY_OFF);
            config.setCheckPreCheckNodesNotExists(checkPreCheckNodesNotExistsPolicy != CHECK_POLICY_OFF);
            config.setCheckNonStartNodeNoPrevious(checkNonStartNodeNoPreviousPolicy != CHECK_POLICY_OFF);
            config.setCheckNonEndNodeNoNext(checkNonEndNodeNoNextPolicy != CHECK_POLICY_OFF);
            param.setConfig(config);
            CheckResult result = checker.check(param);
            for (CheckErrorItem item : result.getErrorItemList()) {
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
                } case FlowNodeLinkChecker.ERROR_TYPE_NON_END_NODE_NO_NEXT: {
                    policy = checkNonEndNodeNoNextPolicy;
                    break;
                }
                default: {
                    policy = CHECK_POLICY_OFF;
                }
                }
                logger.warn("Flow check error, type:" + item.getErrorType() + " flowId:" + item.getFlowId() + " nodeId:"
                        + item.getNodeId() + "flow:\n" + item.getFlow().stringify());
                if (policy == CHECK_POLICY_EXCEPTION) {
                    throw new FlowException("Flow check error, type:" + item.getErrorType() + " flowId:"
                            + item.getFlowId() + " nodeId:" + item.getNodeId());
                }
            }
        }
        }
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

    public Set<String> getFlowWhitelist() {
        return flowWhitelist;
    }

    public void setFlowWhitelist(Set<String> flowWhitelist) {
        this.flowWhitelist = flowWhitelist;
    }

    public FlowNodeLinkChecker getChecker() {
        return checker;
    }

    public void setChecker(FlowNodeLinkChecker checker) {
        this.checker = checker;
    }

    public boolean isCheckOnlyParseEl() {
        return checkOnlyParseEl;
    }

    public void setCheckOnlyParseEl(boolean checkOnlyParseEl) {
        this.checkOnlyParseEl = checkOnlyParseEl;
    }
    
    
    

}

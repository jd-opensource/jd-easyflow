package com.jd.easyflow.fsm.quickstart;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.TransitionAction;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class AStateE1EventTstAction implements TransitionAction {
    
    private static final Logger logger = LoggerFactory.getLogger(AStateE1EventTstAction.class);

    @Override
    public Object execute(TransitionContext transitionContext, FsmContext context) {
        logger.info("Execute TST Action");
        return null;
    }

}

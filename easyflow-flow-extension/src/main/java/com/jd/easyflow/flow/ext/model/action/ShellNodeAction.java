package com.jd.easyflow.flow.ext.model.action;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowIOUtil;
import com.jd.easyflow.flow.util.FlowStringUtil;

public class ShellNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(ShellNodeAction.class);
    
    private List<String> shellCommand;
    
    private String shellCommandExp;
    
    private Integer timeoutSeconds;

    @Override
    public String execute(NodeContext nodeContext, FlowContext context) {
        List<String> command = null;
        if (FlowStringUtil.isNotEmpty(shellCommandExp)) {
            command = context.getElEvaluator().eval(shellCommandExp, nodeContext, context, null);
        } else {
            command = shellCommand;
        }
        // execute shell
        if (context.isLogOn() && logger.isInfoEnabled()) {
            logger.info("Shell command is:" + command);
        }
        
        String result;
        try {
            Process process = Runtime.getRuntime().exec(command.toArray(new String[] {}));
            // wait for result
            process.waitFor(timeoutSeconds, TimeUnit.SECONDS);
            result = FlowIOUtil.toString(process.getInputStream());
        } catch (IOException e) {
            throw new FlowException(e);
        } catch (InterruptedException e) {
            throw new FlowException(e);
        }
        logger.info("shell result:" + result);
        return result;
    }
    
    @Override
    public void init(InitContext initContext, Object parent) {
        FlowNode node = (FlowNode) parent;
        shellCommand = node.getProperty("shellCommand");
        shellCommandExp = node.getProperty("shellCommandExp");
        timeoutSeconds = node.getProperty("shellTimeoutSeconds");
    }

}

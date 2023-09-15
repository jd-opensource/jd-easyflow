package com.jd.easyflow.flow.ext.model.action;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.el.ElFactory;
import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;
import com.jd.easyflow.flow.util.FlowIOUtil;

public class ShellNodeAction implements NodeAction {
    
    private static final Logger logger = LoggerFactory.getLogger(ShellNodeAction.class);
    
    private List<String> shellCommand;
    
    private String shellCommandExp;
    
    private Integer timeoutSeconds;

    @Override
    public String execute(NodeContext nodeContext, FlowContext context) {
        List<String> command = null;
        if (StringUtils.isNotEmpty(shellCommandExp)) {
            command = ElFactory.get().eval(shellCommandExp, nodeContext, context, null);
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
    
    public void init(InitContext initContext, FlowNode node) {
        shellCommand = node.getProperty("shellCommand");
        shellCommandExp = node.getProperty("shellCommandExp");
        timeoutSeconds = node.getProperty("shellTimeoutSeconds");
    }

}

package com.jd.easyflow.flow.ext.model.action;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.exception.FlowException;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.InitContext;
import com.jd.easyflow.flow.model.NodeAction;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class JavaScriptNodeAction implements NodeAction {

    private String javaScript;

    @Override
    public <T> T execute(NodeContext nodeContext, FlowContext context) {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("JavaScript");
        Bindings bindings = new SimpleBindings();
        bindings.put("flowContext", context);
        bindings.put("nodeContext", nodeContext);
        Object result;
        try {
            result = engine.eval(javaScript, bindings);
        } catch (ScriptException e) {
            throw new FlowException(e);
        }
        return (T) result;
    }

    @Override
    public void init(InitContext initContext, Object node) {
        javaScript = ((FlowNode) node).getProperty("javascript");

    }

}

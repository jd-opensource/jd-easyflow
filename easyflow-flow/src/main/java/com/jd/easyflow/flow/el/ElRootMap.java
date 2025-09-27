package com.jd.easyflow.flow.el;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * 
 * @author liyuliang5
 */
public class ElRootMap implements Map<String, Object> {

    private static final String KEY_NODE_CONTEXT = "nodeContext";
    private static final String KEY_ACTION_RESULT = "actionResult";
    private static final String KEY_NODE_BIZ_CONTEXT = "nodeBizContext";
    private static final String KEY_CONTEXT = "context";
    private static final String KEY_BIZ_CONTEXT = "bizContext";
    private static final String KEY_PARAM = "param";
    private static final String KEY_BIZ_PARAM = "bizParam";
    private static final String KEY_PARAM_DATA = "paramData";
    private static final String KEY_RESULT = "result";
    private static final String KEY_BIZ_RESULT = "bizResult";
    private static final String KEY_RESULT_DATA = "resultData";

    NodeContext nodeContext;

    FlowContext context;

    Map<String, Object> data;

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public boolean containsKey(Object key) {
        if (key == null) {
            return data == null ? false : data.containsKey(key);
        }
        if (key.getClass() == String.class) {
            switch ((String) key) {
            case KEY_NODE_CONTEXT:
            case KEY_ACTION_RESULT:
            case KEY_NODE_BIZ_CONTEXT:
            case KEY_CONTEXT:
            case KEY_BIZ_CONTEXT:
            case KEY_PARAM:
            case KEY_BIZ_PARAM:
            case KEY_PARAM_DATA:
            case KEY_RESULT:
            case KEY_BIZ_RESULT:
            case KEY_RESULT_DATA:
                return true;
            default:
                return data == null ? false : data.containsKey(key);
            }
        }
        return false;
    }

    @Override
    public Object get(Object key) {
        if (key == null) {
            return data == null ? null : data.get(key);
        }
        if (key.getClass() == String.class) {
            Object value = null;
            switch ((String) key) {
            case KEY_NODE_CONTEXT:
                value = nodeContext;
                break;
            case KEY_ACTION_RESULT:
                value = nodeContext == null ? null : nodeContext.getActionResult();
                break;
            case KEY_NODE_BIZ_CONTEXT:
                value = nodeContext == null ? null : nodeContext.getNodeContext();
                break;
            case KEY_CONTEXT:
                value = context;
                break;
            case KEY_BIZ_CONTEXT:
                value = context == null ? null : context.getContext();
                break;
            case KEY_PARAM:
                value = context == null ? null : context.getParam();
                break;
            case KEY_BIZ_PARAM:
                value = context == null ? null : (context.getParam() == null ? null : context.getParam().getParam());
                break;
            case KEY_PARAM_DATA:
                value = context == null ? null : (context.getParam() == null ? null : context.getParam().getDataMap());
                break;
            case KEY_RESULT:
                value = context == null ? null : context.getResult();
                break;
            case KEY_BIZ_RESULT:
                value = context == null ? null : (context.getResult() == null ? null : context.getResult().getResult());
                break;
            case KEY_RESULT_DATA:
                value = context == null ? null : (context.getResult() == null ? null : context.getResult().getDataMap());
            default:
                // NOOP
            }
            if (value == null && data != null) {
                value = data.get(key);
            }
            return value;
        }
        return null;
    }

    @Override
    public int size() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Object put(String key, Object value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Object remove(Object key) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        throw new UnsupportedOperationException();

    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();

    }

    @Override
    public Set<String> keySet() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<Object> values() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        throw new UnsupportedOperationException();
    }

}

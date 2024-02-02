package com.jd.easyflow.fsm.el;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.FsmParam;
import com.jd.easyflow.fsm.FsmResult;
import com.jd.easyflow.fsm.model.TransitionContext;

/**
 * 
 * @author liyuliang5
 */
public class ElRootMap implements Map<String, Object> {

    private static final String KEY_CONTEXT = "context";
    private static final String KEY_BIZ_CONTEXT = "bizContext";
    private static final String KEY_PARAM = "param";
    private static final String KEY_BIZ_PARAM = "bizParam";
    private static final String KEY_PARAM_DATA = "paramData";
    private static final String KEY_RESULT = "result";
    private static final String KEY_BIZ_RESULT = "bizResult";
    private static final String KEY_TRANSITION_CONTEXT = "transitionContext";
    private static final String KEY_ACTION_RESULT = "actionResult";
    private static final String KEY_TRANSITION_BIZ_CONTEXT = "transitionBizContext";

    FsmContext context;

    Object bizContext;

    FsmParam param;

    Object paramData;
    
    Object bizParam;

    FsmResult result;

    Object bizResult;

    TransitionContext transitionContext;
    
    Object actionResult;
    
    Object transitionBizContext;

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
        if (key instanceof String) {
            switch ((String) key) {
            case KEY_CONTEXT:
            case KEY_BIZ_CONTEXT:
            case KEY_PARAM:
            case KEY_BIZ_PARAM:
            case KEY_PARAM_DATA:
            case KEY_RESULT:
            case KEY_BIZ_RESULT:
            case KEY_TRANSITION_CONTEXT:
            case KEY_ACTION_RESULT:
            case KEY_TRANSITION_BIZ_CONTEXT:
                return true;
            default:
                return data == null ? null : data.containsKey(key);
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
            case KEY_CONTEXT:
                value = context;
                break;
            case KEY_BIZ_CONTEXT:
                value = bizContext;
                break;
            case KEY_PARAM:
                value = param;
                break;
            case KEY_BIZ_PARAM:
                return bizParam;
            case KEY_PARAM_DATA:
                value = paramData;
                break;
            case KEY_RESULT:
                value = result;
                break;
            case KEY_BIZ_RESULT:
                value = bizResult;
                break;
            case KEY_TRANSITION_CONTEXT:
                value = transitionContext;
                break;
            case KEY_ACTION_RESULT:
                value = actionResult;
                break;
            case KEY_TRANSITION_BIZ_CONTEXT:
                value = transitionBizContext;
                break;
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

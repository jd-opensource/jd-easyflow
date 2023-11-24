package com.jd.easyflow.flow.engine.event;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.InitContext;

/**
 * 
 * @author liyuliang5
 *
 */
public class FlowEventTrigger {
    
    public static final Logger logger = LoggerFactory.getLogger(FlowEventTrigger.class);

    public static final String NULL_KEY = null;

    private List<FlowEventListener> listenerList;

    private Map<String, TreeMap<Integer, List<FlowEventListener>>> listenerMap;
    
    public void init(InitContext initContext, Flow flow) {
        if (listenerList != null) {
            for (FlowEventListener listener : listenerList) {
                listener.init(initContext, flow);
            }
        }
    }
    
    public void destroy () {
        if (listenerList != null) {
            for (FlowEventListener listener : listenerList) {
                listener.destroy();
            }
        }
    }


    public void addListener(FlowEventListener listener) {
        if (listenerList == null) {
            listenerList = new ArrayList();
            listenerMap =  new HashMap<>();
        }
        listenerList.add(listener);
        Pair<String, Integer>[] pairs = listener.getAcceptedEvents();
        if (pairs == null) {
            pairs = new Pair[] { Pair.of(NULL_KEY, 0) };
        }
        for (Pair<String, Integer> pair : pairs) {
            TreeMap<Integer, List<FlowEventListener>> map = listenerMap.get(pair.getLeft());
            if (map == null) {
                map = new TreeMap<Integer, List<FlowEventListener>>(Comparator.reverseOrder());
                listenerMap.put(pair.getLeft(), map);
            }
            List<FlowEventListener> listeners = map.get(pair.getRight());
            if (listeners == null) {
                listeners = new ArrayList();
                map.put(pair.getRight(), listeners);
            }
            listeners.add(listener);
        }
    }

    public void triggerEvent(String eventType, FlowContext context) {
        triggerEvent(eventType, null, context, false);
    }

    public void triggerEvent(String eventType, Object eventData, FlowContext context, boolean catchThrowable) {
        if (listenerList == null || listenerList.size() == 0) {
            return;
        }
        
        FlowEvent event = null;
        
        
        TreeMap<Integer, List<FlowEventListener>> map = listenerMap.get(eventType);
        if (map != null) {
            event = createEvent(eventType, eventData, context);
        }
        // priority>0
        if (map != null) {
            for (Entry<Integer, List<FlowEventListener>> entry : map.entrySet()) {
                if (entry.getKey() <= 0) {
                    break;
                }
                for (FlowEventListener listener : entry.getValue()) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        
        // key exists and priority =0
        if (map != null) {
            List<FlowEventListener> keyZeroListeners = map.get(0);
            if (keyZeroListeners != null) {
                for (FlowEventListener listener : keyZeroListeners) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        // key is null
        TreeMap<Integer, List<FlowEventListener>> nullMap = listenerMap.get(NULL_KEY);
        if (nullMap != null) {
            List<FlowEventListener> nullZeroListeners = nullMap.get(0);
            if (nullZeroListeners != null) {
                if (event == null) {
                    event = createEvent(eventType, eventData, context);
                }
                for (FlowEventListener listener : nullZeroListeners) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        // priority<0
        if (map != null) {
            for (Entry<Integer, List<FlowEventListener>> entry : map.entrySet()) {
                if (entry.getKey() >= 0) {
                    continue;
                }
                for (int i = 0; i < entry.getValue().size(); i++) {
                    onEvent(event, entry.getValue().get(entry.getValue().size() - 1 - i), catchThrowable);
                }
            } 
        }
    }
    
    private FlowEvent createEvent(String eventType, Object eventData, FlowContext context) {
        FlowEvent event = new FlowEvent();
        event.setType(eventType);
        event.setData(eventData);
        event.setContext(context);
        return event;
    }
    
    private void onEvent(FlowEvent event, FlowEventListener listener, boolean catchThrowable) {
        if (!catchThrowable) {
            listener.on(event);
        } else {
            try {
                listener.on(event);
            } catch (Throwable t) { //NOSONAR
                logger.error("OnEvent of " + listener.getClass().getName() + " Exception, " + t.getMessage(), t);
            }
        }
    }

    public List<FlowEventListener> getListenerList() {
        return listenerList;
    }

    public void setListenerList(List<FlowEventListener> listenerList) {
        this.listenerList = listenerList;
    }
    
}

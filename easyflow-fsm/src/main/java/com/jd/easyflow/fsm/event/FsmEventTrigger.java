package com.jd.easyflow.fsm.event;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.FsmContext;
import com.jd.easyflow.fsm.model.InitContext;
import com.jd.easyflow.fsm.util.Pair;

/**
 * 
 * @author liyuliang5
 *
 */
public class FsmEventTrigger {

    public static final Logger logger = LoggerFactory.getLogger(FsmEventTrigger.class);

    public static final String NULL_KEY = null;

    private List<FsmEventListener> listenerList;

    private Map<String, TreeMap<Integer, List<FsmEventListener>>> listenerMap;
    
    public void init(InitContext initContext, Fsm fsm) {
        if (listenerList != null) {
            for (FsmEventListener listener : listenerList) {
                listener.init(initContext, fsm);
            }
        }
    }
    
    public void destroy() {
        if (listenerList != null) {
            for (FsmEventListener listener : listenerList) {
                listener.destroy();
            }            
        }
    }

    public void addListener(FsmEventListener listener) {
        if (listenerList == null) {
            listenerList = new ArrayList();
            listenerMap = new HashMap<>();
        }
        listenerList.add(listener);
        Pair<String, Integer>[] pairs = listener.getAcceptedEvents();
        if (pairs == null) {
            pairs = new Pair[] { Pair.of(NULL_KEY, 0) };
        }
        for (Pair<String, Integer> pair : pairs) {
            TreeMap<Integer, List<FsmEventListener>> map = listenerMap.get(pair.getLeft());
            if (map == null) {
                map = new TreeMap<Integer, List<FsmEventListener>>(Comparator.reverseOrder());
                listenerMap.put(pair.getLeft(), map);
            }
            List<FsmEventListener> listeners = map.get(pair.getRight());
            if (listeners == null) {
                listeners = new ArrayList();
                map.put(pair.getRight(), listeners);
            }
            listeners.add(listener);
        }
    }

    public void triggerEvent(String eventType, FsmContext context) {
        triggerEvent(eventType, null, context, false);
    }

    public void triggerEvent(String eventType, Object eventData, FsmContext context, boolean catchThrowable) {
        if (listenerList == null || listenerList.size() == 0) {
            return;
        }
        
        FsmEvent event = null;
        TreeMap<Integer, List<FsmEventListener>> map = listenerMap.get(eventType);
        if (map != null) {
            event = createEvent(eventType, eventData, context);
        }
        // priority>0
        if (map != null) {
            for (Entry<Integer, List<FsmEventListener>> entry : map.entrySet()) {
                if (entry.getKey() <= 0) {
                    break;
                }
                for (FsmEventListener listener : entry.getValue()) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        
        // key exists and priority=0
        if (map != null) {
            List<FsmEventListener> keyZeroListeners = map.get(0);
            if (keyZeroListeners != null) {
                for (FsmEventListener listener : keyZeroListeners) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        // key null
        TreeMap<Integer, List<FsmEventListener>> nullMap = listenerMap.get(NULL_KEY);
        if (nullMap != null) {
            List<FsmEventListener> nullZeroListeners = nullMap.get(0);
            if (nullZeroListeners != null) {
                if (event == null) {
                    event = createEvent(eventType, eventData, context);
                }
                for (FsmEventListener listener : nullZeroListeners) {
                    onEvent(event, listener, catchThrowable);
                }
            }
        }
        // priority<0
        if (map != null) {
            for (Entry<Integer, List<FsmEventListener>> entry : map.entrySet()) {
                if (entry.getKey() >= 0) {
                    continue;
                }
                for (int i = 0; i < entry.getValue().size(); i++) {
                    onEvent(event, entry.getValue().get(entry.getValue().size() - 1 - i), catchThrowable);
                }
            } 
        }
    }
    
    private FsmEvent createEvent(String eventType, Object eventData, FsmContext context) {
        FsmEvent event = new FsmEvent();
        event.setType(eventType);
        event.setData(eventData);
        if (context != null) {
            event.setContext(context);
        }
        return event;
    }
    
    private void onEvent(FsmEvent event, FsmEventListener listener, boolean catchThrowable) {
        if (!catchThrowable) {
            listener.on(event);
        } else {
            try {
                listener.on(event);
            } catch (Throwable t) { //NOSONAR
                logger.error(listener.getClass().getName() + " Execute exception:" + t.getMessage(), t);
            }
        }
    }

    public List<FsmEventListener> getListenerList() {
        return listenerList;
    }


}

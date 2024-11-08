package com.jd.easyflow.process.client.runtime;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.transaction.TxnCommand;

/**
 * @author liyuliang5
 *
 */
public class ProcessCache {

    private Map<Class, Map<Object, CacheObject<?>>> cacheObjectMap = new ConcurrentHashMap<Class, Map<Object, CacheObject<?>>>();

    private List<TxnCommand> commandList = Collections.synchronizedList(new ArrayList<TxnCommand>());

    public <T> Iterable<T> objects(Class<T> clazz) {
        return new ObjectIterator<T>(clazz);
    }

    public <T> T get(Class<T> clazz, Object id) {
        Map<Object, CacheObject<?>> objectMap = cacheObjectMap.get(clazz);
        if (objectMap == null) {
            return null;
        }
        CacheObject<T> cacheObject = (CacheObject<T>) objectMap.get(id);
        if (cacheObject == null) {
            return null;
        }
        return cacheObject.getObject();
    }
    
    public <T>CacheObject<T> getCacheObject(Class<T> clazz, Object id) {
        Map<Object, CacheObject<?>> objectMap = cacheObjectMap.get(clazz);
        if (objectMap == null) {
            return null;
        }
        CacheObject<T> cacheObject = (CacheObject<T>) objectMap.get(id);
        return cacheObject;
    }

    public void put(Object id, Object o, boolean dirty) {
        Map<Object, CacheObject<?>> objectMap = cacheObjectMap.get(o.getClass());
        if (objectMap == null) {
            objectMap = Collections.synchronizedMap(new LinkedHashMap<>());
            cacheObjectMap.put(o.getClass(), objectMap);
        }
        CacheObject cacheObject = (CacheObject) objectMap.get(id);
        if (cacheObject == null) {
            cacheObject = new CacheObject<>();
            cacheObject.setId(id);
            cacheObject.setObject(o);
            cacheObject.setDirty(dirty);
            if (dirty) {
                cacheObject.setPersistOp(ProcessTransactionConstants.PERSIST_OP_ADD);
            } else {
                cacheObject.setPersistOp(ProcessTransactionConstants.PERSIST_OP_NONE);
            }
            objectMap.put(id, cacheObject);
        } else {
            cacheObject.setObject(o);
            cacheObject.setDirty(dirty);
            if (dirty) {
                if (ProcessTransactionConstants.PERSIST_OP_NONE == cacheObject.getPersistOp()) {
                    cacheObject.setPersistOp(ProcessTransactionConstants.PERSIST_OP_UPDATE);
                }
            } else {
                cacheObject.setPersistOp(ProcessTransactionConstants.PERSIST_OP_NONE);
            }
        }
    }

    public void addTxnCommand(TxnCommand command) {
        commandList.add(command);
    }

    public void addTxnCommand(int index, TxnCommand command) {
        commandList.add(index, command);
    }

    public void clearTxnCommand() {
        commandList.clear();
    }

    class ObjectIterator<T> implements Iterator<T>, Iterable<T> {
        Map<Object, CacheObject<?>> objectMap;
        Iterator<Entry<Object, CacheObject<?>>> entryIterator;

        public ObjectIterator(Class clazz) {
            objectMap = cacheObjectMap.get(clazz);
            if (objectMap != null) {
                entryIterator = objectMap.entrySet().iterator();
            }
        }

        @Override
        public boolean hasNext() {
            return entryIterator != null && entryIterator.hasNext();
        }

        @Override
        public T next() {
            return (T) entryIterator.next().getValue().getObject();
        }

        @Override
        public Iterator<T> iterator() {
            return this;
        }
    }

    public Map<Class, Map<Object, CacheObject<?>>> getCacheObjectMap() {
        return cacheObjectMap;
    }

    public void setCacheObjectMap(Map<Class, Map<Object, CacheObject<?>>> cacheObjectMap) {
        this.cacheObjectMap = cacheObjectMap;
    }

    public List<TxnCommand> getCommandList() {
        return commandList;
    }

    public void setCommandList(List<TxnCommand> commandList) {
        this.commandList = commandList;
    }

}

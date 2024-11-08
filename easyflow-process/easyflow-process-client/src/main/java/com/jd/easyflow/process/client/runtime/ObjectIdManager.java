package com.jd.easyflow.process.client.runtime;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessTransactionExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessTransactionConstants;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdReq;
import com.jd.easyflow.process.adapter.export.dto.transaction.BatchObjectIdRes;

/**
 * 
 * @author liyuliang5
 *
 */
public class ObjectIdManager {
    
    private static final Logger logger = LoggerFactory.getLogger(ObjectIdManager.class);

    public static final ObjectIdManager INSTANCE = new ObjectIdManager();

    private Map<String, Integer> cacheSizeMap = new HashMap<>();

    private Map<String, ObjectIdCache> objectIdCacheMap = new ConcurrentHashMap<>();

    private ProcessTransactionExport processTransactionExport;

    {
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_PROCESS, 20);
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_NODE, 50);
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_EXECUTION, 50);
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_TASK, 50);
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_TASK_ASSIGN, 50);
        cacheSizeMap.put(ProcessTransactionConstants.TYPE_TASK_EVENT, 50);

        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_PROCESS, new ObjectIdCache());
        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_NODE, new ObjectIdCache());
        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_EXECUTION, new ObjectIdCache());
        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_TASK, new ObjectIdCache());
        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_TASK_ASSIGN, new ObjectIdCache());
        objectIdCacheMap.put(ProcessTransactionConstants.TYPE_TASK_EVENT, new ObjectIdCache());

    }

    public String nextObjectId(String type) {
        if (logger.isDebugEnabled()) {
            logger.debug("Get ID, type:" + type);
        }
        ObjectIdCache cache = objectIdCacheMap.get(type);
        String id = cache.getNext();
        if (id != null) {
            if (logger.isDebugEnabled()) {
                logger.debug("Return ID:" + id);
            }
            return id;
        }
        BatchObjectIdReq req = new BatchObjectIdReq();
        req.setType(type);
        req.setNum(cacheSizeMap.get(type));
        ExportResponse<BatchObjectIdRes> response = getProcessTransactionExport()
                .batchNextObjectId(new ExportRequest(req));
        BatchObjectIdRes res = ExportResponseUtil.unwrap(response);
        id = cache.addIdsAndGetNext(res.getIds());
        if (logger.isDebugEnabled()) {
            logger.debug("Return ID:" + id);
        }
        return id;
    }

    private ProcessTransactionExport getProcessTransactionExport() {
        if (processTransactionExport != null) {
            return processTransactionExport;
        }
        processTransactionExport = ObjectFactorys.getDefault().getObject(ProcessTransactionExport.class);
        return processTransactionExport;
    }

    public Map<String, Integer> getCacheSizeMap() {
        return cacheSizeMap;
    }

    public void setCacheSizeMap(Map<String, Integer> cacheSizeMap) {
        this.cacheSizeMap = cacheSizeMap;
    }

    private static class ObjectIdCache {

        LinkedList<String> cacheList = new LinkedList<>();

        public synchronized String getNext() {
            return cacheList.poll();
        }

        public synchronized String addIdsAndGetNext(String[] ids) {
            cacheList.addAll(Arrays.asList(ids));
            return cacheList.pop();
        }

    }
}

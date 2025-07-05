package com.jd.easyflow.flow.util;

import com.jd.easyflow.flow.engine.FlowContext;
import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
public class LockUtil {
    
    public static Object getFlowContextLock(String lockKey, FlowContext context) {
        Object lockObj = context.get(lockKey);
        if (lockObj != null) {
            return lockObj;
        }
        synchronized (context) {
            lockObj = context.get(lockKey);
            if (lockObj == null) {
                lockObj = new Object();
                context.put(lockKey, lockObj);
            }
            return lockObj;
        }
    }
    
    public static Object getNodeContextLock(String lockKey, NodeContext nodeContext) {
        Object lockObj = nodeContext.get(lockKey);
        if (lockObj != null) {
            return lockObj;
        }
        synchronized (nodeContext) {
            lockObj = nodeContext.get(lockKey);
            if (lockObj == null) {
                lockObj = new Object();
                nodeContext.put(lockKey, lockObj);
            }
            return lockObj;
        }
    }

}

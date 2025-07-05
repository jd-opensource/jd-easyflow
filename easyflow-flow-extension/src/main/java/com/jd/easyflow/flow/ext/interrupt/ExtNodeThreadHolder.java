package com.jd.easyflow.flow.ext.interrupt;

import com.jd.easyflow.flow.model.NodeContext;

/**
 * @author liyuliang5
 */
class ExtNodeThreadHolder {

    volatile Thread executionThread;
    Object lock = new Object();
    volatile boolean complete = false;
    volatile NodeContext nodeContext;
}

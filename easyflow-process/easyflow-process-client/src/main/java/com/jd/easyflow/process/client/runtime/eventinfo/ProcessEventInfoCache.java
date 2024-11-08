package com.jd.easyflow.process.client.runtime.eventinfo;

import java.util.List;
import java.util.Vector;

/**
 * 
 * @author liyuliang5
 *
 */
public class ProcessEventInfoCache {

    private List<ProcessInstanceEventInfo> flushedProcessInstanceEventInfoList = new Vector<>();

    private List<ProcessInstanceEventInfo> unflushedProcessInstanceEventInfoList = new Vector<>();

    private List<NodeInstanceEventInfo> flushedNodeInstanceEventInfoList = new Vector<>();

    private List<NodeInstanceEventInfo> unflushedNodeInstanceEventInfoList = new Vector<>();

    public void addUnflushProcessInstanceEventInfo(ProcessInstanceEventInfo event) {
        unflushedProcessInstanceEventInfoList.add(event);
    }

    public void addUnflushNodeInstanceEventInfo(NodeInstanceEventInfo event) {
        unflushedNodeInstanceEventInfoList.add(event);
    }

    public void eventFlush() {
        flushedProcessInstanceEventInfoList.addAll(unflushedProcessInstanceEventInfoList);
        unflushedProcessInstanceEventInfoList.clear();
        flushedNodeInstanceEventInfoList.addAll(unflushedNodeInstanceEventInfoList);
        unflushedNodeInstanceEventInfoList.clear();
    }

    public List<ProcessInstanceEventInfo> getFlushedProcessInstanceEventInfoList() {
        return flushedProcessInstanceEventInfoList;
    }

    public List<ProcessInstanceEventInfo> getUnflushedProcessInstanceEventInfoList() {
        return unflushedProcessInstanceEventInfoList;
    }

    public List<NodeInstanceEventInfo> getFlushedNodeInstanceEventInfoList() {
        return flushedNodeInstanceEventInfoList;
    }

    public List<NodeInstanceEventInfo> getUnflushedNodeInstanceEventInfoList() {
        return unflushedNodeInstanceEventInfoList;
    }

}

package com.jd.easyflow.process.domain.constant;

/**
 * @author liyuliang5
 *
 */
public class ProcessTaskConstants {

    public static final String ASSIGN_TYPE_USER = "USER";
    
    public static final String ASSIGN_TYPE_GROUP = "GROUP";
    
    public static final String ASSIGN_STATUS_PENDING = "PENDING";
    
    public static final String ASSIGN_STATUS_FINISH = "FINISH";
    
    public static final String TASK_STATUS_PENDING = "PENDING";
    
    public static final String TASK_STATUS_INVALID = "INVALID";
    
    public static final String TASK_STATUS_FINISH = "FINISH";

    public static final String TASK_STATUS_CANCELED = "CANCELED";

    public static final String PARAM_TASK_TASKNO = "task.taskNo";
    public static final String PARAM_TASK_GROUP_LIST = "task.groupList";
    public static final String PARAM_TASK_GROUP2_LIST = "task.group2List";
    
    public static final String PARAM_TASK_EXECUTE_RESULT = "task.executeBizResult";
    
    public static final String PARAM_TASK_EXECUTE_DATA = "task.executeBizData";
    
    public static final String PARAM_TASK_INSTANCE_BIZ_DATA = "task.instanceBizData";
    
    public static final String PARAM_TASK_INSTANCE_BIZ_STATUS = "task.instanceBizStatus";
    
    public static final String PARAM_TASK_EXECUTE_CMDLIST_STR = "task.executeCmdListStr";
    
    public static final String PARAM_TASK_OPERATION = "task.operation";
    public static final String PARAM_TASK_EXT_DATA = "task.extData";
    
    
    /*EventType*/
    public static final String TASK_EVENT_CREATE = "CREATE";
    
    public static final String TASK_EVENT_EXECUTE = "EXECUTE";
    
    public static final String TASK_EVENT_INVALID_FOR_WITHDRAW = "INVALID_FOR_WITHDRAW";
    
    public static final String TASK_EVENT_WITHDRAW = "WITHDRAW";

    public static final String TASK_EVENT_CANCELED = "CANCELED";


    public static final String NODE_PROP_TASK_KEY = "task";

    public static final String TASK_PROP_WITHDRAW = "withdraw";
    public static final String TASK_PROP_WITHDRAW_ENABLE = "enable";
    
    public static final String COMMAND_TYPE_CREATE_TASK = "createTask";
    
    public static final String COMMAND_TYPE_CREATE_MULTIPLE_TASK = "createMultipleTask";
    
    public static final String COMMAND_TYPE_EXECUTE_TASK = "executeTask";
    
    public static final String TOPIC_TASK_STATUS = "${easyflow.task.taskStatusTopic:easyflow_task_taskStatus}";

    
}

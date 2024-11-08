package com.jd.easyflow.admin.process.adapter.page;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.jd.easyflow.admin.process.adapter.page.converter.PagerConverter;
import com.jd.easyflow.admin.process.adapter.page.converter.ProcessTaskConverter;
import com.jd.easyflow.admin.process.adapter.page.dto.CommonTaskExecuteDTO;
import com.jd.easyflow.admin.process.adapter.page.dto.ProcessTaskInfoForPagerDTO;
import com.jd.easyflow.admin.process.adapter.page.extension.UserGroupAdminExtension;
import com.jd.easyflow.admin.process.adapter.page.util.AdminProcessConstants;
import com.jd.easyflow.admin.process.adapter.page.util.ClientErrorCode;
import com.jd.easyflow.admin.process.adapter.page.util.ProcessPageUtils;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.dto.ExportResponseCode;
import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.common.adapter.page.BasePageController;
import com.jd.easyflow.common.adapter.page.WebResponse;
import com.jd.easyflow.common.dto.DataResponse;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.exception.UserException;
import com.jd.easyflow.common.util.CommonErrorCode;
import com.jd.easyflow.el.ElFactory;
import com.jd.easyflow.flow.bpmn.BpmnFlowParser;
import com.jd.easyflow.flow.model.Flow;
import com.jd.easyflow.flow.model.FlowNode;
import com.jd.easyflow.flow.model.parser.FlowParser;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessDefinitionExport;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExport;
import com.jd.easyflow.process.adapter.export.ProcessTaskExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.CanWithdrawTaskRes;
import com.jd.easyflow.process.adapter.export.dto.task.ExecuteTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskAssignDTO;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.WithdrawTaskReq;
import com.jd.easyflow.process.adapter.export.dto.task.cmd.TaskCreateCmd;
import com.jd.easyflow.spring.MessageUtil;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 */
@Controller
@RequestMapping("easyflow/processTask")
public class ProcessTaskController extends BasePageController {

    private ProcessTaskExport processTaskExport;
    private ProcessInstanceExport processInstanceExport;
    private ProcessDefinitionExport processDefinitionExport;

    @Autowired
    private UserGroupAdminExtension userGroupAdminExtension;

    private FlowParser flowParser = new BpmnFlowParser();
    
    private String elType;

    @RequestMapping("list")
    public String processTaskList() {
        return "easyflow/process/processtask/processTaskList";
    }
    
    @RequestMapping("ajax/getListData")
    @ResponseBody
    public WebResponse<PagerResult> getData() {
        PagerCondition condition = getPagerCondition();
        return getData(condition);
    }

    protected WebResponse<PagerResult> getData(PagerCondition condition) {
        Map extData = new HashMap<>(1);
        extData.put("productCode", null == condition.getField("productCode") ? null : condition.getField("productCode").getValue());
        String currentUser = userGroupAdminExtension.getCurrentUser(extData);
        List<String> groupList = userGroupAdminExtension.getUserGroupList(currentUser, extData);
        List<String> group2List = userGroupAdminExtension.getUserGroup2List(currentUser, extData);

        String queryType = (String) condition.getField("queryType").getValue();
        if ("MY_TODO".equals(queryType)) {
            condition.addField(new FieldEntry("assignUser", currentUser));
            if (groupList != null) {
                condition.addField(new FieldEntry("assignGroupList", groupList));
            }
            if (group2List != null) {
                condition.addField(new FieldEntry("assignGroup2List", group2List));
            }
            condition.addField(new FieldEntry("status", "PENDING"));
        } else if ("MY_DONE".equals(queryType)) {
            condition.addField(new FieldEntry("executor", currentUser));
        }
        condition.putExtData(ProcessTaskConstants.PAGER_EXT_KEY_ASSIGN, ProcessTaskConstants.PAGER_EXT_ASSIGN_PENDING);
        condition.putExtData(ProcessTaskConstants.PAGER_EXT_KEY_NODE_TASK_CONF,
                ProcessTaskConstants.PAGER_EXT_NODE_TASK_CONF_PENDING);
        condition.putExtData(ProcessTaskConstants.PAGER_EXT_KEY_PROCESS_INSTANCE,
                ProcessTaskConstants.PAGER_EXT_PROCESS_INSTANCE_ALL);
        if (condition.getSortList() == null || condition.getSortList().isEmpty()) {
            condition.addSortField("createdDate", 0, "desc");
        }
        ExportResponse response = getProcessTaskExport()
                .pagerQueryTask(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = (com.jd.easyflow.common.adapter.export.dto.pager.PagerResult) ExportResponseUtil
                .unwrap(response);
        List<ProcessTaskInfoForPagerDTO> processTaskList = ProcessTaskConverter.INSTANCE
                .convertPagerResult(pagerResult.getList());
        List<String> collectInstanceNos = processTaskList.stream().map(processTask -> {
            if (ProcessTaskConstants.TASK_STATUS_PENDING.equals(processTask.getStatus())) {
                return processTask.getProcessInstanceNo();
            }
            return null;
        }).filter(t -> StringUtils.isNoneBlank(t)).collect(Collectors.toList());
        Map<String, Object> taskHandleMap = queryTaskAdminPropertyByProcessInstanceNo(collectInstanceNos, "taskHandle");
        for (int i = 0; i < processTaskList.size(); i++) {
            ProcessTaskInfoForPagerDTO dto = processTaskList.get(i);
            ProcessInstanceDTO instance = ((ProcessTaskDTO) pagerResult.getList().get(i)).getProcessInstance();
            dto.setInstanceName(instance.getInstanceName());
            dto.setInstanceStatus(instance.getStatus());
            dto.setInstanceCreator(instance.getCreator());
            dto.setInstanceCreatedDate(instance.getCreatedDate());
            dto.setInstanceKeyField(instance.getKeyField());
            dto.setInstanceKeyField2(instance.getKeyField2());
            List<ProcessTaskAssignDTO> assignList = dto.getAssignList();
            
            boolean canHandle = false;
            if (null != taskHandleMap && Boolean.TRUE.equals(taskHandleMap.get(instance.getInstanceNo()))) {
                if (assignList != null) {
                    String assignInfo = dto.getAssignInfo();
                    Map<String, Object> assignInfoMap = JSON.parseObject(assignInfo, Map.class);
                    List<String> excludeUserList = assignInfoMap == null ? null : (List<String>)assignInfoMap.get("excludeUser");
                    if (excludeUserList != null && excludeUserList.contains(currentUser)) {
                        canHandle = false;
                    } else {
                    for (ProcessTaskAssignDTO assign : assignList) {
                        if (assign.getAssignType().equals(ProcessTaskConstants.ASSIGN_TYPE_USER)) {
                            if (currentUser.equals(assign.getAssignUser())) {
                                canHandle = true;
                                break;
                            }
                        } else if (assign.getAssignType().equals(ProcessTaskConstants.ASSIGN_TYPE_GROUP)) {
                            boolean assignInGroup = null == assign.getAssignGroup2()
                                    && ObjectUtils.isNotEmpty(groupList) && groupList.contains(assign.getAssignGroup());
                            boolean assignInGroup2 = null == assign.getAssignGroup()
                                    && ObjectUtils.isNotEmpty(group2List)
                                    && group2List.contains(assign.getAssignGroup2());
                            boolean assignInAllGroup = (ObjectUtils.isNotEmpty(groupList)
                                    && ObjectUtils.isNotEmpty(group2List) && groupList.contains(assign.getAssignGroup())
                                    && group2List.contains(assign.getAssignGroup2()));
                            if (assignInGroup || assignInGroup2 || assignInAllGroup) {
                                canHandle = true;
                                break;
                            }
                        }
                    }
                    }
                }
            }
            dto.setCanHandle(canHandle);
            if ((!ProcessInstanceConstants.STATUS_CLOSE.equals(instance.getStatus()))
                    && (ProcessTaskConstants.TASK_STATUS_FINISH.equals(dto.getStatus()))
                    && dto.getExecutor().equals(currentUser)) {
                CanWithdrawTaskReq req = new CanWithdrawTaskReq();
                req.setUser(currentUser);
                req.setTaskNo(dto.getTaskNo());
                ExportResponse<CanWithdrawTaskRes> canWithdrawResponse = getProcessTaskExport()
                        .canWithdraw(new ExportRequest<CanWithdrawTaskReq>(req));
                dto.setCanWithdraw(ExportResponseUtil.unwrap(canWithdrawResponse).isCanWithDraw());
            }
        }
        pagerResult.setList(processTaskList);
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                pagerResult);
    }

    @RequestMapping("handle")
    public String handleTaskPage(String taskNo, Model model) {
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        ExportResponse<ProcessTaskDTO> response = getProcessTaskExport().getTask(new ExportRequest<String>(taskNo));
        ProcessTaskDTO task = ExportResponseUtil.unwrap(response);
        ExportResponse<ProcessInstanceDTO> instanceResponse = getProcessInstanceExport()
                .getProcessInstance(new ExportRequest<String>(task.getProcessInstanceNo()));
        ProcessInstanceDTO instance = ExportResponseUtil.unwrap(instanceResponse);
        ExportResponse<ProcessDefinitionDTO> definitionResponse = getProcessDefinitionExport()
                .getProcessDefinition(new ExportRequest<String>(instance.getProcessDefId()));
        ProcessDefinitionDTO definition = ExportResponseUtil.unwrap(definitionResponse);
        Flow flow = flowParser.parse(definition.getJsonContent(), false).get(0);
        FlowNode flowNode = flow.getNode(task.getTaskBizCode());
        Map<String, Object> taskProperties = (Map<String, Object>) flowNode.getProperty("task");
        Map<String, Object> executeProperties = (Map<String, Object>) taskProperties.get("execute");
        Map<String, Object> flowTaskProperties = flow.getProperty("task");
        Map<String, Object> processProperties = flow.getProperty("process");
        Map<String, Object> flowExecuteProperties = (Map<String, Object>) flowTaskProperties.get("taskExecute");
        Map<String, Object> createTaskProperties = (Map<String, Object>) executeProperties.get("createTask");
        if (createTaskProperties != null && Boolean.TRUE.equals(createTaskProperties.get("enable"))) {
            model.addAttribute("createTask", true);
        } else {
            model.addAttribute("createTask", false);
        }
        String pageUrl = executeProperties == null ? null : (String) executeProperties.get("pageUrl");
        if (pageUrl != null) {
            Map<String, Object> root = new HashMap<String, Object>();
            root.put("taskNo", taskNo);
            root.put("flow", flow);
            root.put("processProperties", processProperties);
            root.put("taskProperties", taskProperties);
            String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
            if (StringUtils.isNotEmpty(pageUrlVal)) {
                return "redirect:" + pageUrlVal;
            }
        }
        
        String pageId = executeProperties == null ? null : (String) executeProperties.get("pageId");
        if (pageId == null) {
            pageUrl = flowExecuteProperties == null ? null : (String) flowExecuteProperties.get("pageUrl");
            if (pageUrl != null) {
                Map<String, Object> root = new HashMap<String, Object>();
                root.put("taskNo", taskNo);
                root.put("flow", flow);
                root.put("processProperties", processProperties);
                root.put("taskProperties", taskProperties);
                String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
                if (StringUtils.isNotEmpty(pageUrlVal)) {
                    return "redirect:" + pageUrlVal;
                }
            }
            pageId = flowExecuteProperties == null ? null : (String) flowExecuteProperties.get("pageId");
        }
        if (pageId == null) {
            pageId = "commonProcessTaskPassReject";
        }
        String instanceFormId = (String) executeProperties.get("instanceFormId");
        if (instanceFormId == null) {
            instanceFormId = flowExecuteProperties == null ? null : (String) flowExecuteProperties.get("instanceFormId");
        }
        if (instanceFormId == null) {
            instanceFormId = (String) processProperties.get("formId");
        }
        model.addAttribute("instanceFormId", instanceFormId);
        String taskFormId = (String) executeProperties.get("formId");
        if (taskFormId == null) {
            taskFormId = flowExecuteProperties == null ? null : (String) flowExecuteProperties.get("formId");
        }
        model.addAttribute("taskFormId", taskFormId);
        boolean instanceDataModify = getTaskAdminProperty(flow.getProperties(), "instanceDataModify", false);
        if (instanceDataModify) {
            instanceDataModify = getTaskAdminProperty(flowNode.getProperties(), "instanceDataModify", false);
        }
        model.addAttribute("instanceDataModify", instanceDataModify);
        model.addAttribute("task", task);
        model.addAttribute("instance", instance);
        model.addAttribute("definition", definition);
        model.addAttribute("executeProperties", executeProperties);
        
        String executeBizResult = null;
        String executeBizData = null;
        String instanceBizData = instance.getBizData();
        String extDataStr = task.getExtData();
        Integer taskVersion = null;
        if (StringUtils.isNotEmpty(extDataStr)) {
            Map<String, Object> extData = JSON.parseObject(extDataStr, Map.class);
            taskVersion = (Integer) extData.get("taskVersion");
            
            Map<String, Object> taskTmpSaveData = (Map<String, Object>) extData.get("taskTmpSaveData");
            if (taskTmpSaveData != null) {
                executeBizResult = (String) taskTmpSaveData.get("executeBizResult");
                executeBizData = (String) taskTmpSaveData.get("executeBizData");
                instanceBizData = (String) taskTmpSaveData.get("instanceBizData");
                }
            Map<String, Object> userTmpSaveData = (Map<String, Object>) extData.get("userTmpSaveData");
            if (userTmpSaveData != null && userTmpSaveData.get(currentUser) != null) {
                Map<String, Object> currentuserTmpSaveData = (Map<String, Object>) userTmpSaveData.get(currentUser);
                executeBizResult = (String) currentuserTmpSaveData.get("executeBizResult");
                executeBizData = (String) currentuserTmpSaveData.get("executeBizData");
                instanceBizData = (String) currentuserTmpSaveData.get("instanceBizData");
            }
        }

        Map<String, Object> pageData = new HashMap<>();
        pageData.put("taskNo", task.getTaskNo());
        pageData.put("version", taskVersion == null ? 0 : taskVersion);
        pageData.put("instanceBizData", JSON.parseObject(instanceBizData, Map.class));
        pageData.put("executeBizResult", executeBizResult);
        pageData.put("executeBizData",  JSON.parseObject(executeBizData, Map.class));
        model.addAttribute("pageData", JSON.toJSONString(pageData));

        return "easyflow/process/processtask/" + pageId;
    }

    @RequestMapping("ajax/commonTaskExecute")
    @ResponseBody
    public DataResponse<Object> commonTaskExecute(CommonTaskExecuteDTO executeDto) {
        ExportResponse<ProcessTaskDTO> taskExportRes = getProcessTaskExport().getTask(new ExportRequest<String>(executeDto.getTaskNo()));
        ProcessTaskDTO processTaskDTO = ExportResponseUtil.unwrap(taskExportRes);
        String processInstanceNo = processTaskDTO.getProcessInstanceNo();
        Map<String, Object> taskHandleMap = queryTaskAdminPropertyByProcessInstanceNo(Arrays.asList(processInstanceNo), "taskHandle");
        if (null != taskHandleMap && Boolean.FALSE.equals(taskHandleMap.get(processInstanceNo))) {
            throw new UserException(MessageUtil.getMessage("easyflow.process.admin.tip.handleDisable"));
        }
        Map<String, Object> extData = new HashMap<>();

        ExportResponse<ProcessTaskDTO> taskResponse = getProcessTaskExport().getTask(new ExportRequest<String>(executeDto.getTaskNo()));
        ProcessTaskDTO task = ExportResponseUtil.unwrap(taskResponse);
        Integer currentVersion = null;
        if (StringUtils.isNotEmpty(task.getExtData())) {
            Map<String, Object> taskExtData = JSON.parseObject(task.getExtData(), Map.class);
            currentVersion = (Integer) taskExtData.get("taskVersion");
        }
        currentVersion = currentVersion == null ? 0 : currentVersion;
        if (executeDto.getVersion() != null && ! executeDto.getVersion().equals(currentVersion)) {
            return new DataResponse<>(CommonErrorCode.E9999999.getCode(), MessageUtil.getMessage("easyflow.process.admin.tip.flowChange"), null);
        }
        extData.put("taskVersion", currentVersion + 1);
        
        ExecuteTaskReq req = new ExecuteTaskReq();
        req.setTaskNo(executeDto.getTaskNo());
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        req.setUser(currentUser);
        req.setGroupList(userGroupAdminExtension.getUserGroupList(currentUser, null));
        req.setGroup2List(userGroupAdminExtension.getUserGroup2List(currentUser, null));
        req.setOperation(executeDto.getOperation());     
        if (executeDto.getOperation() == null
                || ProcessTaskConstants.TASK_OP_EXECUTE.equals(executeDto.getOperation())) {
            req.setExecuteBizResult(executeDto.getExecuteBizResult());
            req.setInstanceBizStatus(executeDto.getExecuteBizResult());
            req.setExecuteBizData(executeDto.getExecuteBizData());
            req.setInstanceBizData(
                    StringUtils.isNotEmpty(executeDto.getInstanceBizData()) ? executeDto.getInstanceBizData() : null);
            extData.put("taskTmpSaveData", new HashMap<>());
            extData.put("userTmpSaveData", new HashMap<>());
            req.setTaskExtData(JSON.toJSONString(extData));
            if (StringUtils.isNotBlank(executeDto.getAssignUserList())) {
                TaskCreateCmd cmd = new TaskCreateCmd();
                Map<String, Object> assignInfo = new HashMap<>();
                assignInfo.put("user", Arrays.asList(executeDto.getAssignUserList().split(",")));
                cmd.setAssignInfo(assignInfo);
                req.setCmdList(Arrays.asList(cmd));
            }
        } else if (ProcessTaskConstants.TASK_OP_SAVE.equals(executeDto.getOperation())) {
            ExportResponse<ProcessInstanceDTO> instanceResponse = getProcessInstanceExport()
                    .getProcessInstance(new ExportRequest<String>(processTaskDTO.getProcessInstanceNo()));
            ProcessInstanceDTO instance = ExportResponseUtil.unwrap(instanceResponse);
            ExportResponse<ProcessDefinitionDTO> definitionResponse = getProcessDefinitionExport()
                    .getProcessDefinition(new ExportRequest<String>(instance.getProcessDefId()));
            ProcessDefinitionDTO definition = ExportResponseUtil.unwrap(definitionResponse);
            Flow flow = flowParser.parse(definition.getJsonContent(), false).get(0);
            FlowNode flowNode = flow.getNode(processTaskDTO.getTaskBizCode());
            Map<String, Object> taskProperties = (Map<String, Object>) flowNode.getProperty("task");
            Map<String, Object> adminProperties = taskProperties == null ? null : (Map<String, Object>) taskProperties.get("admin");
            String saveScope = adminProperties == null ? "USER" : (String) adminProperties.get("saveScope");
            
            Map<String, Object> tmpSaveData = new HashMap<>();
            tmpSaveData.put("executeBizResult", executeDto.getExecuteBizResult());
            tmpSaveData.put("executeBizData", executeDto.getExecuteBizData());
            if (StringUtils.isNotEmpty(executeDto.getInstanceBizData())) {
                tmpSaveData.put("instanceBizData", executeDto.getInstanceBizData());
            }
            if ("TASK".equals(saveScope)) {
                extData.put("taskTmpSaveData", tmpSaveData);
            } else {
                String extDataStr = processTaskDTO.getExtData();
                Map<String, Object> taskExtData = JSON.parseObject(extDataStr, Map.class);
                Map<String, Object> userTmpSaveData = null;
                if (taskExtData != null) {
                    userTmpSaveData = (Map<String, Object>) taskExtData.get("userTmpSaveData");
                }
                if (userTmpSaveData == null) {
                    userTmpSaveData = new HashMap<>();
                }
                userTmpSaveData.put(currentUser, tmpSaveData);
                extData.put("userTmpSaveData", userTmpSaveData);
            }
            req.setTaskExtData(JSON.toJSONString(extData));
            req.setExecuteBizResult(null);
            req.setExecuteBizData(null);
            req.setInstanceBizStatus(null);
            req.setInstanceBizData(null);
        } else {
            throw new UnsupportedOperationException("Unsupproted operation:" + executeDto.getOperation());
        }
        
        
        ExportResponse response = getProcessTaskExport().executeTask(new ExportRequest(req));
        if (ExportResponseCode.SUCCESS.getCode().equals(response.getResCode())) {
            return DataResponse.buildSuccessResponse(null);
        }
        if (ClientErrorCode.PTC_0101.name().equals(response.getResCode())) {
            Map<String, String> errorData = response.getExt() == null ? null
                    : (Map<String, String>) response.getExt().get("errorData");
            if (errorData != null) {
                return new DataResponse<Object>(errorData.get(AdminProcessConstants.BIZ_RES_CODE),
                        errorData.get(AdminProcessConstants.BIZ_RES_DESC), null);
            }
        }
        return new DataResponse<>(CommonErrorCode.E9999999.getCode(), CommonErrorCode.E9999999.getDesc(), null);
    }

    @RequestMapping("ajax/withdraw")
    @ResponseBody
    public DataResponse<Object> withdraw(String taskNo) {
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        WithdrawTaskReq req = new WithdrawTaskReq();
        req.setTaskNo(taskNo);
        req.setUser(currentUser);
        ExportResponseUtil.unwrap(getProcessTaskExport().withDraw(new ExportRequest<WithdrawTaskReq>(req)));
        return DataResponse.buildSuccessResponse(null);
    }

    @RequestMapping("detail")
    public String detailPage(String taskNo, Model model) {
        ExportResponse<ProcessTaskDTO> response = getProcessTaskExport().getTask(new ExportRequest<String>(taskNo));
        ProcessTaskDTO task = ExportResponseUtil.unwrap(response);
        ExportResponse<ProcessInstanceDTO> instanceResponse = getProcessInstanceExport()
                .getProcessInstance(new ExportRequest<String>(task.getProcessInstanceNo()));
        ProcessInstanceDTO instance = ExportResponseUtil.unwrap(instanceResponse);
        ExportResponse<ProcessDefinitionDTO> definitionResponse = getProcessDefinitionExport()
                .getProcessDefinition(new ExportRequest<String>(instance.getProcessDefId()));
        ProcessDefinitionDTO definition = ExportResponseUtil.unwrap(definitionResponse);
        Flow flow = flowParser.parse(definition.getJsonContent(), false).get(0);
        FlowNode flowNode = flow.getNode(task.getTaskBizCode());

        Map<String, Object> taskProperties = (Map<String, Object>) flowNode.getProperty("task");
        Map<String, Object> detailProperties = (Map<String, Object>) taskProperties.get("detail");
        Map<String, Object> flowTaskProperties = flow.getProperty("task");
        Map<String, Object> processProperties = flow.getProperty("process");
        Map<String, Object> flowDetailProperties = flowTaskProperties == null ? null : (Map<String, Object>) flowTaskProperties.get("taskDetail");
        model.addAttribute("task", task);
        model.addAttribute("instance", instance);
        model.addAttribute("definition", definition);
        String instanceBizData = instance.getBizData();
        String executeBizResult = null;
        String executeBizData = null;
        
        if (ProcessTaskConstants.TASK_STATUS_PENDING.equals(task.getStatus())) {
            String assignInfo = "";
            if (task.getAssignList() != null) {
                for (ProcessTaskAssignDTO assign : task.getAssignList()) {
                    if (ProcessTaskConstants.ASSIGN_TYPE_USER.equals(assign.getAssignType())) {
                        assignInfo += MessageUtil.getMessage("easyflow.process.admin.user") + ":" + assign.getAssignUser() + ";";
                    } else {
                        assignInfo += (assign.getAssignGroup() == null ? "" : (MessageUtil.getMessage("easyflow.process.admin.userGroup") + assign.getAssignGroup()))
                                + (assign.getAssignGroup2() == null ? " " : (MessageUtil.getMessage("easyflow.process.admin.userGrooup2") + assign.getAssignGroup2())) + ";";
                    }
                }
            }
            model.addAttribute("assignInfoStr", assignInfo);
            
            String currentUser = userGroupAdminExtension.getCurrentUser(null);
            String extDataStr = task.getExtData();
            if (StringUtils.isNotEmpty(extDataStr)) {
                Map<String, Object> extData = JSON.parseObject(extDataStr, Map.class);
                Map<String, Object> taskTmpSaveData = (Map<String, Object>) extData.get("taskTmpSaveData");
                if (taskTmpSaveData != null) {
                    executeBizResult = (String) taskTmpSaveData.get("executeBizResult");
                    executeBizData = (String) taskTmpSaveData.get("executeBizData");
                    instanceBizData = (String) taskTmpSaveData.get("instanceBizData");
                    }
                Map<String, Object> userTmpSaveData = (Map<String, Object>) extData.get("userTmpSaveData");
                if (userTmpSaveData != null && userTmpSaveData.get(currentUser) != null) {
                    Map<String, Object> currentuserTmpSaveData = (Map<String, Object>) userTmpSaveData.get(currentUser);
                    executeBizResult = (String) currentuserTmpSaveData.get("executeBizResult");
                    executeBizData = (String) currentuserTmpSaveData.get("executeBizData");
                    instanceBizData = (String) currentuserTmpSaveData.get("instanceBizData");
                }
            }
        }

        String instanceFormId = detailProperties == null ? null : (String) detailProperties.get("instanceFormId");
        if (instanceFormId == null && flowDetailProperties != null) {
            instanceFormId = (String) flowDetailProperties.get("instanceFormId");
        }
        if (instanceFormId == null) {
            instanceFormId = (String) processProperties.get("formId");
        }
        model.addAttribute("instanceFormId", instanceFormId);

        String pageUrl = detailProperties == null ? null : (String) detailProperties.get("pageUrl");
        if (pageUrl != null) {
            Map<String, Object> root = new HashMap<String, Object>();
            root.put("taskNo", taskNo);
            root.put("flow", flow);
            root.put("processProperties", processProperties);
            root.put("taskProperties", taskProperties);
            String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
            if (StringUtils.isNotEmpty(pageUrlVal)) {
                return "redirect:" + pageUrlVal;
            }
        }
        
        String pageId = detailProperties == null ? null : (String) detailProperties.get("pageId");
        if (pageId == null && flowDetailProperties != null) {
            pageUrl = flowDetailProperties == null ? null : (String) flowDetailProperties.get("pageUrl");
            if (pageUrl != null) {
                Map<String, Object> root = new HashMap<String, Object>();
                root.put("taskNo", taskNo);
                root.put("flow", flow);
                root.put("processProperties", processProperties);
                root.put("taskProperties", taskProperties);
                String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
                if (StringUtils.isNotEmpty(pageUrlVal)) {
                    return "redirect:" + pageUrlVal;
                }
            }
            pageId = (String) flowDetailProperties.get("pageId");
        }
        if (pageId == null) {
            pageId = "commonProcessTaskDetail";
        }
        String taskDetailFormId = detailProperties == null ? null : (String) detailProperties.get("formId");
        model.addAttribute("taskDetailFormId", ProcessPageUtils.handleAddDefaultValue(taskDetailFormId));
        model.addAttribute("taskExecuteBizData", ProcessPageUtils.handleAddDefaultValue(task.getExecuteBizData()));
        Map<String, Object> pageData = new HashMap<>();
        pageData.put("taskNo", task.getTaskNo());
        pageData.put("instanceBizData", JSON.parseObject(instanceBizData, Map.class));
        pageData.put("executeBizResult", executeBizResult);
        pageData.put("executeBizData",  JSON.parseObject(executeBizData, Map.class));
        
        model.addAttribute("pageData", JSON.toJSONString(pageData));

        return "easyflow/process/processtask/commonProcessTaskDetail";
    }

    protected Map<String, Object> queryTaskAdminPropertyByProcessInstanceNo(List<String> processInstanceNos, String propertyKey) {
        if (ObjectUtils.isEmpty(processInstanceNos)) {
            return null;
        }
        ExportResponse<List<ProcessInstanceDTO>> exportResponse = getProcessInstanceExport().queryInstanceByInstanceNos(new ExportRequest<List<String>>(processInstanceNos));
        List<ProcessInstanceDTO> processInstanceDTOs = ExportResponseUtil.unwrap(exportResponse);
        Map<String, Object> resultMap = new HashMap<>(processInstanceDTOs.size());
        processInstanceDTOs.forEach(processInstanceDTO -> {
            String processDefId = processInstanceDTO.getProcessDefId();
            boolean value = true;
            try {
                Map<String, Object> instanceProperties = ((ProcessDTO) ExportResponseUtil.unwrap(getProcessDefinitionExport()
                        .getProcessProperties(new ExportRequest(processDefId)))).getProperties();
                value = getTaskAdminProperty(instanceProperties, propertyKey, value);
            } catch (Exception e) {
                logger.error("Query processDefinition:{} config exception:{}", processDefId, e);
                value = false;
            }
            resultMap.put(processInstanceDTO.getInstanceNo(), value);
        });
        return resultMap;
    }

    private <T> T getTaskAdminProperty(Map<String, Object> properties, String taskProperty, T defaultValue) {
        Map<String, Object> taskProperties = properties == null ? null
                : (Map<String, Object>) properties.get("task");
        Map<String, Object> adminProperties = taskProperties == null ? null
                : (Map<String, Object>) taskProperties.get("admin");
        if (null != adminProperties) {
            Object taskPropertyVal = adminProperties.get(taskProperty);
            if (null != taskPropertyVal) {
                return (T) taskPropertyVal;
            }
        }
        return defaultValue;
    }

    protected ProcessInstanceExport getProcessInstanceExport() {
        if (processInstanceExport == null) {
            processInstanceExport = ObjectFactorys.getDefault().getObject(ProcessInstanceExport.class);
        }
        return processInstanceExport;
    }

    protected ProcessDefinitionExport getProcessDefinitionExport() {
        if (processDefinitionExport == null) {
            processDefinitionExport = ObjectFactorys.getDefault().getObject(ProcessDefinitionExport.class);
        }
        return processDefinitionExport;
    }

    protected ProcessTaskExport getProcessTaskExport() {
        if (processTaskExport == null) {
            processTaskExport = ObjectFactorys.getDefault().getObject(ProcessTaskExport.class);
        }
        return processTaskExport;
    }

    @RequestMapping("ajax/getTaskHistoryListData")
    @ResponseBody
    public WebResponse<PagerResult> getTaskHistoryData(String taskNo) {
        ExportResponse<ProcessTaskDTO> taskResponse = getProcessTaskExport().getTask(new ExportRequest<String>(taskNo));
        ProcessTaskDTO task = ExportResponseUtil.unwrap(taskResponse);
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        PagerCondition condition = new PagerCondition(1, 1000);
        condition.addField(new FieldEntry("processInstanceNo", task.getProcessInstanceNo()));
        condition.addField(new FieldEntry("status", "FINISH"));
        ExportResponse response = getProcessTaskExport()
                .pagerQueryTask(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = (com.jd.easyflow.common.adapter.export.dto.pager.PagerResult) ExportResponseUtil
                .unwrap(response);
        List<ProcessTaskInfoForPagerDTO> processTaskList = ProcessTaskConverter.INSTANCE
                .convertPagerResult(pagerResult.getList());
        pagerResult.setList(processTaskList);
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                pagerResult);
    }

    public String getElType() {
        return elType;
    }

    public void setElType(String elType) {
        this.elType = elType;
    }

    public FlowParser getFlowParser() {
        return flowParser;
    }

    public void setFlowParser(FlowParser flowParser) {
        this.flowParser = flowParser;
    }

}

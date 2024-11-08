package com.jd.easyflow.admin.process.adapter.page;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jd.easyflow.admin.process.adapter.page.converter.PagerConverter;
import com.jd.easyflow.admin.process.adapter.page.converter.ProcessDefinitionConverter;
import com.jd.easyflow.admin.process.adapter.page.converter.ProcessInstanceConverter;
import com.jd.easyflow.admin.process.adapter.page.dto.CommonTaskProcessInstanceCreateDTO;
import com.jd.easyflow.admin.process.adapter.page.dto.ProcessDefDTO;
import com.jd.easyflow.admin.process.adapter.page.dto.ProcessInstanceInfoForPagerDTO;
import com.jd.easyflow.admin.process.adapter.page.extension.UserGroupAdminExtension;
import com.jd.easyflow.admin.process.adapter.page.util.ProcessPageUtils;
import com.jd.easyflow.codegenerator.adapter.export.CodeGenerateExport;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateResult;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
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
import com.jd.easyflow.flow.model.parser.FlowParserImpl;
import com.jd.easyflow.fsm.Fsm;
import com.jd.easyflow.fsm.model.State;
import com.jd.easyflow.fsm.parser.FsmParser;
import com.jd.easyflow.objects.factory.ObjectFactorys;
import com.jd.easyflow.process.adapter.export.ProcessDefinitionExport;
import com.jd.easyflow.process.adapter.export.ProcessInstanceExport;
import com.jd.easyflow.process.adapter.export.ProcessTaskExport;
import com.jd.easyflow.process.adapter.export.constant.ProcessInstanceConstants;
import com.jd.easyflow.process.adapter.export.constant.ProcessTaskConstants;
import com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO;
import com.jd.easyflow.process.adapter.export.dto.definition.QueryProcessDefinitionReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CanCancelProcessInstanceRes;
import com.jd.easyflow.process.adapter.export.dto.instance.CancelProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.CreateProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.ProcessNodeInstanceDTO;
import com.jd.easyflow.process.adapter.export.dto.instance.QueryProcessInstanceReq;
import com.jd.easyflow.process.adapter.export.dto.task.ProcessTaskDTO;
import com.jd.easyflow.process.adapter.export.dto.task.QueryTaskReq;
import com.jd.easyflow.utils.json.JSON;

@Controller
@RequestMapping("easyflow/processInstance")
public class ProcessInstanceController extends BasePageController {
    
    private static final Logger log = LoggerFactory.getLogger(ProcessInstanceController.class);


    private static final String INSTANCE_CREATE_PAGE_ID_TASK = "commonTaskProcessInstanceAdd";

    private static final String FORMAT_FLOW_BPMN = "FLOW-bpmn";
    private static final String FORMAT_FLOW_EASY = "FLOW-easy";
    private static final String FORMAT_FSM_EASY = "FSM-easy";

    @Autowired
    private UserGroupAdminExtension userGroupAdminExtension;

    private static final String VERSION_PREFIX = "--V_";

    private static final String BPMN_FORMAT = "FLOW-bpmn";

    private static final String EXT_DATA_BPMN_OF_JSON_KEY = "bpmnOfJson";

    private ProcessInstanceExport processInstanceExport;

    private ProcessDefinitionExport processDefinitionExport;
    
    private ProcessTaskExport processTaskExport;
    
    private CodeGenerateExport codeGenerateExport;

    protected FlowParser flowParser = new BpmnFlowParser();
    
    private String elType;

    @RequestMapping("list")
    public String processInstanceList() {
        return "easyflow/process/processinstance/processInstanceList";
    }
    
    @RequestMapping("ajax/getListData")
    @ResponseBody
    public WebResponse<PagerResult> getData() {
        PagerCondition condition = getPagerCondition();
        return getData(condition);
    }

    protected WebResponse<PagerResult> getData(PagerCondition condition) {
        FieldEntry fieldEntry = condition.getField("queryType");
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        if (fieldEntry != null && "MY".equals(fieldEntry.getValue())) {
            FieldEntry creatorEntry = condition.getField("creator");   
            if (creatorEntry != null) {
                String creator = (String) creatorEntry.getValue();
                if (!StringUtils.equals(creator, currentUser)) {
                    log.info("creator and current user not same. return empty data.");
                    com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = new com.jd.easyflow.common.adapter.export.dto.pager.PagerResult();
                    pagerResult.setCount(0L);
                    pagerResult.setList(new ArrayList<>());
                    return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(),
                            CommonErrorCode.E0000000.getDesc(), pagerResult);
                }
            } else {
            condition.addField(new FieldEntry("creator", currentUser));
            }
        }
        
        ExportResponse response = getProcessInstanceExport()
                .pagerQueryProcessInstance(new ExportRequest(PagerConverter.INSTANCE.convert(condition)));
        com.jd.easyflow.common.adapter.export.dto.pager.PagerResult pagerResult = (com.jd.easyflow.common.adapter.export.dto.pager.PagerResult) ExportResponseUtil
                .unwrap(response);

        List<ProcessInstanceInfoForPagerDTO> processInstanceList = ProcessInstanceConverter.INSTANCE
                .convertPagerResult(pagerResult.getList());
        List<ProcessInstanceInfoForPagerDTO> pagerDTOList = processInstanceList.stream().map(processInstance -> {
            CanCancelProcessInstanceReq req = new CanCancelProcessInstanceReq();
            req.setInstanceNo(processInstance.getInstanceNo());
            req.setCancelUser(currentUser);
            ExportResponse<CanCancelProcessInstanceRes> canCancelResponse = getProcessInstanceExport()
                    .canCancel(new ExportRequest<CanCancelProcessInstanceReq>(req));
            processInstance.setCanCanCel(ExportResponseUtil.unwrap(canCancelResponse).isCanCancel());
            return processInstance;
        }).collect(Collectors.toList());

        pagerResult.setList(pagerDTOList);
        return WebResponse.buildResponse(CommonErrorCode.E0000000.getCode(), CommonErrorCode.E0000000.getDesc(),
                pagerResult);
    }

    @RequestMapping("detail")
    public String instanceView(String instanceNo, String processType, String bizNo, Model model) throws IOException {
        ExportResponse<ProcessInstanceDTO> response;
        if (StringUtils.isNotBlank(instanceNo)) {
            response = getProcessInstanceExport().getProcessInstance(new ExportRequest(instanceNo));
        } else {
            QueryProcessInstanceReq req = QueryProcessInstanceReq.builder().processType(processType).bizNo(bizNo)
                    .build();
            response = getProcessInstanceExport().queryProcessInstanceByProcessTypeAndBizNo(new ExportRequest(req));
        }
        if (!CommonErrorCode.E0000000.getCode().equals(response.getResCode())) {
            throw new UserException("Process instance not exists.");
        }
        ProcessInstanceDTO processInstance = ExportResponseUtil.unwrap(response);
        Object[] processDefInfo = parseProcessDefIdAndVersion(processInstance.getProcessDefId());
        QueryProcessDefinitionReq req = new QueryProcessDefinitionReq();
        req.setDefId((String) processDefInfo[0]);
        req.setDefVersion((Integer) processDefInfo[1]);
        ExportResponse<ProcessDefinitionDTO> exportResponse = getProcessDefinitionExport()
                .queryProcessDefinitionByVersion(new ExportRequest(req));
        ProcessDefinitionDTO processDefinition = exportResponse.getData();
        ProcessDefDTO processDef = ProcessDefinitionConverter.INSTANCE.adapterConvert(processDefinition);
        if (BPMN_FORMAT.equals(processDefinition.getFormat())) {
            processDef.setBpmnXmlData(processDefinition.getContent());
        } else {
            String extData = processDefinition.getExtData();
            if (StringUtils.isNotEmpty(extData)) {
                Map<String, Object> extDataMap = JSON.parseObject(extData, Map.class);
                String bpmnData = (String) extDataMap.get(EXT_DATA_BPMN_OF_JSON_KEY);
                processDef.setBpmnXmlData(bpmnData);
            }
        }

        model.addAttribute("defData", JSON.toJSONString(processDef));
        ExportResponse<List<ProcessNodeInstanceDTO>> nodeResponse = getProcessInstanceExport()
                .queryProcessNodeInstanceByInstanceNo(new ExportRequest(processInstance.getInstanceNo()));
        List<ProcessNodeInstanceDTO> nodeInstanceList = nodeResponse.getData();
        nodeInstanceList = nodeInstanceList.stream().filter(node -> !ProcessInstanceConstants.NODE_STATUS_INVALID.equals(node.getStatus()))
                .collect(Collectors.toList());

        List<ProcessNodeInstanceDTO> sortedNodeInstanceList = sortNodeInstances(nodeInstanceList);
        fillNodeName(processDef, sortedNodeInstanceList);

        Map<String, Object> processProperties = null;
        if (FORMAT_FSM_EASY.equals(processDefinition.getFormat())) {
            Fsm fsm = FsmParser.parse(processDefinition.getJsonContent(), false);
            processProperties = fsm.getProperty("process");
        } else {
            Flow flow = flowParser.parse(processDefinition.getJsonContent(), false).get(0);
            processProperties = flow.getProperty("process");
        }
        String pageUrl = processProperties == null ? null : (String) processProperties.get("pageUrl");
        if (pageUrl != null) {
            Map<String, Object> root = new HashMap<String, Object>();
            root.put("instanceNo", instanceNo);
            root.put("processType", processType);
            root.put("bizNo", bizNo);
            String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
            if (StringUtils.isEmpty(pageUrlVal)) {
                return "redirect:" + pageUrlVal;
            }
        }
        

        QueryTaskReq queryTaskReq = new QueryTaskReq();
        queryTaskReq.setProcessInstanceNo(processInstance.getInstanceNo());
        ExportResponse<List<ProcessTaskDTO>> taskResponse = getProcessTaskExport().queryTask(new ExportRequest<QueryTaskReq>(queryTaskReq));
        List<ProcessTaskDTO> taskList = ExportResponseUtil.unwrap(taskResponse);
        
        String instanceFormId = processProperties == null ? null : (String) processProperties.get("formId");
        model.addAttribute("instanceFormId", ProcessPageUtils.handleAddDefaultValue(instanceFormId));
        model.addAttribute("instanceBizData", ProcessPageUtils.handleAddDefaultValue(processInstance.getBizData()));

        model.addAttribute("processDef", processDef);
        model.addAttribute("processInstance", processInstance);
        model.addAttribute("processNodeInstances", sortedNodeInstanceList);
        model.addAttribute("defData", JSON.toJSONString(processDef));
        
        ObjectMapper mapper = new ObjectMapper();
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        mapper.setDateFormat(format);
        model.addAttribute("nodeInstancesStr", JSON.toJSONString(sortedNodeInstanceList, mapper));
        model.addAttribute("tasksStr", JSON.toJSONString(taskList, mapper));
        
        return "easyflow/process/processinstance/processInstanceDetail";
    }

    private void fillNodeName(ProcessDefDTO processDef, List<ProcessNodeInstanceDTO> nodeList) {
        if (processDef == null) {
            log.warn("process definition null");
            return;
        }
        String definition = processDef.getJsonData();
        if (FORMAT_FLOW_BPMN.equals(processDef.getFormat()) || FORMAT_FLOW_EASY.equals(processDef.getFormat())) {
            Flow flow = new FlowParserImpl().parse(definition, false).get(0);
            for (ProcessNodeInstanceDTO node : nodeList) {
                FlowNode flowNode = flow.getNode(node.getNodeId());
                if (flowNode != null) {
                    node.setNodeName(flowNode.getName());
                }
            }
        } else {
            Fsm fsm = new FsmParser().parse(definition, false);
            for (ProcessNodeInstanceDTO node : nodeList) {
                State state = fsm.getState(node.getNodeId());
                if (state != null) {
                    node.setNodeName(state.getName());
                }
            }
        }
    }

    private Object[] parseProcessDefIdAndVersion(String defId) {
        defId = StringUtils.substringAfter(defId, "-");
        if (defId.contains(VERSION_PREFIX)) {
            String versionStr = StringUtils.substringAfter(defId, VERSION_PREFIX);
            Integer defVersion = Integer.parseInt(versionStr);
            String processDefId = StringUtils.substringBefore(defId, VERSION_PREFIX);
            return new Object[] {processDefId, defVersion};
        }
        return new Object[] {defId, null};
    }

    @RequestMapping("create")
    public String create() {
        return "easyflow/process/processinstance/processInstanceCreate";
    }

    @RequestMapping("add")
    public String add(String processId, Model model) {
        ExportResponse<com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO> definitionResponse = getProcessDefinitionExport()
                .getVersionedProcessDefinition(new ExportRequest<String>(processId));
        com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO definition = ExportResponseUtil
                .unwrap(definitionResponse);
        Flow flow = flowParser.parse(definition.getJsonContent(), false).get(0);
        Map<String, Object> processProperties = flow.getProperty("process");
        Map<String, Object> instanceCreateProperties = (Map<String, Object>) processProperties.get("instanceCreate");
        
        String pageUrl = processProperties == null ? null : (String) processProperties.get("pageUrl");
        if (pageUrl != null) {
            Map<String, Object> root = new HashMap<String, Object>();
            root.put("processId", processId);
            root.put("flow", flow);
            root.put("processProperties", processProperties);
            String pageUrlVal = ElFactory.get(elType).evalWithDefaultContext(pageUrl, root, true);
            if (StringUtils.isNotEmpty(pageUrlVal)) {
                return "redirect:" + pageUrlVal;
            }
        }

        String pageId = (String) instanceCreateProperties.get("pageId");
        String processInstanceFormId = (String) instanceCreateProperties.get("formId");
        if (processInstanceFormId == null) {
            processInstanceFormId = (String) processProperties.get("formId");
        }
        model.addAttribute("instanceFormId", processInstanceFormId);
        model.addAttribute("definition", definition);
        model.addAttribute("processName", flow.getName());
        if (pageId == null) {
            pageId = INSTANCE_CREATE_PAGE_ID_TASK;
        }

        if (INSTANCE_CREATE_PAGE_ID_TASK.equals(pageId)) {

            String startNodeId = flow.getStartNodeIds()[0];
            FlowNode flowNode = flow.getNode(startNodeId);
            Map<String, Object> taskProperties = (Map<String, Object>) flowNode.getProperty("task");
            Map<String, Object> createProperties = (Map<String, Object>) taskProperties.get("create");

            Map<String, Object> flowTaskProperties = flow.getProperty("task");
            Map<String, Object> flowCreateProperties = (Map<String, Object>) flowTaskProperties.get("taskCreate");

            String formId = (String) createProperties.get("formId");
            if (formId == null && flowCreateProperties != null) {
                formId = (String) flowCreateProperties.get("formId");
            }
            String instanceFormId = (String) createProperties.get("instanceFormId");
            if (instanceFormId != null) {
                model.addAttribute("instanceFormId", instanceFormId);
            }

            model.addAttribute("taskFormId", formId);
        }
        return "easyflow/process/processinstance/" + pageId;
    }

    @RequestMapping("ajax/commonTaskProcessInstanceAdd")
    @ResponseBody
    public DataResponse<Object> commonTaskProcessInstanceAdd(CommonTaskProcessInstanceCreateDTO dto) {
        ExportResponse<com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO> definitionResponse = getProcessDefinitionExport()
                .getVersionedProcessDefinition(new ExportRequest<String>(dto.getProcessId()));
        com.jd.easyflow.process.adapter.export.dto.definition.ProcessDefinitionDTO definition = ExportResponseUtil
                .unwrap(definitionResponse);
        Flow flow = flowParser.parse(definition.getJsonContent(), false).get(0);
        Map<String, Object> processProperties = flow.getProperty("process");
        String processType = (String) processProperties.get("processType");

        CreateProcessInstanceReq req = new CreateProcessInstanceReq();
        req.setProcessId(dto.getProcessId());
        req.setProcessType(processType);
        GenerateParam generateParam = GenerateParam.builder().typeId("PROCESS_BIZ_NO")
                .codePrefix("B").build();
        ExportResponse<GenerateResult> response = getCodeGenerateExport().generateUniqueCode(new ExportRequest<GenerateParam>(generateParam));
        String bizNo = ExportResponseUtil.unwrap(response).getCode();
        req.setBizNo(bizNo);

        req.setProductCode(dto.getProductCode());
        req.setCreator(userGroupAdminExtension.getCurrentUser(null));
        req.setInstanceName(dto.getInstanceName());
        req.setBizData(dto.getInstanceBizData());
        req.setKeyField(dto.getKeyField());
        req.setKeyField2(dto.getKeyField2());
        Map<String, Object> param = new HashMap<>();
        param.put(ProcessTaskConstants.PARAM_TASK_EXECUTE_DATA, dto.getTaskData());
        req.setParam(param);

        ExportResponseUtil.unwrap(
                getProcessInstanceExport().createProcessInstance(new ExportRequest<CreateProcessInstanceReq>(req)));
        return DataResponse.buildSuccessResponse(null);
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
    
    protected CodeGenerateExport getCodeGenerateExport() {
        if (codeGenerateExport == null) {
            codeGenerateExport = ObjectFactorys.getDefault().getObject(CodeGenerateExport.class);
        }
        return codeGenerateExport;
    }
    
    private List<ProcessNodeInstanceDTO> sortNodeInstances(List<ProcessNodeInstanceDTO> nodeList) {
        List<ProcessNodeInstanceDTO> result = new ArrayList<ProcessNodeInstanceDTO>();
        Map<String, ProcessNodeInstanceDTO> nodeMap = new HashMap<String, ProcessNodeInstanceDTO>(nodeList.size());
        nodeList.forEach(node -> nodeMap.put(node.getNodeInstanceNo(), node));
        
        while (nodeMap.size() > 0) {
            List<ProcessNodeInstanceDTO> noPreviousList = new ArrayList<ProcessNodeInstanceDTO>();
            for (ProcessNodeInstanceDTO node : nodeMap.values()) {
                String previousStr = node.getPreviousNodeInstances();
                if (StringUtils.isEmpty(previousStr)) {
                    noPreviousList.add(node);
                    continue;
                }
                String[] previousList = previousStr.split(",");
                boolean hasPrevious = false;
                for (String previous : previousList) {
                    if (nodeMap.containsKey(previous)) {
                       hasPrevious = true;
                       break;
                    }
                }
                if (! hasPrevious) {
                    noPreviousList.add(node);
                }
            }
            
            if (noPreviousList.size() == 0) {
                log.warn("Node instance list data error");
                result.addAll(nodeMap.values());
                break;
            }
            result.addAll(noPreviousList);
            noPreviousList.forEach(tmp -> nodeMap.remove(tmp.getNodeInstanceNo()));
        }
        return result;
    }
    
    @RequestMapping("ajax/cancel")
    @ResponseBody
    public DataResponse<Object> cancel(String instanceNo) {
        String currentUser = userGroupAdminExtension.getCurrentUser(null);
        CancelProcessInstanceReq req = new CancelProcessInstanceReq();
        req.setInstanceNo(instanceNo);
        req.setCancelUser(currentUser);
        ExportResponseUtil.unwrap(getProcessInstanceExport().cancel(new ExportRequest<CancelProcessInstanceReq>(req)));
        return DataResponse.buildSuccessResponse(null);
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

package com.jd.easyflow.admin.process.adapter.page;

import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.jd.easyflow.common.adapter.page.BasePageController;
import com.jd.easyflow.common.dto.DataResponse;
import com.jd.easyflow.common.util.CommonErrorCode;
import com.jd.easyflow.flow.bpmn.converter.BpmnConverter;

/**
 * @author liyuliang5
 */
@Controller
public class Bpmn2JsonController extends BasePageController {

    @CrossOrigin(origins = "*")
    @RequestMapping(value = "/public/ajax/bpmn2Json")
    @ResponseBody
    public DataResponse<String> convert2Json(String bpmnXmlData) {
        try {
            return new DataResponse<String>(BpmnConverter.convert(bpmnXmlData));
        } catch (Exception e) {
            return new DataResponse<String>(CommonErrorCode.E0000001.getCode(), e.getMessage(), null);
        }
    }
    
    @CrossOrigin(origins = "*")
    @RequestMapping(value = "/public/ajax/bpmn2Json2")
    @ResponseBody
    public Map<String, String> convert2Json2(String bpmnXmlData) {
        Map<String, String> result = new HashMap<>();
        String jsonData = BpmnConverter.convert(bpmnXmlData);
        result.put("resultData", jsonData);
        return result;
    }   

}

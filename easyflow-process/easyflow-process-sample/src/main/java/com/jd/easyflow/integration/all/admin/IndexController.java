package com.jd.easyflow.integration.all.admin;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author liyuliang5
 *
 */
@Controller
public class IndexController {


    @RequestMapping("/")
    public String index() {
        return "redirect:/easyflow/processInstance/list";
    }

}

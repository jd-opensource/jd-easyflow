INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (222,'PROCESS_TEST_TASK_EXECUTE_001','Test task execute',NULL,'{\"type\": \"page\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 4, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"审批意见\", \"type\": \"textarea\", \"cfgKey\": \"auditInfo\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"valProcess\": \"key\"}]}]}]}]}, \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-01-15 07:26:54','2022-01-15 07:26:54',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (234,'PROCESS_TEST_TASK_CREATE_001','test task create',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": null, \"name\": \"测试1\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"测试1\", \"type\": \"inputText\", \"cfgKey\": \"test\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-01-18 14:32:07','2022-01-18 14:32:07',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (239,'PROCESS_TEST_TASK_EXECUTE_002','test 002 node template',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 4, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"审批意见\", \"type\": \"inputText\", \"cfgKey\": \"auditInfo\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": true, \"valProcess\": \"key\"}, {\"cols\": -1, \"name\": \"补充信息\", \"type\": \"select\", \"cfgKey\": \"select\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"selectList\": [{\"name\": \"a\", \"value\": \"a\"}, {\"name\": \"b\", \"value\": \"b\"}], \"selectType\": \"single\", \"valProcess\": \"key\", \"selectValType\": \"\"}]}]}]}]}, \"bottomHtml\": \"         <form method=\\\"post\\\" id=\\\"executeForm\\\" class=\\\"j-ajax-form\\\">\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"taskNo\\\" id=\\\"taskNo\\\"/>\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"executeBizResult\\\" id=\\\"executeBizResult\\\"/>\\n                <textarea name=\\\"executeBizData\\\" id=\\\"executeBizData\\\" class=\\\"d-none\\\" ></textarea>\\n          <div class=\\\"row mt-2\\\">\\n            <div class=\\\"col text-center\\\">\\n            <button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"PASS\\\">通过2</button>\\n\\t\\t\\t<button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"REJECT\\\">拒绝2</button>\\n\\t\\t\\t<button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"BACK\\\">退回到开始节点</button>\\n         </div>\\n    </div> \\t\\t\\t\\t\\n        </form> \", \"nullPolicy\": null, \"afterRender\": \"var $executeForm=$(\\\"#executeForm\\\");\\n$executeForm.attr(\\\"action\\\",\\\"/easyflow/processTask/ajax/commonTaskExecute\\\");\\n$executeForm.data(\\\"success-url\\\", \\\"/easyflow/processTask/list\\\");\\n$executeForm.find(\\\".j-btn-ok\\\").click(function(){\\n    taskPage.collect(taskData);\\n     $(\\\"#taskNo\\\").val(pageData.taskNo);\\n     $(\\\"#executeBizResult\\\").val($(this).data(\\\"biz-result\\\"));\\n     $(\\\"#executeBizData\\\").val(JSON.stringify(taskData, null, 2));\\n    if (page.$form.valid()) {\\n        $executeForm.submit();\\n         return false;\\n    } else {\\n        $.jMessage({title:\\\"提示\\\", msg:\\\"信息录入有误，请检查\\\"});\\n    }\\n}); \", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-02-22 23:18:39','2022-02-22 23:18:39',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (240,NULL,NULL,NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 4, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"测试1\", \"type\": \"table\", \"cfgKey\": \"aaa\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": true, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-08-13 15:47:29','2022-08-13 15:47:29',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (244,'PROCESS_TEST_001','PT-001',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": null, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"请假说明\", \"type\": \"textarea\", \"cfgKey\": \"myData\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-08-14 14:01:32','2022-08-14 14:01:32',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (245,'TABLE_TEST','table test',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 1, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"表格\", \"type\": \"table\", \"cfgKey\": \"abc\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"json\", \"modify0\": true, \"newline\": false, \"required\": true, \"valProcess\": \"key\", \"beforeRender\": \"this.tableConf={\\ncolumns: [\\n            {radio: true},\\n            {field: \'instanceName\', title: \'流程名称\'},\\n            {field: \'processType\', title: \'业务类型\'}\\n        ]\\n}\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2022-08-15 11:26:45','2022-08-15 11:26:45',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (252,'PROCESS_TEST_TMP_SAVE_001','form instance-tmp save',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": null, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"请假说明\", \"type\": \"textarea\", \"cfgKey\": \"myData\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2023-02-04 20:02:31','2023-02-04 20:02:31',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (255,'PROCESS_TEST_TASK_EXECUTE_TMP_SAVE_002','tmp save form',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 4, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"审批意见\", \"type\": \"inputText\", \"cfgKey\": \"auditInfo\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": true, \"valProcess\": \"key\"}, {\"cols\": -1, \"name\": \"补充信息\", \"type\": \"select\", \"cfgKey\": \"select\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"selectList\": [{\"name\": \"a\", \"value\": \"a\"}, {\"name\": \"b\", \"value\": \"b\"}], \"selectType\": \"single\", \"valProcess\": \"key\", \"selectValType\": \"\"}]}]}]}]}, \"bottomHtml\": \"         <form method=\\\"post\\\" id=\\\"executeForm\\\" class=\\\"j-ajax-form\\\">\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"taskNo\\\" id=\\\"taskNo\\\"/>\\n                        <input type=\\\"hidden\\\" name=\\\"instanceBizData\\\" id=\\\"instanceBizData\\\"/>\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"executeBizResult\\\" id=\\\"executeBizResult\\\"/>\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"operation\\\" id=\\\"operation\\\"/>\\n\\t\\t        <input type=\\\"hidden\\\" name=\\\"version\\\" id=\\\"version\\\"/>\\n                <textarea name=\\\"executeBizData\\\" id=\\\"executeBizData\\\" class=\\\"d-none\\\" ></textarea>\\n          <div class=\\\"row mt-2\\\">\\n            <div class=\\\"col text-center\\\">\\n           <button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-operation=\\\"SAVE\\\">暂存</button>\\n            <button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"PASS\\\">通过2</button>\\n\\t\\t\\t<button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"REJECT\\\">拒绝2</button>\\n\\t\\t\\t<button type=\\\"submit\\\" class=\\\"btn btn-primary j-btn-ok\\\" data-biz-result=\\\"BACK\\\">退回到开始节点</button>\\n         </div>\\n    </div> \\t\\t\\t\\t\\n        </form> \", \"nullPolicy\": null, \"afterRender\": \"var $executeForm=$(\\\"#executeForm\\\");\\n$executeForm.attr(\\\"action\\\",\\\"/easyflow/processTask/ajax/commonTaskExecute\\\");\\n$executeForm.data(\\\"success-url\\\", \\\"/easyflow/processTask/list\\\");\\n$executeForm.find(\\\".j-btn-ok\\\").click(function(){\\n\\t\\t if (! instancePage.$form.valid()) {\\n\\t\\t\\t $.jMessage({title:\\\"提示\\\", msg:\\\"流程实例信息录入有误，请检查\\\"}); \\n\\t\\t\\t return false;\\n\\t\\t } else {\\n\\t\\t\\t instancePage.collect(instanceData);\\n\\t\\t\\t $(\\\"#instanceBizData\\\").val(JSON.stringify(instanceData, null, 2));\\n\\t\\t }\\n    taskPage.collect(taskData);\\n     $(\\\"#taskNo\\\").val(pageData.taskNo);\\n     $(\\\"#version\\\").val(pageData.version);\\n     $(\\\"#executeBizResult\\\").val($(this).data(\\\"biz-result\\\"));\\n     $(\\\"#operation\\\").val($(this).data(\\\"operation\\\"));\\n     $(\\\"#executeBizData\\\").val(JSON.stringify(taskData, null, 2));\\n    if (page.$form.valid()) {\\n        $executeForm.submit();\\n         return false;\\n    } else {\\n        $.jMessage({title:\\\"提示\\\", msg:\\\"信息录入有误，请检查\\\"});\\n    }\\n}); \", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2023-02-18 18:13:28','2023-02-18 18:13:28',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (256,'PROCESS_SHELL_001','SHELL flow instance',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": null, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"申请信息\", \"type\": \"textarea\", \"cfgKey\": \"applyData\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": true, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2023-03-22 21:42:18','2023-03-22 21:42:18',0);
INSERT INTO `form_template` (`id`,`template_code`,`template_name`,`biz_type`,`config`,`status`,`created_date`,`modified_date`,`deleted`) VALUES (257,'PROCESS_SHELL_TASK_AUDIT_001','SHELL task audit template',NULL,'{\"type\": \"page\", \"topHtml\": \"\", \"component\": {\"type\": \"tabs\", \"components\": [{\"name\": \"\", \"type\": \"tab\", \"sider\": false, \"components\": [{\"name\": \"\", \"type\": \"panel\", \"components\": [{\"cols\": 4, \"name\": \"\", \"type\": \"card\", \"components\": [{\"cols\": -1, \"name\": \"审批意见\", \"type\": \"textarea\", \"cfgKey\": \"auditInfo\", \"modify\": true, \"source\": \"product\", \"cfgType\": \"string\", \"modify0\": true, \"newline\": false, \"required\": false, \"valProcess\": \"key\"}]}]}]}]}, \"bottomHtml\": \"\", \"nullPolicy\": null, \"afterRender\": \"\", \"afterCollect\": \"\", \"beforeRender\": \"\", \"beforeCollect\": \"\"}','VALID','2023-03-22 23:09:16','2023-03-22 23:09:16',0);


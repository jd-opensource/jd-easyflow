# JDEasyFlow

### 介绍
JDEasyFlow是一款通用流程编排组件, 适用于服务编排、工作流、任务审批等场景。它的特点是简单、灵活、易扩展。开发人员30分钟可入门上手，半天可掌握其原理。

### 软件架构
JDEasyFlow底层为流程引擎/状态机模块(使用时选一便可，建议优先使用流程引擎)，此模块提供了基于JSON格式的JDEasyFlow规范进行流程编排的能力。

BPMN模块提供了基于BPMN规范进行流程定义和可视化的能力，流程可视化基于[bpmn-js](https://bpmn.io/)，其本质为提供了将BPMN格式流程定义转换为JDEasyFlow格式的能力。


### 使用说明

在源码的test目录下有quickstart测试用例，可直接运行或调试以了解使用方式和运行原理。

#### 流程引擎

1. 代码中引入easyflow-flow jar包，以maven为例:
```
    <dependency>
        <groupId>com.jd.easyflow</groupId>
        <artifactId>easyflow-flow</artifactId>
        <version>{latestVersion}</version>
    </dependency>
```
2. 编写流程定义文件
以node001->node002->node003的执行顺序为例:
```
{"id": "quickstart_001", "name": "Quick Start 001",
"nodes": [
  {"id": "node001","name": "Node001","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart001Node01Action()"},"start": true,"post": {"to": "node002"}},
  {"id": "node002","name": "Node002","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart002Node01Action()"},"post": {"to": "node003"}},
  {"id": "node003","name": "Node003","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart003Node01Action()"}}
]
}
```
其中QuickStart001Node01Action等为java节点动作类
3. 编写应用启动时加载流程引擎的代码
```
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/quickstart/quickstart_001.json");
        flowEngine.init();
```     
Spring环境可直接定义FlowEngineImpl bean.        
4. 编写具体流程调用执行的代码
```
       FlowParam param = new FlowParam("quickstart_001");
        FlowResult result = flowEngine.execute(param);
```
日志打印结果如下:
```
[main            ] INFO  FlowEngineImpl          - Start parsing definition files:easyflow-flow/target/test-classes/flow/quickstart/quickstart_001.json
[main            ] INFO  FlowEngineImpl          - SART EXECUTE FLOW, flowId:quickstart_001 nodeIds:null
[main            ] INFO  BaseFlowRunner          - EXECUTE NODE:node001
[main            ] INFO  QuickStart001Node01Action  - Execute Node 001
[main            ] INFO  BaseFlowRunner          - NEXT NODES:node002
[main            ] INFO  BaseFlowRunner          - EXECUTE NODE:node002
[main            ] INFO  QuickStart002Node01Action  - Execute Node 002
[main            ] INFO  BaseFlowRunner          - NEXT NODES:node003
[main            ] INFO  BaseFlowRunner          - EXECUTE NODE:node003
[main            ] INFO  QuickStart003Node01Action  - Execute Node 003
[main            ] INFO  BaseFlowRunner          - NEXT NODES:
[main            ] INFO  QuickStartTest          - Execute finish, current node is:node003           
```
以上只是简单使用示例，EasyFlow可支持很多的配置项和使用场景，更多使用见wiki文档.

#### 流程引擎-BPMN
打开easyflow-flow-bpmn/BPMNDesigner.html流程设计器. 点击导入按钮，导入easyflow-flow-bpmn/src/test/resources/flow/quickstart/quickstart_001.bpmn文件，可在设计器中看到和以上JSON定义等价的BPMN流程定义.
使用时只需要将FlowEngineImpl的flowParser设置为BpmnFlowParser.

#### 工作流
工作流模块提供了基于流程引擎的持久化和任务审批能力, 需要关系型数据库支持. 源码中的示例工程启动步骤如下:
1. 检出本工程库代码至本地.
1. 安装关系型数据库, 如MYSQL(使用时需遵守其许可). 
1. 建库表, 数据库名为easyflow, 表结构可参考源码中的create_all_table.sql和sample_form_template.sql. 
1. sample模块的application-open-all.yml文件中配置数据库连接信息, pom中引入数据库驱动jar包.
1. 编译构建代码工程, 启动IntegrationOpenApplication类. 启动成功后访问http://localhost:9888便可看到JDEasyFlow管理界面.


### 更多
JDEasyFlow具有非常灵活的扩展性，你可以基于目前已开源组件扩展做更多的功能.

### 联系我们
email: liyuliang5@jd.com
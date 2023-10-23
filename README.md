# JDEasyFlow

### Introduce
JDEasyFlow is a general flow orchestration component, suitable for service orchestration、workflow、auditing, etc. The characteristics are easy use、flexible、easy extended. Developer can understand using it in 30 minutes，understand its principle half of the day.

### Architecture
JDEasyFlow bottom layer is flow engine/state machine.(select one when use it, flow engine is recommanded), this module supply flow orchestration ability base on JSON format flow definition.

BPMN module supply define flow based on BPMN and visualization ability. visualization is based on [bpmn-js](https://bpmn.io/). The essence of this module is
convert BPMN format definition to JDEasyFlow JSON format definition.


### Usage

There are test cases in the test package of source code. You can run or debug directly to understand its usage and implement principle. 

#### Flow Engine

1. Import easyflow-flow jar. take maven as example:
```
    <dependency>
        <groupId>com.jd.easyflow</groupId>
        <artifactId>easyflow-flow</artifactId>
        <version>{latestVersion}</version>
    </dependency>
```
2. Write flow definition. For example, the sequence is node001->node002->node003:
```
{"id": "quickstart_001", "name": "Quick Start 001",
"nodes": [
  {"id": "node001","name": "Node001","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart001Node01Action()"},"start": true,"post": {"to": "node002"}},
  {"id": "node002","name": "Node002","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart002Node01Action()"},"post": {"to": "node003"}},
  {"id": "node003","name": "Node003","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart003Node01Action()"}}
]
}
```
QuickStart001Node01Action and so on is java node action class.
3. Write the code of loading flow engine when application start.
```
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/quickstart/quickstart_001.json");
        flowEngine.init();
```     
You can define FlowEngineImpl bean in Spring.    
4. Write invoke flow engine code.
```
       FlowParam param = new FlowParam("quickstart_001");
        FlowResult result = flowEngine.execute(param);
```
The executing log are as follows:
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
#### FlowEngine-BPMN
Open flow designer with path easyflow-flow-bpmn/BPMNDesigner.html. Click import button，import easyflow-flow-bpmn/src/test/resources/flow/quickstart/quickstart_001.bpmn file. You can see bpmn flow definition of equal JSON format.
You only need set flowPaser of FlowEngineImpl to BpmnFlowParser when use.

### More
Above is simple usecase, JDEasyFlow support many configurations and use cases. More can be seen in wiki doc.

JDEasyFlow has very flexible extension ability. You can implement more feature based on current component. For example flow data persistence、auditting、exception retry. 

### Contact US
mailTo: liyuliang5@jd.com



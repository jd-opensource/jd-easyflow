# JDEasyFlow

### Introduction

JDEasyFlow is a general-purpose flow orchestration component for service orchestration, workflows, approvals, and similar scenarios. It is easy to use, flexible, and extensible. Developers can learn the basic usage in about 30 minutes and understand the core principles within half a day.

### Architecture

JDEasyFlow is built on a flow engine and a state machine. Choose one based on your scenario; the flow engine is recommended for most use cases. The flow engine provides JSON-based flow orchestration capabilities.

The BPMN module provides BPMN-based flow definition and visualization capabilities. Visualization is based on [bpmn-js](https://bpmn.io/). The module converts BPMN definitions into JDEasyFlow JSON definitions.

### Usage

Test cases are available in the source code. You can run or debug them directly to understand the usage and implementation principles.

#### Flow Engine

1. Import the `easyflow-flow` JAR. Maven example:

```
    <dependency>
        <groupId>com.jd.easyflow</groupId>
        <artifactId>easyflow-flow</artifactId>
        <version>{latestVersion}</version>
    </dependency>
```

2. Write a flow definition. For example, the following flow runs in sequence: `node001 -> node002 -> node003`.

```
{"id": "quickstart_001", "name": "Quick Start 001",
"nodes": [
  {"id": "node001","name": "Node001","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart001Node01Action()"},"start": true,"post": {"to": "node002"}},
  {"id": "node002","name": "Node002","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart002Node01Action()"},"post": {"to": "node003"}},
  {"id": "node003","name": "Node003","action": {"createExp": "new com.jd.easyflow.flow.quickstart.QuickStart003Node01Action()"}}
]
}
```

`QuickStart001Node01Action` and similar classes are Java node action classes.

3. Load the flow engine when the application starts.

```
        FlowEngineImpl flowEngine = new FlowEngineImpl();
        flowEngine.setFlowPath("classpath:flow/quickstart/quickstart_001.json");
        flowEngine.init();
```

You can also define `FlowEngineImpl` as a Spring bean. In that case, do not configure or invoke the `init()` method explicitly.

4. Invoke the flow engine.

```
       FlowParam param = new FlowParam("quickstart_001");
        FlowResult result = flowEngine.execute(param);
```

The execution log is as follows:

```
[main            ] INFO  FlowEngineImpl          - Start parsing definition files:easyflow-flow/target/test-classes/flow/quickstart/quickstart_001.json
[main            ] INFO  FlowEngineImpl          - START EXECUTE FLOW, flowId:quickstart_001 nodeIds:null
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

This is a simple use case. JDEasyFlow supports many other configurations and usage patterns. For more information, see the wiki documentation.

#### FlowEngine-BPMN

Open the flow designer at `easyflow-flow-bpmn/BPMNDesigner.html`. Click **Import**, then import `easyflow-flow-bpmn/src/test/resources/flow/quickstart/quickstart_001.bpmn`. You will see a BPMN flow definition equivalent to the JSON definition.

To use BPMN definitions, set the `flowParser` of `FlowEngineImpl` to `BpmnFlowParser`.

#### Process

The Process module provides persistence and task approval capabilities based on the flow engine. It requires a relational database. You can start the sample application from the source code as follows:

1. Check out the source code.
1. Install a relational database, such as MySQL. Make sure you comply with the database license.
1. Create the database and tables. The database name is `easyflow`. For the table schema, see `create_all_table.sql` and `sample_form_template.sql`.
1. Configure the database connection in `application-open-all.yml` of the sample module, and import the database driver JAR in the sample module's `pom.xml`.
1. Build the project, start `IntegrationOpenApplication`, and access `http://localhost:9888`.

### More

JDEasyFlow is highly extensible. You can build more capabilities on top of the current component, such as flow data persistence, auditing, and exception retry.

### Contact Us

mailto: liyuliang5@jd.com

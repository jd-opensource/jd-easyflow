package com.jd.easyflow.flow.cases.parser;

import java.util.List;
import java.util.Map;

import com.jd.easyflow.flow.model.parser.event.FlowParseEvent;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventListener;
import com.jd.easyflow.flow.model.parser.event.FlowParseEventTypes;

/**
 * 
 * @author liyuliang5
 *
 */
public class TestFlowParseListener implements FlowParseEventListener {

    @Override
    public void on(FlowParseEvent event) {
        switch (event.getType()) {
        case FlowParseEventTypes.PARSE_FLOW_START: {
            List<Map<String, Object>> nodeList = (List<Map<String, Object>>) event.getFlowDef().get("nodes");
            for (Map<String, Object> node : nodeList) {
                String name = (String) node.get("name");
                if (name == null) {
                    node.put("name", node.get("id"));
                }
            }
            break;
        }
        case FlowParseEventTypes.PARSE_FLOW_END: {
            event.getFlow().setStartNodeIds(new String[] { event.getFlow().getNodeList().get(0).getId() });
            break;
        }

        }

    }

}

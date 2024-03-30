package com.jd.easyflow.flow.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * JSON pretty handler to formating json definition.
 * 
 * @author liyuliang5
 *
 */
public class JsonPrettyHelper {

    public static final Logger logger = LoggerFactory.getLogger(JsonPrettyHelper.class);

    private static final int INDENT = 2;

    private static final String KEY_TYPE = "keyType";

    public static String pretty(String valueStr, String prettyConfStr) {
        Object value = JsonUtil.parseObject(valueStr, Object.class);
        Map<String, Object> prettyConf = JsonUtil.parseObject(prettyConfStr, Map.class);
        StringBuilder builder = new StringBuilder();
        pretty(value, prettyConf, (Map<String, Object>) prettyConf.get("default"), builder, 0);
        return builder.toString();
    }

    public static String pretty(Object value, Map<String, Object> prettyConf) {
        StringBuilder builder = new StringBuilder();
        pretty(value, prettyConf, (Map<String, Object>) prettyConf.get("default"), builder, 0);
        return builder.toString();
    }

    private static void pretty(Object value, Map<String, Object> prettyConf, Map<String, Object> defaultConf,
            StringBuilder builder, int indent) {
        if (value == null) {
            builder.append("null");
        } else if (value instanceof String) {
            builder.append(quote((String) value));
        } else if (value instanceof Number || value instanceof Boolean) {
            builder.append(value);
        } else if (value instanceof Map) {
            prettyMap((Map<String, Object>) value, prettyConf, defaultConf, builder, indent);
        } else if (value instanceof List) {
            prettyList((List) value, prettyConf, defaultConf, builder, indent);
        } else {
            throw new UnsupportedOperationException("Unsupported type:" + value.getClass());
        }

    }

    /**
     * Pretty Map
     * 
     * @param value
     * @param prettyConf
     * @param builder
     * @param indent
     */
    public static void prettyMap(Map<String, Object> value, Map<String, Object> prettyConf,
            Map<String, Object> defaultConf, StringBuilder builder, int indent) {
        if (prettyConf == null) {
            prettyConf = new HashMap<>();
        }
        // Head
        builder.append("{");
        // Element
        indent = indent + INDENT;
        List<String> processedKey = new ArrayList<>();

        List<Map<String, Object>> subList = (List<Map<String, Object>>) prettyConf.get("subList");
        if (subList == null) {
            subList = new ArrayList<>();
        }
        if (!subList.stream().anyMatch(map -> "OTHER".equals(map.get(KEY_TYPE)))) {
            Map<String, Object> otherConf = new HashMap<>();
            otherConf.put("keyType", "OTHER");
            subList.add(otherConf);
        }

        boolean hasElements = false;
        for (Map<String, Object> subConf : subList) {
            Map<String, Object> subDefaultConf = (Map<String, Object>) subConf.get("default");
            subDefaultConf = subDefaultConf == null ? defaultConf : subDefaultConf;

            List<String> keys = null;
            String confKey = (String) subConf.get("key");
            String keyType = (String) subConf.get("keyType");
            if (FlowStringUtil.isNotEmpty(confKey)) {
                keys = Arrays.asList(confKey);
            } else if ("OTHER".equals(keyType)) {
                keys = subtract(new ArrayList<String>(value.keySet()), processedKey);
            } else if (subConf.containsKey("subList")){
                continue;
            } else {
                throw new IllegalArgumentException("Config error," + subConf);
            }
            if (keys.size() > 0) {
                for (String key : keys) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Start pretty element:" + key);
                    }
                    if (!value.containsKey(key)) {
                        continue;
                    }
                    boolean subNewLine = Boolean.TRUE.equals(getConf(subConf, subDefaultConf, "newLine"));
                    if (subNewLine) {
                        builder.append(newLine(indent));
                    }
                    processedKey.add(key);
                    builder.append(quoteColon(key));
                    pretty(value.get(key), subConf, subDefaultConf, builder, indent);
                    builder.append(",");
                    hasElements = true;
                }
            }
        }
        if (hasElements) {
            builder.deleteCharAt(builder.length() - 1);
        }
        // Tail
        indent = indent - INDENT;
        boolean endNewLine = Boolean.TRUE.equals(getConf(prettyConf, defaultConf, "endNewLine"));
        if (endNewLine) {
            builder.append(newLine(indent));
        }
        builder.append("}");
    }
    
    private static List<String> subtract(List<String> list1, List<String> list2) {
        List<String> result = new ArrayList<>();
        if (list1 == null) {
            return result;
        }
        for (String s : list1) {
            if (!list2.contains(s)) {
                result.add(s);
            }
        }
        return result;
    }

    /**
     * Pretty Array
     * 
     * @param value
     * @param prettyConf
     * @param builder
     * @param indent
     */
    public static void prettyList(List<Object> value, Map<String, Object> prettyConf, Map<String, Object> defaultConf,
            StringBuilder builder, int indent) {
        if (prettyConf == null) {
            prettyConf = new HashMap<>();
        }

        List<Map<String, Object>> subConfList = (List<Map<String, Object>>) prettyConf.get("subList");
        if (subConfList == null) {
            subConfList = new ArrayList<>();
            Map<String, Object> arrayConf = new HashMap<>();
            subConfList.add(arrayConf);
        }
        // Head
        builder.append("[");
        // Element
        indent = indent + INDENT;
        Map<String, Object> subConf = subConfList.get(0);
        Map<String, Object> subDefaultConf = (Map<String, Object>) subConf.get("default");
        subDefaultConf = subDefaultConf == null ? defaultConf : subDefaultConf;

        if (value.size() > 0) {
            for (Object o : value) {
                boolean subNewLine = Boolean.TRUE.equals(getConf(subConf, subDefaultConf, "newLine"));
                if (subNewLine) {
                    builder.append(newLine(indent));
                }
                pretty(o, subConf, subDefaultConf, builder, indent);
                builder.append(",");
            }
            builder.deleteCharAt(builder.length() - 1);
        }
        // Tail
        indent = indent - INDENT;
        boolean endNewLine = Boolean.TRUE.equals(getConf(prettyConf, defaultConf, "endNewLine"));
        if (endNewLine) {
            builder.append(newLine(indent));
        }
        builder.append("]");
    }

    // ===Utils===

    private static <T> T getConf(Map<String, Object> confMap, Map<String, Object> defaultConf, String key) {
        Object value = confMap.get(key);
        if (value != null) {
            return (T) value;
        }
        if (defaultConf != null) {
            value = defaultConf.get(key);
        }
        return (T) value;
    }

    private static String newLine(int indent) {
        return "\n" + FlowStringUtil.repeat(' ', indent);
    }

    private static String blank(int num) {
        return FlowStringUtil.repeat(' ', num);
    }

    private static String quote(String str) {
        return JsonUtil.toJsonString(str);
    }

    private static String quoteColon(String s) {
        return quote(s) + ":";
    }

    /**
     * intent chars.
     * 
     * @param s
     * @param indent
     * @return
     */
    private static String indent(String s, int indent) {
        String[] lines = s.split("\n");
        StringBuilder builder = new StringBuilder();
        for (String line : lines) {
            builder.append(FlowStringUtil.repeat(' ', indent) + line);
        }
        return builder.toString();
    }

    /**
     * intent chars.
     * 
     * @param s
     * @param indent
     * @return
     */
    private static String indent(String[] lines, int indent) {
        StringBuilder builder = new StringBuilder();
        for (String line : lines) {
            builder.append(FlowStringUtil.repeat(' ', indent) + line);
        }
        return builder.toString();
    }

}

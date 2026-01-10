package com.jd.easyflow.processunit.infrastructure.repository;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.type.TypeReference;
import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.processunit.domain.model.entity.ProcessUnitEntity;
import com.jd.easyflow.utils.json.JSON;

/**
 * 
 * @author liyuliang5
 */
public class ProcessUnitConfigFileCache implements ProcessUnitConfigCache {

    private static final Logger log = LoggerFactory.getLogger(ProcessUnitConfigFileCache.class);

    protected String processUnitConfigPath = "/config/processUnitConfig.json";

    protected volatile Map<String, ProcessUnitEntity> processUnitConfigMap;


    public void init() {
        if (processUnitConfigPath != null && ! processUnitConfigPath.isEmpty()) {
            try {
                processUnitConfigMap = JSON.parseObject(
                        toString(this.getClass().getResourceAsStream(processUnitConfigPath)),
                        new TypeReference<Map<String, ProcessUnitEntity>>() {
                        });
                processUnitConfigMap.entrySet().forEach(entry -> {
                    entry.getValue().setParentCode(entry.getKey());
                });
            } catch (IOException e) {
                throw new EasyFlowException("processUnit read exception");
            }
        } else {
            processUnitConfigMap = new ConcurrentHashMap<>();
        }
    }

    public String getProcessUnitConfigPath() {
        return processUnitConfigPath;
    }

    public void setProcessUnitConfigPath(String processUnitConfigPath) {
        this.processUnitConfigPath = processUnitConfigPath;
    }

    @Override
    public ProcessUnitEntity getProcessUnit(String key) {
        ProcessUnitEntity processUnit = processUnitConfigMap.get(key);
        return processUnit;
    }

    @Override
    public List<ProcessUnitEntity> getAllProcessUnitList() {
        return new ArrayList<>(processUnitConfigMap.values());

    }

    private static String toString(InputStream inputStream) throws IOException {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        byte[] buffer = new byte[1024];
        int length;
        while ((length = inputStream.read(buffer)) != -1) {
            result.write(buffer, 0, length);
        }
        String str = result.toString(StandardCharsets.UTF_8.name());
        return str;
    }

}

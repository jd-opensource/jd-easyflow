package com.jd.easyflow.codegenerator.client;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jd.easyflow.codegenerator.adapter.export.CodeGenerateExport;
import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.BatchGenerateResult;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateParam;
import com.jd.easyflow.codegenerator.adapter.export.dto.GenerateResult;
import com.jd.easyflow.common.adapter.export.dto.ExportRequest;
import com.jd.easyflow.common.adapter.export.dto.ExportResponse;
import com.jd.easyflow.common.adapter.export.util.ExportResponseUtil;
import com.jd.easyflow.objects.factory.ObjectFactorys;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerateHelper {

    private static final Logger logger = LoggerFactory.getLogger(CodeGenerateHelper.class);

    public static boolean cache = true;

    public static int batchRequestSize = 100;

    private static Map<String, ObjectIdCache> objectIdCacheMap = new ConcurrentHashMap<>();
    
    public static boolean clearByDay = true;
    
    private static volatile long currentDayMills = 0;
    
    private static final long ONE_DAY_MILLS = 24L * 60 * 60 * 1000;

    public static long clearByDayDelayMills = 10;

    public static String generateCode(String typeId, String codePrefix) {
        CodeGenerateParam codeGenerator = CodeGenerateParam.builder().typeId(typeId).codePrefix(codePrefix).build();
        return generateCode(codeGenerator);
    }

    private static String generateCode(CodeGenerateParam codeGenerator) {
        if (logger.isDebugEnabled()) {
            logger.debug("Get ID, type:" + codeGenerator.getTypeId());
        }
        if (!cache) {
            GenerateParam generateParam = GenerateParam.builder().typeId(codeGenerator.getTypeId())
                    .codePrefix(codeGenerator.getCodePrefix()).build();
            ExportResponse<GenerateResult> generateRes = ObjectFactorys.getDefault().getObject(CodeGenerateExport.class)
                    .generateUniqueCode(new ExportRequest<GenerateParam>(generateParam));
            GenerateResult result = ExportResponseUtil.unwrap(generateRes);
            return result.getCode();
        }

        ObjectIdCache cache = objectIdCacheMap.get(codeGenerator.getTypeId());
        if (cache == null) {
            cache = new ObjectIdCache();
            objectIdCacheMap.put(codeGenerator.getTypeId(), cache);
        }
        
        if (clearByDay) {
            if (System.currentTimeMillis() - currentDayMills >= ONE_DAY_MILLS + clearByDayDelayMills) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Date change, clear cache");
                }
                currentDayMills = LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
                for (ObjectIdCache objectIdCache : objectIdCacheMap.values()) {
                    objectIdCache.clear();
                }
            }
        }
        
        String id = cache.getNext();
        if (id != null) {
            if (logger.isDebugEnabled()) {
                logger.debug("Return ID:" + id);
            }
            return id;
        }

        BatchGenerateParam generateParam = BatchGenerateParam.builder().typeId(codeGenerator.getTypeId())
                .codePrefix(codeGenerator.getCodePrefix()).batchSize(batchRequestSize).build();
        ExportResponse<BatchGenerateResult> generateRes = ObjectFactorys.getDefault()
                .getObject(CodeGenerateExport.class)
                .batchGenerateUniqueCode(new ExportRequest<BatchGenerateParam>(generateParam));
        BatchGenerateResult result = ExportResponseUtil.unwrap(generateRes);
        String[] results = result.getCodes();
        id = cache.addIdsAndGetNext(results);
        if (logger.isDebugEnabled()) {
            logger.debug("Return ID:" + id);
        }
        return id;

    }

    public static String[] batchGenerateCode(String typeId, String codePrefix, int batchSize) {
        CodeGenerateParam codeGenerator = CodeGenerateParam.builder().typeId(typeId).codePrefix(codePrefix)
                .batchSize(batchSize).build();
        return batchGenerateCode(codeGenerator);
    }

    private static String[] batchGenerateCode(CodeGenerateParam codeGenerator) {
        BatchGenerateParam generateParam = BatchGenerateParam.builder().typeId(codeGenerator.getTypeId())
                .codePrefix(codeGenerator.getCodePrefix()).batchSize(codeGenerator.getBatchSize()).build();
        ExportResponse<BatchGenerateResult> generateRes = ObjectFactorys.getDefault()
                .getObject(CodeGenerateExport.class)
                .batchGenerateUniqueCode(new ExportRequest<BatchGenerateParam>(generateParam));
        BatchGenerateResult result = ExportResponseUtil.unwrap(generateRes);
        return result.getCodes();
    }

    private static class ObjectIdCache {

        LinkedList<String> cacheList = new LinkedList<>();

        public synchronized String getNext() {
            return cacheList.poll();
        }

        public synchronized String addIdsAndGetNext(String[] ids) {
            cacheList.addAll(Arrays.asList(ids));
            return cacheList.pop();
        }
        
        public synchronized void clear() {
            cacheList.clear();
        }

    }
}

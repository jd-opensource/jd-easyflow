package com.jd.easyflow.codegenerator.domain.service;

import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.codegenerator.domain.constant.CodeGeneratorConstants;
import com.jd.easyflow.codegenerator.domain.model.vo.CodeGenerateReq;
import com.jd.easyflow.codegenerator.domain.repository.SequenceRepository;
import com.jd.easyflow.common.util.AssertUtils;
import com.jd.easyflow.lock.Locker;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerateDomainService {
    
    private static final int DEFAULT_CACHE_SIZE = 1000;
    private static final int DEFAULT_NUM_LENGTH = 8;
    
    private int maxBatchSize = 10000;

    private ConcurrentHashMap<String, CodeGenerator> generatorMap = new ConcurrentHashMap<>();
    
    private Map<String, CodeGenerator> configuredGeneratorMap;
    
    @Autowired
    private SequenceRepository sequenceRepository;
    @Resource(name = CodeGeneratorConstants.BEAN_NEW_TX_TEMPLATE)
    private TransactionTemplate newTransactionTemplate;
    @Resource(name = CodeGeneratorConstants.BEAN_LOCKER)
    private Locker locker;
    
    @PostConstruct
    public void init() {
        if (configuredGeneratorMap != null) {
            for (Entry<String, CodeGenerator> entry : configuredGeneratorMap.entrySet()) {
                CodeGenerator generator = entry.getValue();
                if (generator.getCacheSize() == 0) {
                    generator.setCacheSize(DEFAULT_CACHE_SIZE);
                } else if (generator.getCacheSize() == -1) {
                    generator.setCacheSize(0);
                }
                if (generator.getKey() == null) {
                    generator.setKey(entry.getKey());
                }
                if (generator.getNumLength() == 0) {
                    generator.setNumLength(DEFAULT_NUM_LENGTH);
                }
                if (generator.getRollingType() == 0) {
                    generator.setRollingType(CodeGenerateReq.ROLLING_TYPE_DAY);
                } else if (generator.getRollingType() == -1) {
                    generator.setRollingType(CodeGenerateReq.ROLLING_TYPE_NONE);
                }
                if (generator.getStep() == 0) {
                    generator.setStep(1);
                }
                if (generator.getSequenceRepository() == null) {
                    generator.setSequenceRepository(sequenceRepository);
                }
                if (generator.getLocker() == null) {
                    generator.setLocker(locker);
                }
                if (generator.getNewTransactionTemplate() == null) {
                    generator.setNewTransactionTemplate(newTransactionTemplate);
                }
            }
            generatorMap.putAll(configuredGeneratorMap);
        }
    }
    
    
    public String next(CodeGenerateReq codeGenerateReq) {
        AssertUtils.isNotNull(codeGenerateReq, "codeGenerateReq must not be null");
        AssertUtils.isNotNull(codeGenerateReq.getTypeId(), "CodeGenerateReq typeId must not be blank");
        CodeGenerator generator = generatorMap.get(codeGenerateReq.getTypeId());
        if (generator == null) {
            generator = createCodeGenerator(codeGenerateReq);
        }
        return generator.next();
    }

    public String[] nextBatch(CodeGenerateReq codeGenerateReq) {
        AssertUtils.isNotNull(codeGenerateReq, "codeGenerateReq must not be null");
        AssertUtils.isNotNull(codeGenerateReq.getTypeId(), "CodeGenerateReq typeId must not be blank");
        AssertUtils.isTrue(codeGenerateReq.getBatchSize() > 0, "batchSize should be greater than 0");
        AssertUtils.isTrue(codeGenerateReq.getBatchSize() <= maxBatchSize, "batchSize exceed limit");
        CodeGenerator generator = generatorMap.get(codeGenerateReq.getTypeId());
        if (generator == null) {
            generator = createCodeGenerator(codeGenerateReq);
        }
        String[] result = new String[codeGenerateReq.getBatchSize()];
        for (int i = 0; i < codeGenerateReq.getBatchSize(); i++) {
            result[i] = generator.next();
        }
        return result;
    }

    private synchronized CodeGenerator createCodeGenerator(CodeGenerateReq request) {
        CodeGenerator generator = generatorMap.get(request.getTypeId());
        if (generator != null) {
            return generator;
        }
        generator = new CodeGenerator();
        generator.setCacheSize(request.getCacheSize());
        generator.setCodePrefix(request.getCodePrefix());
        generator.setKey(request.getTypeId());
        generator.setNumLength(request.getNumLength());
        generator.setRollingType(request.getRollingType());
        generator.setSeperator1(request.getSeperator1());
        generator.setSeperator2(request.getSeperator2());
        generator.setStart(request.getStart());
        generator.setStartRandomRange(request.getStartRandomRange());
        generator.setStep(request.getStep());
        generator.setStepRandomRange(request.getStepRandomRange());
        generator.setSequenceRepository(sequenceRepository);
        generator.setNewTransactionTemplate(newTransactionTemplate);
        generator.setLocker(locker);
        generatorMap.put(request.getTypeId(), generator);
        return generator;
    }

    public int getMaxBatchSize() {
        return maxBatchSize;
    }

    public void setMaxBatchSize(int maxBatchSize) {
        this.maxBatchSize = maxBatchSize;
    }

    public Map<String, CodeGenerator> getConfiguredGeneratorMap() {
        return configuredGeneratorMap;
    }

    public void setConfiguredGeneratorMap(Map<String, CodeGenerator> configuredGeneratorMap) {
        this.configuredGeneratorMap = configuredGeneratorMap;
    }

}

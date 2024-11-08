package com.jd.easyflow.codegenerator.domain.service;

import java.util.Random;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import com.jd.easyflow.codegenerator.domain.model.entity.SequenceEntity;
import com.jd.easyflow.codegenerator.domain.repository.SequenceRepository;
import com.jd.easyflow.lock.Locker;
import com.jd.easyflow.spring.PropertyUtil;

/**
 * @author liyuliang5
 *
 */
public class CodeGenerator {
    public static final Logger logger = LoggerFactory.getLogger(CodeGenerator.class);

    private static final String LOCK_BIZ_TYPE = "_sequence";
    
    private static final String EASYFLOW_CODE_PREFIX_KEY = "easyflow.codeGenerator.prefix";

    public static final int ROLLING_TYPE_NONE = 0;
    public static final int ROLLING_TYPE_DAY = 1;

    private String codePrefix;

    private String key;

    private String subKey;

    private int rollingType = 0;

    private long start;

    private int startRandomRange;

    private int step;

    private int stepRandomRange;

    private int cacheSize;

    private int currentStep;

    private volatile Long currentValue;

    private volatile Long maxCachedValue;

    private int numLength;

    private String seperator1 = "";

    private String seperator2 = "";

    private long lastSubKeyCheckTime;

    private long subKeyCheckInterval = 60L * 1000;

    private SequenceRepository sequenceRepository;

    private Locker locker;

    private TransactionTemplate newTransactionTemplate;

    private Random random = new Random();

    public CodeGenerator() {
    }


    public synchronized String next() {
        long nextNumVal = nextNum();
        String nextNumStr = StringUtils.leftPad(nextNumVal + "", numLength, '0');
        String coffeeCodePrefix = StringUtils.isBlank(PropertyUtil.get(EASYFLOW_CODE_PREFIX_KEY)) ? "" : PropertyUtil.get(EASYFLOW_CODE_PREFIX_KEY);
        return coffeeCodePrefix + (codePrefix == null ? "" : codePrefix) + seperator1 + (subKey == null ? "" : subKey + seperator2) + nextNumStr;
    }


    public synchronized long nextNum() {
        if (ROLLING_TYPE_DAY == rollingType) {
            long currentTime = System.currentTimeMillis();
            boolean needCheckDate = subKey == null || currentTime - lastSubKeyCheckTime > subKeyCheckInterval;
            if (needCheckDate) {
                String currentSubKey = FastDateFormat.getInstance("yyyyMMdd").format(currentTime);
                if (!StringUtils.equals(currentSubKey, subKey)) {
                    logger.info("subkey change, old:{} new:{}", subKey, currentSubKey);
                    subKey = currentSubKey;
                    currentValue = null;
                }
                lastSubKeyCheckTime = currentTime;
            }
        }

        currentStep = this.step + (stepRandomRange == 0 ? 0 : (int) (Math.random() * stepRandomRange));
        if (currentValue == null || currentValue + currentStep > maxCachedValue) {
            loadNextBatch();
        }
        currentValue = currentValue + currentStep;
        return currentValue;
    }

    protected synchronized void loadNextBatch() {
        logger.info("Load new batch from database, key:" + key);
        boolean result = loadIfPresent();
        if (!result) {
            insertIfAbsent();
            loadIfPresent();
        }
    }

    protected synchronized boolean loadIfPresent() {
        return newTransactionTemplate.execute(new TransactionCallback<Boolean>() {
            @Override
            public Boolean doInTransaction(TransactionStatus status) {

                SequenceEntity sequence;
                sequence = sequenceRepository.queryWithLock(key, subKey);
                if (sequence == null) {
                    return false;
                }
                Long currentValue = sequence.getSeqValue();

                boolean startFlag = currentValue == null;
                if (startFlag) {
                    currentValue = start - currentStep + (startRandomRange == 0 ? 0 : random.nextInt(startRandomRange));
                }
                long maxValue = currentValue + (cacheSize == 0 ? currentStep : cacheSize);
                logger.info("current value:{}  maxValue:{}", currentValue, maxValue);
                sequenceRepository.updateSeqValueById(sequence.getId(), maxValue);
                CodeGenerator.this.currentValue = currentValue;
                CodeGenerator.this.maxCachedValue = maxValue;
                return true;
            }

        });

    }

    protected synchronized void insertIfAbsent() {
        logger.info("Insert new record into database, key:{}, subkey:{}", key, subKey);
        newTransactionTemplate.execute(new TransactionCallback<Object>() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                return locker.doInLock(LOCK_BIZ_TYPE, key, () -> {
                    SequenceEntity sequence = sequenceRepository.queryWithLock(key, subKey);
                    if (sequence == null) {
                        logger.info("Record not exists");
                        sequence = new SequenceEntity();
                        sequence.setSeqKey(key);
                        sequence.setSeqSubKey(subKey);
                        try {
                            sequenceRepository.addSequence(sequence);
                        } catch (DuplicateKeyException e) {
                            logger.warn("Sequence duplicate," + sequence + ", Ignore error.", e);
                        }
                    }
                    return null;
                });

            }
        });
    }
    

    public String getCodePrefix() {
        return codePrefix;
    }

    public void setCodePrefix(String codePrefix) {
        this.codePrefix = codePrefix;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getSubKey() {
        return subKey;
    }

    public void setSubKey(String subKey) {
        this.subKey = subKey;
    }

    public int getRollingType() {
        return rollingType;
    }

    public void setRollingType(int rollingType) {
        this.rollingType = rollingType;
    }


    /**
     * @return the start
     */
    public long getStart() {
        return start;
    }

    /**
     * @param start the start to set
     */
    public void setStart(long start) {
        this.start = start;
    }

    public int getStartRandomRange() {
        return startRandomRange;
    }

    public void setStartRandomRange(int startRandomRange) {
        this.startRandomRange = startRandomRange;
    }

    public int getStep() {
        return step;
    }

    public void setStep(int step) {
        this.step = step;
    }

    public int getStepRandomRange() {
        return stepRandomRange;
    }

    public void setStepRandomRange(int stepRandomRange) {
        this.stepRandomRange = stepRandomRange;
    }

    public int getCacheSize() {
        return cacheSize;
    }

    public void setCacheSize(int cacheSize) {
        this.cacheSize = cacheSize;
    }

    public int getNumLength() {
        return numLength;
    }

    public void setNumLength(int numLength) {
        this.numLength = numLength;
    }

    public String getSeperator1() {
        return seperator1;
    }

    public void setSeperator1(String seperator1) {
        this.seperator1 = seperator1;
    }

    public String getSeperator2() {
        return seperator2;
    }

    public void setSeperator2(String seperator2) {
        this.seperator2 = seperator2;
    }

    public long getSubKeyCheckInterval() {
        return subKeyCheckInterval;
    }

    public void setSubKeyCheckInterval(long subKeyCheckInterval) {
        this.subKeyCheckInterval = subKeyCheckInterval;
    }

    public SequenceRepository getSequenceRepository() {
        return sequenceRepository;
    }

    public void setSequenceRepository(SequenceRepository sequenceRepository) {
        this.sequenceRepository = sequenceRepository;
    }

    public TransactionTemplate getNewTransactionTemplate() {
        return newTransactionTemplate;
    }

    public void setNewTransactionTemplate(TransactionTemplate newTransactionTemplate) {
        this.newTransactionTemplate = newTransactionTemplate;
    }

    public Locker getLocker() {
        return locker;
    }

    public void setLocker(Locker locker) {
        this.locker = locker;
    }

}

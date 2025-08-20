package com.jd.easyflow.codegenerator.infrastructure.repository;

import org.springframework.beans.factory.annotation.Autowired;

import com.jd.easyflow.codegenerator.domain.model.entity.SequenceEntity;
import com.jd.easyflow.codegenerator.domain.repository.SequenceRepository;
import com.jd.easyflow.codegenerator.infrastructure.converter.SequenceConverter;
import com.jd.easyflow.codegenerator.infrastructure.persistence.mapper.SequenceMapper;
import com.jd.easyflow.codegenerator.infrastructure.persistence.po.Sequence;
import com.jd.easyflow.common.util.AssertUtils;

/**
 * @author liyuliang5
 *
 */
public class SequenceRepositoryImpl implements SequenceRepository {


    @Autowired
    private SequenceMapper sequenceMapper;


    @Override
    public SequenceEntity queryWithLock(String key, String subKey) {
        Sequence sequence = sequenceMapper.selectForUpdate(key, subKey);
        return SequenceConverter.INSTANCE.convert(sequence);
    }

    @Override
    public int addSequence(SequenceEntity entity) {
        AssertUtils.isNotNull(entity);
        AssertUtils.isNotNull(entity.getSeqKey());
        Sequence sequence = SequenceConverter.INSTANCE.convert(entity);
        return sequenceMapper.insert(sequence);
    }

    @Override
    public int updateSeqValueById(Long id, long seqValue) {
        return sequenceMapper.updateValueByPrimaryKey(id, seqValue);
    }
}

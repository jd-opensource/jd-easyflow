package com.jd.easyflow.codegenerator.domain.repository;

import com.jd.easyflow.codegenerator.domain.model.entity.SequenceEntity;

/**
 * @author liyuliang5
 *
 */
public interface SequenceRepository {

    SequenceEntity queryWithLock(String key, String subKey);

    int addSequence(SequenceEntity entity);

    int updateSeqValueById(Long id, long seqValue);

}

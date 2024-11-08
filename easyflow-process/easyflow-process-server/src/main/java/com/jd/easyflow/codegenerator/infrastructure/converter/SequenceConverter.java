package com.jd.easyflow.codegenerator.infrastructure.converter;

import com.jd.easyflow.codegenerator.domain.model.entity.SequenceEntity;
import com.jd.easyflow.codegenerator.infrastructure.persistence.po.Sequence;

/**
 * @author liyuliang5
 *
 */
public class SequenceConverter {

    public static SequenceConverter INSTANCE = new SequenceConverter();
    
    private SequenceConverter() {
        
    }


    public SequenceEntity convert(Sequence sequence) {
        if ( sequence == null ) {
            return null;
        }

        SequenceEntity sequenceEntity = new SequenceEntity();

        sequenceEntity.setId( sequence.getId() );
        sequenceEntity.setSeqKey( sequence.getSeqKey() );
        sequenceEntity.setSeqSubKey( sequence.getSeqSubKey() );
        sequenceEntity.setSeqValue( sequence.getSeqValue() );
        sequenceEntity.setCreatedDate( sequence.getCreatedDate() );
        sequenceEntity.setModifiedDate( sequence.getModifiedDate() );

        return sequenceEntity;
    }

    public Sequence convert(SequenceEntity entity) {
        if ( entity == null ) {
            return null;
        }

        Sequence sequence = new Sequence();

        sequence.setId( entity.getId() );
        sequence.setSeqKey( entity.getSeqKey() );
        sequence.setSeqSubKey( entity.getSeqSubKey() );
        sequence.setSeqValue( entity.getSeqValue() );
        sequence.setCreatedDate( entity.getCreatedDate() );
        sequence.setModifiedDate( entity.getModifiedDate() );

        return sequence;
    }
}

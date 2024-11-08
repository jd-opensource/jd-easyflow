package com.jd.easyflow.codegenerator.infrastructure.persistence.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import com.jd.easyflow.codegenerator.infrastructure.persistence.po.Sequence;

/**
 * @author liyuliang5
 *
 */
@Mapper
public interface SequenceMapper {
    int deleteByPrimaryKey(Long id);

    int insert(Sequence record);

    int insertSelective(Sequence record);

    Sequence selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(Sequence record);

    int updateByPrimaryKey(Sequence record);
    
    Sequence selectForUpdate(@Param("key") String key, @Param("subKey") String subKey);
    
    int updateValueByPrimaryKey(@Param("id")long id, @Param("seqValue") long seqValue);
}
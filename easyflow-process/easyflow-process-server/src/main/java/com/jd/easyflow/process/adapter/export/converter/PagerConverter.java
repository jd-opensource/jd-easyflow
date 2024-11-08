package com.jd.easyflow.process.adapter.export.converter;


import com.jd.easyflow.common.adapter.export.dto.pager.PagerResult;
import com.jd.easyflow.common.dto.pager.FieldEntry;
import com.jd.easyflow.common.dto.pager.PagerCondition;
import com.jd.easyflow.common.dto.pager.SortEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author liyuliang5
 *
 */
public class PagerConverter {
    
    public static PagerConverter INSTANCE = new PagerConverter();

    public PagerCondition convert(com.jd.easyflow.common.adapter.export.dto.pager.PagerCondition condition) {
        if ( condition == null ) {
            return null;
        }

        PagerCondition pagerCondition = new PagerCondition();

        pagerCondition.setCount( condition.isCount() );
        Map<String, Object> map = condition.getExtData();
        if ( map != null ) {
            pagerCondition.setExtData( new HashMap<String, Object>( map ) );
        }
        pagerCondition.setFieldList( fieldEntryListToFieldEntryList( condition.getFieldList() ) );
        pagerCondition.setPage( condition.getPage() );
        pagerCondition.setPageIndex( condition.getPageIndex() );
        pagerCondition.setPageSize( condition.getPageSize() );
        pagerCondition.setSortList( sortEntryListToSortEntryList( condition.getSortList() ) );
        pagerCondition.setStart( condition.getStart() );
        if ( pagerCondition.getFieldMap() != null ) {
            Map<String, FieldEntry> map1 = stringFieldEntryMapToStringFieldEntryMap( condition.getFieldMap() );
            if ( map1 != null ) {
                pagerCondition.getFieldMap().putAll( map1 );
            }
        }
        if ( pagerCondition.getSortMap() != null ) {
            Map<String, SortEntry> map2 = stringSortEntryMapToStringSortEntryMap( condition.getSortMap() );
            if ( map2 != null ) {
                pagerCondition.getSortMap().putAll( map2 );
            }
        }

        return pagerCondition;
    }

    public PagerResult convert(com.jd.easyflow.common.dto.pager.PagerResult result) {
        if ( result == null ) {
            return null;
        }

        PagerResult pagerResult = new PagerResult();

        pagerResult.setCount( result.getCount() );
        List list = result.getList();
        if ( list != null ) {
            pagerResult.setList( new ArrayList( list ) );
        }
        pagerResult.setPageNum( result.getPageNum() );
        pagerResult.setPageSize( result.getPageSize() );

        return pagerResult;
    }

    protected FieldEntry fieldEntryToFieldEntry(com.jd.easyflow.common.adapter.export.dto.pager.FieldEntry fieldEntry) {
        if ( fieldEntry == null ) {
            return null;
        }

        FieldEntry fieldEntry1 = new FieldEntry();

        fieldEntry1.setName( fieldEntry.getName() );
        fieldEntry1.setValue( fieldEntry.getValue() );

        return fieldEntry1;
    }

    protected List<FieldEntry> fieldEntryListToFieldEntryList(List<com.jd.easyflow.common.adapter.export.dto.pager.FieldEntry> list) {
        if ( list == null ) {
            return null;
        }

        List<FieldEntry> list1 = new ArrayList<FieldEntry>( list.size() );
        for ( com.jd.easyflow.common.adapter.export.dto.pager.FieldEntry fieldEntry : list ) {
            list1.add( fieldEntryToFieldEntry( fieldEntry ) );
        }

        return list1;
    }

    protected SortEntry sortEntryToSortEntry(com.jd.easyflow.common.adapter.export.dto.pager.SortEntry sortEntry) {
        if ( sortEntry == null ) {
            return null;
        }

        SortEntry sortEntry1 = new SortEntry();

        sortEntry1.setKey( sortEntry.getKey() );
        sortEntry1.setSeq( sortEntry.getSeq() );
        sortEntry1.setType( sortEntry.getType() );

        return sortEntry1;
    }

    protected List<SortEntry> sortEntryListToSortEntryList(List<com.jd.easyflow.common.adapter.export.dto.pager.SortEntry> list) {
        if ( list == null ) {
            return null;
        }

        List<SortEntry> list1 = new ArrayList<SortEntry>( list.size() );
        for ( com.jd.easyflow.common.adapter.export.dto.pager.SortEntry sortEntry : list ) {
            list1.add( sortEntryToSortEntry( sortEntry ) );
        }

        return list1;
    }

    protected Map<String, FieldEntry> stringFieldEntryMapToStringFieldEntryMap(Map<String, com.jd.easyflow.common.adapter.export.dto.pager.FieldEntry> map) {
        if ( map == null ) {
            return null;
        }

        Map<String, FieldEntry> map1 = new HashMap<String, FieldEntry>( Math.max( (int) ( map.size() / .75f ) + 1, 16 ) );

        for ( java.util.Map.Entry<String, com.jd.easyflow.common.adapter.export.dto.pager.FieldEntry> entry : map.entrySet() ) {
            String key = entry.getKey();
            FieldEntry value = fieldEntryToFieldEntry( entry.getValue() );
            map1.put( key, value );
        }

        return map1;
    }

    protected Map<String, SortEntry> stringSortEntryMapToStringSortEntryMap(Map<String, com.jd.easyflow.common.adapter.export.dto.pager.SortEntry> map) {
        if ( map == null ) {
            return null;
        }

        Map<String, SortEntry> map1 = new HashMap<String, SortEntry>( Math.max( (int) ( map.size() / .75f ) + 1, 16 ) );

        for ( java.util.Map.Entry<String, com.jd.easyflow.common.adapter.export.dto.pager.SortEntry> entry : map.entrySet() ) {
            String key = entry.getKey();
            SortEntry value = sortEntryToSortEntry( entry.getValue() );
            map1.put( key, value );
        }

        return map1;
    }
}

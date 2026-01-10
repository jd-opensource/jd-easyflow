package com.jd.easyflow.processunit.domain.model.converter;

import java.util.ArrayList;
import java.util.List;

import com.jd.easyflow.processunit.adapter.export.dto.ProcessUnitCreateReq;
import com.jd.easyflow.processunit.domain.model.vo.ExecParam;
import com.jd.easyflow.processunit.domain.model.vo.SyncBeforeCallReq;
import com.jd.easyflow.processunit.domain.model.vo.BatchExecuteReq.VariableEntry;

/**
 * @author liyuliang5
 * 
 */
public class ProcessUnitExecuteConverter {

    public static ProcessUnitExecuteConverter INSTANCE = new ProcessUnitExecuteConverter();


    public ExecParam convert(SyncBeforeCallReq param) {
        if ( param == null ) {
            return null;
        }

        ExecParam execParam = new ExecParam();

        execParam.setBizNo( param.getBizNo() );
        execParam.setProductCode( param.getProductCode() );
        execParam.setRequestContent( param.getRequestContent() );
        execParam.setRequestNo( param.getRequestNo() );
        execParam.setUnitCode( param.getUnitCode() );
        execParam.setVersion( param.getVersion() );
        execParam.setClientInfo(param.getClientInfo());
        return execParam;
    }

    public ExecParam convert(ProcessUnitCreateReq param) {
        if ( param == null ) {
            return null;
        }

        ExecParam execParam = new ExecParam();

        execParam.setBizNo( param.getBizNo() );
        execParam.setProductCode( param.getProductCode() );
        execParam.setRequestContent( param.getRequestContent() );
        execParam.setRequestNo( param.getRequestNo() );
        execParam.setUnitCode( param.getUnitCode() );
        execParam.setAutoRunFlag(param.getAutoRunFlag());
        execParam.setNextAutoRunTime(param.getNextAutoRunTime());
        execParam.setVariables(param.getVariables());
        execParam.setClientInfo(param.getClientInfo());
        return execParam;
    }

    public List<com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry> convertVariableList(List<VariableEntry> list) {
        if ( list == null ) {
            return null;
        }

        List<com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry> list1 = new ArrayList<com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry>( list.size() );
        for ( VariableEntry variableEntry : list ) {
            list1.add( variableEntryToVariableEntry( variableEntry ) );
        }

        return list1;
    }

    protected com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry variableEntryToVariableEntry(VariableEntry variableEntry) {
        if ( variableEntry == null ) {
            return null;
        }

        com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry variableEntry1 = new com.jd.easyflow.processunit.domain.model.vo.QueryAsyncInstanceVO.VariableEntry();

        variableEntry1.setName( variableEntry.getName() );
        variableEntry1.setOperator( variableEntry.getOperator() );
        variableEntry1.setValue( variableEntry.getValue() );

        return variableEntry1;
    }
}

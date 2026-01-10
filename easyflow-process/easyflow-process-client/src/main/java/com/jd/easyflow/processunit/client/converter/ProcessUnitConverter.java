package com.jd.easyflow.processunit.client.converter;

import com.jd.easyflow.processunit.adapter.export.dto.ExecPolicyDTO;
import com.jd.easyflow.processunit.client.bean.ExecPolicy;

/**
 * @author liyuliang5
 *
 */
public class ProcessUnitConverter {

    public static ProcessUnitConverter INSTANCE = new ProcessUnitConverter();
    

    public ExecPolicy convert(ExecPolicyDTO execPolicy) {
        if ( execPolicy == null ) {
            return null;
        }

        ExecPolicy execPolicy1 = new ExecPolicy();

        execPolicy1.setPolicyData( execPolicy.getPolicyData() );
        execPolicy1.setPolicyType( execPolicy.getPolicyType() );
        execPolicy1.setRequestContent( execPolicy.getRequestContent() );
        execPolicy1.setResponseContent( execPolicy.getResponseContent() );
        execPolicy1.setResult( execPolicy.getResult() );

        return execPolicy1;
    }

}


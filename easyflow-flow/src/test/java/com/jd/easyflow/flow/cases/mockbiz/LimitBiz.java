package com.jd.easyflow.flow.cases.mockbiz;

import java.math.BigDecimal;

/**
 * 
 * @author liyuliang5
 *
 */
public class LimitBiz {

    public boolean judgeLimit(BigDecimal amount) {
        return amount.compareTo(new BigDecimal(100)) < 0;
    }
}

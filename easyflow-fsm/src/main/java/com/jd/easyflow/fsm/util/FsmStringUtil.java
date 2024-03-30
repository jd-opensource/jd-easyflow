package com.jd.easyflow.fsm.util;

/**
 * 
 * @author liyuliang5
 */
public class FsmStringUtil {
    
    public static boolean isNotEmpty(final CharSequence cs) {
        return ! isEmpty(cs);
    }

    public static boolean isEmpty(final CharSequence cs) {
        return cs == null || cs.length() == 0;
    }
    
    public static String repeat(final char ch, final int repeat) {
        if (repeat <= 0) {
            return "";
        }
        final char[] buf = new char[repeat];
        for (int i = repeat - 1; i >= 0; i--) {
            buf[i] = ch;
        }
        return new String(buf);
    }
}

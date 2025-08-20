package com.jd.easyflow.common.util;

import java.util.Locale;

import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

import com.jd.easyflow.objects.factory.ObjectFactorys;

/**
 * 
 * @author liyuliang5
 */
public class MessageUtil {

    private static MessageSource messageSource;

    public static String getMessage(String code) {
        return getMessageSource().getMessage(code, null, null);
    }
    
    public static String getMessage(String code, Object[] args) {
        return getMessageSource().getMessage(code, args, LocaleContextHolder.getLocale());
    }

    private static MessageSource getMessageSource() {
        if (messageSource == null) {
            messageSource = ObjectFactorys.getDefault().getObject(MessageSource.class);
        }
        return messageSource;
    }
    
    public static String locale() {
        Locale locale = LocaleContextHolder.getLocale();
        if (locale == null) {
            return null;
        }
        return locale.toString();
    }
    
}

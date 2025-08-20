package com.jd.easyflow.properties;

import com.jd.easyflow.objects.factory.ObjectFactorys;

/**
 * @author liyuliang5
 */
public class PropertyUtil {
    
    private static PropertiesAccessor propertiesAccessor;

    public static String get(String key) {
        return getPropertiesAccessor().getProperty(key);
    }

    public static PropertiesAccessor getPropertiesAccessor() {
        if (propertiesAccessor == null) {
            propertiesAccessor = ObjectFactorys.getDefault().getObject(PropertiesAccessor.class);
        }
        return propertiesAccessor;
    }

    public static void setPropertiesAccessor(PropertiesAccessor propertiesAccessor) {
        PropertyUtil.propertiesAccessor = propertiesAccessor;
    }
    
}

package com.jd.easyflow.properties.spring;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;
import org.springframework.core.env.SystemEnvironmentPropertySource;

import com.jd.easyflow.properties.PropertiesAccessor;

/**
 * @author liyuliang5
 *
 */
public class SpringEnvPropertiesAccessor implements PropertiesAccessor {
    
    private static final Logger log = LoggerFactory.getLogger(SpringEnvPropertiesAccessor.class);

    

    private Environment env;

    @Autowired
    public void setEnvironment(Environment env) {
      this.env = env;
      StringBuilder builder = new StringBuilder("\nConfig sources:");
      // print all
      Set<String> keys = new TreeSet<>();
      for (Iterator it = ((ConfigurableEnvironment) env).getPropertySources().iterator(); it
          .hasNext();) {
        PropertySource propertySource = (PropertySource) it.next();
        builder.append(propertySource.getName() + "\n");
        if (propertySource instanceof EnumerablePropertySource) {
          if (propertySource instanceof SystemEnvironmentPropertySource
              || "systemProperties".equals(propertySource.getName())) {
            continue;
          }
          for (String key : ((EnumerablePropertySource) propertySource).getPropertyNames()) {
            keys.add(key);
          }
        }
      }
      builder.append("\nConfig info:\n");
      for (String key : keys) {
        String value = env.getProperty(key);
        builder.append(key + "=" + value + "\n");
      }
      log.info(builder.toString());

    }

    @Override
    public String getProperty(String key) {
        return env.getProperty(key);
    }

}

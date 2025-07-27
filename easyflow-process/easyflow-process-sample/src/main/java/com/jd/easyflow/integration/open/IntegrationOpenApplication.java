package com.jd.easyflow.integration.open;

import java.time.LocalDateTime;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.DispatcherServlet;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;

import com.jd.easyflow.spel.SpelHelper;

/**
 * @author liyuliang5
 *
 */
@SpringBootApplication
@ImportResource("classpath:spring/open/easyflow-spring-open-all.xml")
@ComponentScan(basePackages = {"com.jd.easyflow.integration.all.**", "com.jd.easyflow.admin.**"})
@EnableScheduling
public class IntegrationOpenApplication implements WebMvcConfigurer, CommandLineRunner {
    
    private static final Logger log = LoggerFactory.getLogger(IntegrationOpenApplication.class);


    private static final String CLIENT_NAME = "IntegrationOpenApplication";
    
    @Autowired
    private ApplicationContext applicationContext;
    
    /**
     *
     * @param args
     */
    public static void main(String[] args) {
        //Locale.setDefault(Locale.US);
        log.info(CLIENT_NAME + "start boot,Start time:" + LocalDateTime.now());
        new SpringApplicationBuilder(IntegrationOpenApplication.class)
        .properties("spring.config.location=classpath:application-open-all.yml").run(args);
        log.info(CLIENT_NAME + "Start success,End time:" + LocalDateTime.now());
    }
    
    @Override
    public void run(String... args) throws Exception {
       log.info("Start");
       SpelHelper.setApplicationContext(applicationContext);
    }
    
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        LocaleChangeInterceptor localeChangeInterceptor = new LocaleChangeInterceptor();
        registry.addInterceptor(localeChangeInterceptor);
    }
    
    @Bean(DispatcherServlet.LOCALE_RESOLVER_BEAN_NAME)
    public SessionLocaleResolver SessionLocaleResolver() {
        return new SessionLocaleResolver();
    }

}

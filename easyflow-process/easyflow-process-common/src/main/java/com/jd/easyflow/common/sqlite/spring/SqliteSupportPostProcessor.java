package com.jd.easyflow.common.sqlite.spring;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.ManagedArray;
import org.springframework.core.env.Environment;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * Unified SQLite support for Spring + MyBatis.
 * <p>
 * Implements both {@link BeanFactoryPostProcessor} and {@link BeanPostProcessor}
 * to conditionally apply SQLite compatibility adjustments only when SQLite is detected.
 * <p>
 * <b>BeanFactoryPostProcessor phase</b> (before bean instantiation):
 * <ul>
 *   <li>Detects SQLite from {@code db.jdbcUrl} property</li>
 *   <li>If SQLite: registers {@link SqliteDateTypeHandler} into all {@code SqlSessionFactoryBean} definitions</li>
 * </ul>
 * <p>
 * <b>BeanPostProcessor phase</b> (after bean instantiation):
 * <ul>
 *   <li>If SQLite: downgrades all {@link TransactionTemplate} with REQUIRES_NEW to REQUIRED</li>
 * </ul>
 * <p>
 * For MySQL environments, neither phase takes effect — zero overhead, zero behavior change.
 * <p>
 * Register once in Spring context (e.g. spring-datasource.xml):
 * <pre>
 * &lt;bean class="com.jd.easyflow.common.sqlite.spring.SqliteSupportPostProcessor"/&gt;
 * </pre>
 */
public class SqliteSupportPostProcessor implements BeanFactoryPostProcessor, BeanPostProcessor {

    private static final Logger logger = LoggerFactory.getLogger(SqliteSupportPostProcessor.class);

    private volatile Boolean sqlite;

    @Override
    public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
        Environment env = beanFactory.getBean(Environment.class);
        String jdbcUrl = env.getProperty("db.jdbcUrl", "");
        sqlite = jdbcUrl.contains("sqlite");

        if (!sqlite) {
            logger.info("Non-SQLite database detected (jdbcUrl={}), SQLite support skipped", jdbcUrl);
            return;
        }

        logger.info("SQLite database detected, applying compatibility adjustments");

        // Register SqliteDateTypeHandler into all SqlSessionFactoryBean definitions
        String[] factoryNames = beanFactory.getBeanNamesForType(
                org.mybatis.spring.SqlSessionFactoryBean.class);
        for (String name : factoryNames) {
            String beanName = name.startsWith("&") ? name.substring(1) : name;
            BeanDefinition bd = beanFactory.getBeanDefinition(beanName);
            ManagedArray typeHandlers = new ManagedArray(
                    org.apache.ibatis.type.TypeHandler.class.getName(), 1);
            typeHandlers.add(BeanDefinitionBuilder
                    .genericBeanDefinition(SqliteDateTypeHandler.class)
                    .getBeanDefinition());
            bd.getPropertyValues().add("typeHandlers", typeHandlers);
            logger.info("Configured SqlSessionFactoryBean [{}]: SqliteDateTypeHandler", beanName);
        }
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
        if (sqlite == null || !sqlite) {
            return bean;
        }
        if (bean instanceof TransactionTemplate) {
            TransactionTemplate tt = (TransactionTemplate) bean;
            if (tt.getPropagationBehavior() == TransactionDefinition.PROPAGATION_REQUIRES_NEW) {
                tt.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);
                logger.info("SQLite: TransactionTemplate [{}] changed from REQUIRES_NEW to REQUIRED", beanName);
            }
        }
        return bean;
    }

    /**
     * Inner TypeHandler: only registered when SQLite is detected.
     * Handles TEXT columns containing millisecond timestamps or datetime strings.
     */
    @MappedTypes(Date.class)
    @MappedJdbcTypes(JdbcType.TIMESTAMP)
    public static class SqliteDateTypeHandler extends BaseTypeHandler<Date> {
        private static final String[] DATETIME_PATTERNS = {
            "yyyy-MM-dd HH:mm:ss",
            "yyyy-MM-dd'T'HH:mm:ss",
            "yyyy-MM-dd HH:mm:ss.SSS",
            "yyyy-MM-dd'T'HH:mm:ss.SSS"
        };

        @Override
        public void setNonNullParameter(PreparedStatement ps, int i, Date parameter, JdbcType jdbcType)
                throws SQLException {
            ps.setTimestamp(i, new Timestamp(parameter.getTime()));
        }

        @Override
        public Date getNullableResult(ResultSet rs, String columnName) throws SQLException {
            return parseDate(rs.getString(columnName));
        }

        @Override
        public Date getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
            return parseDate(rs.getString(columnIndex));
        }

        @Override
        public Date getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
            return parseDate(cs.getString(columnIndex));
        }

        private Date parseDate(String value) {
            if (value == null || value.isEmpty()) {
                return null;
            }
            try {
                return new Date(Long.parseLong(value));
            } catch (NumberFormatException ignored) {
            }
            for (String pattern : DATETIME_PATTERNS) {
                try {
                    return new SimpleDateFormat(pattern).parse(value);
                } catch (ParseException ignored) {
                }
            }
            return null;
        }
    }

}

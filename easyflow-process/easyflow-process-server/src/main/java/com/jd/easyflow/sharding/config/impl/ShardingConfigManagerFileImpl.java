package com.jd.easyflow.sharding.config.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

import com.jd.easyflow.common.exception.EasyFlowException;
import com.jd.easyflow.sharding.config.ShardInfo;
import com.jd.easyflow.sharding.config.ShardingConfig;
import com.jd.easyflow.utils.json.JSON;

/**
 * @author liyuliang5
 */
public class ShardingConfigManagerFileImpl extends ShardingConfigManagerImpl {
    
        protected String shardingConfigPath;

        public void init() {
            if (shardingConfigPath != null && !shardingConfigPath.isEmpty()) {
                try {
                    String shardingConfigStr = toString(this.getClass().getResourceAsStream(shardingConfigPath));
                    ShardingConfig shardingConfig = JSON.parseObject(shardingConfigStr, ShardingConfig.class);
                    Map<String, ShardInfo> map = new LinkedHashMap<String, ShardInfo>();
                    if (shardingConfig.getShardList() != null) {
                        shardingConfig.getShardList().forEach(info -> {
                            map.put(info.getShard(), info);
                        });
                    }
                    shardingConfig.setShardMap(map);
                    this.shardingConfig = shardingConfig;
                } catch (IOException e) {
                    throw new EasyFlowException("sharding config read exception, path:" + shardingConfigPath, e);
                }
            }
        }

        private static String toString(InputStream inputStream) throws IOException {
            ByteArrayOutputStream result = new ByteArrayOutputStream();
            byte[] buffer = new byte[1024];
            int length;
            while ((length = inputStream.read(buffer)) != -1) {
                result.write(buffer, 0, length);
            }
            String str = result.toString(StandardCharsets.UTF_8.name());
            return str;
        }

        public String getShardingConfigPath() {
            return shardingConfigPath;
        }

        public void setShardingConfigPath(String shardingConfigPath) {
            this.shardingConfigPath = shardingConfigPath;
        }

        

}

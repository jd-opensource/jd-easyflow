package com.jd.easyflow.net;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author liyuliang5
 *
 */
public class NetUtils {

    private static final Logger log = LoggerFactory.getLogger(NetUtils.class);

    private static final String SYSTEM_PROPERTY_KEY = "easyflow.host.address";

    private static final String SYSTEM_ENV_KEY = "EASYFLOW_HOST_ADDRESS";

    public static final String ANYHOST = "0.0.0.0";
    private static final Pattern LOCAL_IP_PATTERN = Pattern.compile("127(\\.\\d{1,3}){3}$");
    public static final Pattern IPV4_PATTERN = Pattern
            .compile("^(25[0-5]|2[0-4]\\d|[0-1]?\\d?\\d)(\\.(25[0-5]|2[0-4]\\d|[0-1]?\\d?\\d)){3}$");
    
    private static String sysIp;

    static {
        sysIp = NetUtils.getLocalHost();
    }

    public static String getSysIp() {
        return sysIp;
    }

    public static boolean isLocalHost(String host) {
        return isNotEmpty(host)
                && (LOCAL_IP_PATTERN.matcher(host).matches() || "localhost".equalsIgnoreCase(host));
    }

    public static boolean isAnyHost(String host) {
        return "0.0.0.0".equals(host);
    }

    public static boolean isIPv4Host(String host) {
        return isNotEmpty(host) && IPV4_PATTERN.matcher(host).matches();
    }

    private static boolean isValidAddress(InetAddress address) {
        if (address != null && !address.isLoopbackAddress()) {
            String name = address.getHostAddress();
            return name != null && !isAnyHost(name) && !isLocalHost(name) && isIPv4Host(name);
        } else {
            return false;
        }
    }

    public static String getLocalHost() {
        List<String> ipList = getLocalIpList();
        String sysIp = getLocalIpFromSystemProperty(ipList);
        if (isNotEmpty(sysIp)) {
            return sysIp;
        }
        sysIp = getLocalIpFromSystemEnv(ipList);
        if (isNotEmpty(sysIp)) {
            return sysIp;
        }
        sysIp = getLocalIpFromLocalHost();
        if (isNotEmpty(sysIp)) {
            return sysIp;
        }
        if (ipList.size() > 0) {
            log.info("local machine IP:" + ipList.get(0));
            return ipList.get(0);
        } else {
            log.warn("local machine ip is empty");
            return "";
        }
    }

    /**
     * 
     * @return
     */
    private static List<String> getLocalIpList() {
        List<String> ipList = new ArrayList<>();
        Enumeration<NetworkInterface> netInterfaces = null;
        try {
            InetAddress ip = null;
            netInterfaces = NetworkInterface.getNetworkInterfaces();
            if (netInterfaces != null) {
                while (netInterfaces.hasMoreElements()) {
                    NetworkInterface ni = netInterfaces.nextElement();
                    Enumeration<InetAddress> ips = ni.getInetAddresses();
                    while (ips.hasMoreElements()) {
                        ip = ips.nextElement();
                        if (isValidAddress(ip)) {
                            ipList.add(ip.getHostAddress());
                        }
                    }
                }

            }
            if (log.isDebugEnabled()) {
                log.debug("Local machine ip list:" + ipList);
            }
        } catch (SocketException e) {
            log.error("Get ip of local machine exception", e);
        }
        return ipList;
    }

    /**
     * 
     * @param localIpList
     * @return
     */
    private static String getLocalIpFromSystemProperty(List<String> localIpList) {
        String ipInSystemProperty = System.getProperty(SYSTEM_PROPERTY_KEY);
        if (isNotEmpty(ipInSystemProperty)) {
            if (localIpList.contains(ipInSystemProperty)) {
                log.info("System config local IP:" + ipInSystemProperty);
                return ipInSystemProperty;
            } else {
                throw new IllegalArgumentException("System config IP" + ipInSystemProperty + " not in ip list, ipList:" + localIpList);
            }
        }
        return null;
    }

    /**
     * 
     * @param localIpList
     * @return
     */
    private static String getLocalIpFromSystemEnv(List<String> localIpList) {
        String ipInEnv = System.getenv(SYSTEM_ENV_KEY);
        if (isNotEmpty(ipInEnv)) {
            if (localIpList.contains(ipInEnv)) {
                log.info("env local IP:" + ipInEnv);
                return ipInEnv;
            } else {
                throw new IllegalArgumentException("env local IP" + ipInEnv + " not in ip list, ipList:" + localIpList);
            }
        }
        return null;
    }

    /**
     * 
     * @param localIpList
     * @return
     */
    private static String getLocalIpFromLocalHost() {
        InetAddress localAddress = null;
        try {
            localAddress = InetAddress.getLocalHost();
            if (isValidAddress(localAddress)) {
                log.info("LocalHost IP is:" + localAddress.getHostAddress());
                return localAddress.getHostAddress();
            }
            return null;
        } catch (Throwable t) {
            log.warn("Get local ip exception: " + t.getMessage(), t);
            return null;
        }
    }
    
    private static boolean isNotEmpty(String cs) {
        return cs != null && cs.length() > 0;
    }
}
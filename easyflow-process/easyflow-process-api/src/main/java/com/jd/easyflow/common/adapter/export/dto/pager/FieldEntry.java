package com.jd.easyflow.common.adapter.export.dto.pager;

import java.io.Serializable;

/**
 * @author liyuliang5
 *
 */
public class FieldEntry implements Serializable {

  private static final long serialVersionUID = 7553262335974337343L;

  private String name;

  private Object value;

  public FieldEntry() {

  }

  public FieldEntry(String name, Object value) {
    this.name = name;
    this.value = value;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Object getValue() {
    return value;
  }
  
  public String stringValue() {
    return (String) value;
  }

  public void setValue(Object value) {
    this.value = value;
  }

@Override
public String toString() {
    return "FieldEntry [name=" + name + ", value=" + value + "]";
}
  
  

}

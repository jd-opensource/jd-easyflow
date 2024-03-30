package com.jd.easyflow.flow.util;

/**
 * 
 * @author liyuliang5
 */
public class Triple<L, M, R> {

    private L left;
    
    private M middle;

    private R right;

    private Triple(L left, M middle, R right) {
        this.left = left;
        this.middle = middle;
        this.right = right;
    }

    public static <L, M, R> Triple<L, M, R> of(L left, M middle, R right) {
        return new Triple<L, M, R>(left, middle, right);
    }

    public L getLeft() {
        return left;
    }

    public void setLeft(L left) {
        this.left = left;
    }
    
    public M getMiddle() {
        return middle;
    }

    public void setMiddle(M middle) {
        this.middle = middle;
    }

    public R getRight() {
        return right;
    }

    public void setRight(R right) {
        this.right = right;
    }

    @Override
    public String toString() {
        return "Triple [left=" + left + ", middle=" + middle + ", right=" + right + "]";
    }
    
}

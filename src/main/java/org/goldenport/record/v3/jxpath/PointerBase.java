package org.goldenport.record.v3.jxpath;

import org.apache.commons.jxpath.*;

/*
 * @since   Nov. 29, 2019
 * @version Nov. 30, 2019
 * @author  ASAMI, Tomoharu
 */
public abstract class PointerBase implements Pointer {
    @Override
    public Object clone() {
        return this;
    }

    @Override
    public int compareTo(Object p) {
        if (p == this) {
            return 0;
        }
        if (p instanceof PointerBase) {
            return compare_To((PointerBase)p);
        } else {
            throw new IllegalArgumentException("Not Pointer: " + p);
        }
    }

    public abstract int compare_To(PointerBase p);
}

package com.sxyangsuper.exunifier.common.asserts;

import com.sxyangsuper.exunifier.common.BaseException;
import com.sxyangsuper.exunifier.common.IExceptionSupplier;
import org.jetbrains.annotations.Contract;

public interface IThrowAsserts<E extends BaseException> extends IExceptionSupplier<E> {

    @Contract("_->fail")
    default void throwE(final Object... args) {
        throw newE(args);
    }

    @Contract("_,_->fail")
    default void throwEWithCause(final Throwable cause, final Object... args) {
        throw newEWithCause(cause, args);
    }

    @Contract("_,_->fail")
    default void throwEWithData(final Object data, final Object... args) {
        throw newEWithData(data, args);
    }

    @Contract("_,_,_->fail")
    default void throwEWithCauseAndData(
        final Throwable cause,
        final Object data,
        final Object... args
    ) {
        throw newEWithCauseAndData(cause, data, args);
    }
}

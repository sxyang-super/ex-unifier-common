package com.sxyangsuper.exunifier.common;

import java.util.function.Supplier;

public interface IExceptionSupplier<E extends BaseException> {
    E newE(Object... args);

    E newEWithCause(Throwable cause, Object... args);

    E newEWithData(Object data, Object... args);

    E newEWithCauseAndData(Throwable cause, Object data, Object... args);

    default Supplier<BaseException> newESupplier(Object... args) {
        return () -> newE(args);
    }

    default Supplier<BaseException> newEWithCauseSupplier(Throwable cause, Object... args) {
        return () -> newEWithCause(cause, args);
    }

    default Supplier<BaseException> newEWithDataSupplier(Object data, Object... args) {
        return () -> newEWithData(data, args);
    }

    default Supplier<BaseException> newEWithCauseAndDataSupplier(Throwable cause, Object data, Object... args) {
        return () -> newEWithCauseAndData(cause, data, args);
    }
}

package com.sxyangsuper.exunifier.common;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import org.jetbrains.annotations.Contract;

import java.util.Optional;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import static com.sxyangsuper.exunifier.common.AssertUtil.doesNotMatch;
import static com.sxyangsuper.exunifier.common.AssertUtil.isNotCharSequenceOrBlank;
import static com.sxyangsuper.exunifier.common.AssertUtil.isNotCharSequenceOrNotBlank;

public interface IAsserts<E extends BaseException> extends IExceptionSupplier<E> {

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

    @Contract("null,_->fail; !null,_->!null")
    default <T> T assertNotNull(final T target, final Object... args) {
        return Optional.ofNullable(target).orElseThrow(() -> newE(args));
    }

    @Contract("null,_,_->fail; !null,_,_->!null")
    default <T> T assertNotNullWithData(
        final T target,
        final Object data,
        final Object... args
    ) {
        return Optional.ofNullable(target).orElseThrow(() -> newEWithData(data, args));
    }

    @Contract("null,_,_->fail; !null,_,_->!null")
    default <T> T assertNotNullWithDataSupplier(
        final T target,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        return Optional.ofNullable(target).orElseThrow(() -> newEWithData(dataSupplier.get(), args));
    }

    // assert null

    @Contract("!null,_->fail")
    default void assertNull(final Object target, final Object... args) {
        Optional.ofNullable(target).ifPresent(nonNullTarget -> {
            throw newE(args);
        });
    }

    @Contract("!null,_,_->fail")
    default void assertNullWithData(
        final Object target,
        final Object data,
        final Object... args
    ) {
        Optional.ofNullable(target).ifPresent(nonNullTarget -> {
            throw newEWithData(data, args);
        });
    }

    // assert true

    @Contract("false,_->fail")
    default void assertTrue(final Boolean target, final Object... args) {
        if (!Boolean.TRUE.equals(target)) {
            throw newE(args);
        }
    }

    @Contract("false,_,_->fail")
    default void assertTrueWithData(
        final Boolean target,
        final Object data,
        final Object... args
    ) {
        if (!Boolean.TRUE.equals(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert false

    @Contract("true,_->fail")
    default void assertFalse(final Boolean target, final Object... args) {
        if (!Boolean.FALSE.equals(target)) {
            throw newE(args);
        }
    }

    @Contract("true,_,_->fail")
    default void assertFalseWithData(
        final Boolean target,
        final Object data,
        final Object... args
    ) {
        if (!Boolean.FALSE.equals(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert equal

    default void assertEqual(
        final Object source,
        final Object target,
        final Object... args
    ) {
        if (ObjectUtil.notEqual(source, target)) {
            throw newE(args);
        }
    }

    default void assertEqualWithData(
        final Object source,
        final Object target,
        final Object data,
        final Object... args
    ) {
        if (ObjectUtil.notEqual(source, target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not equal

    default void assertNotEqual(
        final Object source,
        final Object target,
        final Object... args
    ) {
        if (ObjectUtil.equal(source, target)) {
            throw newE(args);
        }
    }

    default void assertNotEqualWithData(
        final Object source,
        final Object target,
        final Object data,
        final Object... args
    ) {
        if (ObjectUtil.equal(source, target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not blank

    @Contract("null,_-> fail;!null,_->!null")
    default <T extends CharSequence> T assertNotBlank(final Object target, final Object... args) {
        if (isNotCharSequenceOrBlank(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_-> fail;!null,_,_->!null")
    default <T extends CharSequence> T assertNotBlankWithData(
        final Object target,
        final Object data,
        final Object... args
    ) {
        if (isNotCharSequenceOrBlank(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_-> fail;!null,_,_->!null")
    default <T extends CharSequence> T assertNotBlankWithDataSupplier(
        final Object target,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (isNotCharSequenceOrBlank(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert blank

    default <T extends CharSequence> T assertBlank(final Object target, final Object... args) {
        if (isNotCharSequenceOrNotBlank(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    default <T extends CharSequence> T assertBlankWithData(
        final Object target,
        final Object data,
        final Object... args
    ) {
        if (isNotCharSequenceOrNotBlank(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    default <T extends CharSequence> T assertBlankWithDataSupplier(
        final Object target,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (isNotCharSequenceOrNotBlank(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert matches

    @Contract("null,_,_->fail;_,null,_->fail;!null,_,_->!null")
    default <T extends CharSequence> T assertMatches(
        final CharSequence target,
        final Pattern pattern,
        final Object... args
    ) {
        if (doesNotMatch(pattern, target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_,_->fail;_,null,_,_->fail;!null,_,_,_->!null")
    default <T extends CharSequence> T assertMatchesWithData(
        final CharSequence target,
        final Pattern pattern,
        final Object data,
        final Object... args
    ) {
        if (doesNotMatch(pattern, target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_,_->fail;_,null,_,_->fail;!null,_,_,_->!null")
    default <T extends CharSequence> T assertMatchesWithDataSupplier(
        final CharSequence target,
        final Pattern pattern,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (doesNotMatch(pattern, target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert not matches

    @Contract("!null,_,_->!null")
    default <T extends CharSequence> T assertNotMatches(
        final CharSequence target,
        final Pattern pattern,
        final Object... args
    ) {
        if (!doesNotMatch(pattern, target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("!null,_,_,_->!null")
    default <T extends CharSequence> T assertNotMatchesWithData(
        final CharSequence target,
        final Pattern pattern,
        final Object data,
        final Object... args
    ) {
        if (!doesNotMatch(pattern, target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("!null,_,_,_->!null")
    default <T extends CharSequence> T assertNotMatchesWithDataSupplier(
        final CharSequence target,
        final Pattern pattern,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (!doesNotMatch(pattern, target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert not empty

    @Contract("null,_->fail;!null,_->!null")
    default <T extends Iterable<?>> T assertNotEmpty(final Iterable<?> target, final Object... args) {
        if (CollectionUtil.isEmpty(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T extends Iterable<?>> T assertNotEmptyWithData(
        final Iterable<?> target,
        final Object data,
        final Object... args
    ) {
        if (CollectionUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T extends Iterable<?>> T assertNotEmptyWithDataSupplier(
        final Iterable<?> target,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (CollectionUtil.isEmpty(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert empty

    default void assertEmpty(final Iterable<?> target, final Object... args) {
        if (CollectionUtil.isNotEmpty(target)) {
            throw newE(args);
        }
    }

    default void assertEmptyWithData(
        final Iterable<?> target,
        final Object data,
        final Object... args
    ) {
        if (CollectionUtil.isNotEmpty(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not empty

    @Contract("null,_->fail;!null,_->!null")
    default <T> T[] assertNotEmpty(final Object[] target, final Object... args) {
        if (ArrayUtil.isEmpty(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T> T[] assertNotEmptyWithData(
        final Object[] target,
        final Object data,
        final Object... args
    ) {
        if (ArrayUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T> T[] assertNotEmptyWithDataSupplier(
        final Object[] target,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        if (ArrayUtil.isEmpty(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    // assert supply successfully

    @Contract("null,_->fail")
    default <T> T assertSupplySuccessfully(final Supplier<T> supplier, final Object... args) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw newEWithCause(e, args);
        }
    }

    @Contract("null,_,_->fail")
    default <T> T assertSupplySuccessfullyWithData(
        final Supplier<T> supplier,
        final Object data,
        final Object... args
    ) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw newEWithCauseAndData(e, data, args);
        }
    }

    @Contract("null,_,_->fail")
    default <T> T assertSupplySuccessfullyWithDataSupplier(
        final Supplier<T> supplier,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        try {
            return supplier.get();
        } catch (Exception e) {
            throw newEWithCauseAndData(e, dataSupplier.get(), args);
        }
    }

    // assert execute successfully

    default void assertExecuteSuccessfully(final Executable executable, final Object... args) {
        try {
            executable.execute();
        } catch (Exception e) {
            throw newEWithCause(e, args);
        }
    }

    default void assertExecuteSuccessfullyWithData(
        final Executable executable,
        final Object data,
        final Object... args
    ) {
        try {
            executable.execute();
        } catch (Exception e) {
            throw newEWithCauseAndData(e, data, args);
        }
    }

    default void assertExecuteSuccessfullyWithDataSupplier(
        final Executable executable,
        final Supplier<Object> dataSupplier,
        final Object... args
    ) {
        try {
            executable.execute();
        } catch (Exception e) {
            throw newEWithCauseAndData(e, dataSupplier.get(), args);
        }
    }
}

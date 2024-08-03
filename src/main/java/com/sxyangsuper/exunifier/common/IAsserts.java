package com.sxyangsuper.exunifier.common;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import org.jetbrains.annotations.Contract;

import java.util.Optional;
import java.util.function.Supplier;
import java.util.regex.Pattern;

public interface IAsserts<E extends BaseException> extends IExceptionSupplier<E> {

    @Contract("_->fail")
    default void throwE(Object... args) {
        throw newE(args);
    }

    @Contract("_,_->fail")
    default void throwEWithCause(Throwable cause, Object... args) {
        throw newEWithCause(cause, args);
    }

    @Contract("_,_->fail")
    default void throwEWithData(Object data, Object... args) {
        throw newEWithData(data, args);
    }

    @Contract("_,_,_->fail")
    default void throwEWithCauseAndData(
        Throwable cause,
        Object data,
        Object... args
    ) {
        throw newEWithCauseAndData(cause, data, args);
    }

    // assert not null

    @Contract("null,_->fail")
    default void assertNotNull(Object target, Object... args) {
        Optional.ofNullable(target).orElseThrow(() -> newE(args));
    }

    @Contract("null,_,_->fail")
    default void assertNotNullWithData(
        Object target,
        Object data,
        Object... args
    ) {
        Optional.ofNullable(target).orElseThrow(() -> newEWithData(data, args));
    }

    @Contract("null,_->fail; !null,_->!null")
    default <T> T assertNotNullAndReturn(T target, Object... args) {
        return Optional.ofNullable(target).orElseThrow(() -> newE(args));
    }

    @Contract("null,_,_->fail; !null,_,_->!null")
    default <T> T assertNotNullWithDataAndReturn(
        T target,
        Object data,
        Object... args
    ) {
        return Optional.ofNullable(target).orElseThrow(() -> newEWithData(data, args));
    }

    @Contract("null,_,_->fail; !null,_,_->!null")
    default <T> T assertNotNullWithDataSupplierAndReturn(
        T target,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        return Optional.ofNullable(target).orElseThrow(() -> newEWithData(dataSupplier.get(), args));
    }

    // assert null

    @Contract("!null,_->fail")
    default void assertNull(Object target, Object... args) {
        Optional.ofNullable(target).ifPresent(nonNullTarget -> {
            throw newE(args);
        });
    }

    @Contract("!null,_,_->fail")
    default void assertNullWithData(
        Object target,
        Object data,
        Object... args
    ) {
        Optional.ofNullable(target).ifPresent(nonNullTarget -> {
            throw newEWithData(data, args);
        });
    }

    // assert true

    @Contract("false,_->fail")
    default void assertTrue(Boolean target, Object... args) {
        if (!Boolean.TRUE.equals(target)) {
            throw newE(args);
        }
    }

    @Contract("false,_,_->fail")
    default void assertTrueWithData(
        Boolean target,
        Object data,
        Object... args
    ) {
        if (!Boolean.TRUE.equals(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert false

    @Contract("true,_->fail")
    default void assertFalse(Boolean target, Object... args) {
        if (!Boolean.FALSE.equals(target)) {
            throw newE(args);
        }
    }

    @Contract("true,_,_->fail")
    default void assertFalseWithData(
        Boolean target,
        Object data,
        Object... args
    ) {
        if (!Boolean.FALSE.equals(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert equal

    default void assertEqual(
        Object source,
        Object target,
        Object... args
    ) {
        if (ObjectUtil.notEqual(source, target)) {
            throw newE(args);
        }
    }

    default void assertEqualWithData(
        Object source,
        Object target,
        Object data,
        Object... args
    ) {
        if (ObjectUtil.notEqual(source, target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not equal

    default void assertNotEqual(
        Object source,
        Object target,
        Object... args
    ) {
        if (ObjectUtil.equal(source, target)) {
            throw newE(args);
        }
    }

    default void assertNotEqualWithData(
        Object source,
        Object target,
        Object data,
        Object... args
    ) {
        if (ObjectUtil.equal(source, target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not blank

    @Contract("null,_->fail")
    default void assertNotBlank(Object target, Object... args) {
        if (!(target instanceof CharSequence) || StrUtil.isBlank((CharSequence) target)) {
            throw newE(args);
        }
    }

    @Contract("null,_,_->fail")
    default void assertNotBlankWithData(
        Object target,
        Object data,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(data, args);
        }
    }

    @Contract("null,_-> fail;!null,_->!null")
    default <T extends CharSequence> T assertNotBlankAndReturn(Object target, Object... args) {
        if (!(target instanceof CharSequence) || StrUtil.isBlank((CharSequence) target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_-> fail;!null,_,_->!null")
    default <T extends CharSequence> T assertNotBlankWithDataAndReturn(
        Object target,
        Object data,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_-> fail;!null,_,_->!null")
    default <T extends CharSequence> T assertNotBlankWithDataSupplierAndReturn(
        Object target,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert blank

    default void assertBlank(Object target, Object... args) {
        if (!(target instanceof CharSequence) || !StrUtil.isBlank((CharSequence) target)) {
            throw newE(args);
        }
    }

    default void assertBlankWithData(
        Object target,
        Object data,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || !StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(data, args);
        }
    }

    default <T extends CharSequence> T assertBlankAndReturn(Object target, Object... args) {
        if (!(target instanceof CharSequence) || !StrUtil.isBlank((CharSequence) target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    default <T extends CharSequence> T assertBlankWithDataAndReturn(
        Object target,
        Object data,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || !StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    default <T extends CharSequence> T assertBlankWithDataSupplierAndReturn(
        Object target,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (!(target instanceof CharSequence) || !StrUtil.isBlank((CharSequence) target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert matches

    @Contract("null,_,_->fail;_,null,_->fail")
    default void assertMatches(
        CharSequence target,
        Pattern pattern,
        Object... args
    ) {
        if (pattern == null || target == null || !pattern.matcher(target).matches()) {
            throw newE(args);
        }
    }

    @Contract("null,_,_,_->fail;_,null,_,_->fail")
    default void assertMatchesWithData(
        CharSequence target,
        Pattern pattern,
        Object data,
        Object... args
    ) {
        if (pattern == null || target == null || !pattern.matcher(target).matches()) {
            throw newEWithData(data, args);
        }
    }

    @Contract("null,_,_->fail;_,null,_->fail;!null,_,_->!null")
    default <T extends CharSequence> T assertMatchesAndReturn(
        CharSequence target,
        Pattern pattern,
        Object... args
    ) {
        if (pattern == null || target == null || !pattern.matcher(target).matches()) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_,_->fail;_,null,_,_->fail;!null,_,_,_->!null")
    default <T extends CharSequence> T assertMatchesWithDataAndReturn(
        CharSequence target,
        Pattern pattern,
        Object data,
        Object... args
    ) {
        if (pattern == null || target == null || !pattern.matcher(target).matches()) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_,_->fail;_,null,_,_->fail;!null,_,_,_->!null")
    default <T extends CharSequence> T assertMatchesWithDataSupplierAndReturn(
        CharSequence target,
        Pattern pattern,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (pattern == null || target == null || !pattern.matcher(target).matches()) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert not matches

    default void assertNotMatches(
        CharSequence target,
        Pattern pattern,
        Object... args
    ) {
        if (pattern != null && target != null && pattern.matcher(target).matches()) {
            throw newE(args);
        }
    }

    default void assertNotMatchesWithData(
        CharSequence target,
        Pattern pattern,
        Object data,
        Object... args
    ) {
        if (pattern != null && target != null && pattern.matcher(target).matches()) {
            throw newEWithData(data, args);
        }
    }

    @Contract("!null,_,_->!null")
    default <T extends CharSequence> T assertNotMatchesAndReturn(
        CharSequence target,
        Pattern pattern,
        Object... args
    ) {
        if (pattern != null && target != null && pattern.matcher(target).matches()) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("!null,_,_,_->!null")
    default <T extends CharSequence> T assertNotMatchesWithDataAndReturn(
        CharSequence target,
        Pattern pattern,
        Object data,
        Object... args
    ) {
        if (pattern != null && target != null && pattern.matcher(target).matches()) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("!null,_,_,_->!null")
    default <T extends CharSequence> T assertNotMatchesWithDataSupplierAndReturn(
        CharSequence target,
        Pattern pattern,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (pattern != null && target != null && pattern.matcher(target).matches()) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert not empty

    @Contract("null,_->fail")
    default void assertNotEmpty(Iterable<?> target, Object... args) {
        if (CollectionUtil.isEmpty(target)) {
            throw newE(args);
        }
    }

    @Contract("null,_,_->fail")
    default void assertNotEmptyWithData(
        Iterable<?> target,
        Object data,
        Object... args
    ) {
        if (CollectionUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
    }

    @Contract("null,_->fail;!null,_->!null")
    default <T extends Iterable<?>> T assertNotEmptyAndReturn(Iterable<?> target, Object... args) {
        if (CollectionUtil.isEmpty(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T extends Iterable<?>> T assertNotEmptyWithDataAndReturn(
        Iterable<?> target,
        Object data,
        Object... args
    ) {
        if (CollectionUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T extends Iterable<?>> T assertNotEmptyWithDataSupplierAndReturn(
        Iterable<?> target,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (CollectionUtil.isEmpty(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T) target;
    }

    // assert empty

    default void assertEmpty(Iterable<?> target, Object... args) {
        if (CollectionUtil.isNotEmpty(target)) {
            throw newE(args);
        }
    }

    default void assertEmptyWithData(Iterable<?> target, Object data, Object... args) {
        if (CollectionUtil.isNotEmpty(target)) {
            throw newEWithData(data, args);
        }
    }

    // assert not empty

    @Contract("null,_->fail")
    default <T> void assertNotEmpty(T[] target, Object... args) {
        if (ArrayUtil.isEmpty(target)) {
            throw newE(args);
        }
    }

    @Contract("null,_,_->fail")
    default void assertNotEmptyWithData(
        Object[] target,
        Object data,
        Object... args
    ) {
        if (ArrayUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
    }

    @Contract("null,_->fail;!null,_->!null")
    default <T> T[] assertNotEmptyAndReturn(Object[] target, Object... args) {
        if (ArrayUtil.isEmpty(target)) {
            throw newE(args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T> T[] assertNotEmptyWithDataAndReturn(Object[] target, Object data, Object... args) {
        if (ArrayUtil.isEmpty(target)) {
            throw newEWithData(data, args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    @Contract("null,_,_->fail;!null,_,_->!null")
    default <T> T[] assertNotEmptyWithDataSupplierAndReturn(
        Object[] target,
        Supplier<Object> dataSupplier,
        Object... args
    ) {
        if (ArrayUtil.isEmpty(target)) {
            throw newEWithData(dataSupplier.get(), args);
        }
        //noinspection unchecked
        return (T[]) target;
    }

    // return or wrapper exception

    @Contract("null,_->null")
    default <T> T returnOrWrapperE(Supplier<T> supplier, Object... args) {
        if (supplier == null) {
            return null;
        }

        try {
            return supplier.get();
        } catch (Exception e) {
            throw newEWithCause(e, args);
        }
    }

    @Contract("null,_,_->null")
    default <T> T returnOrWrapperEWithData(
        Supplier<T> supplier,
        Object data,
        Object... args
    ) {
        if (supplier == null) {
            return null;
        }

        try {
            return supplier.get();
        } catch (Exception e) {
            throw newEWithCauseAndData(e, data, args);
        }
    }

    // execute or wrapper exception

    default void executeOrWrapperE(Executable executable, Object... args) {
        try {
            executable.execute();
        } catch (Exception e) {
            throw newEWithCause(e, args);
        }
    }
}

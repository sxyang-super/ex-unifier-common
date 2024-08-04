package com.sxyangsuper.exunifier.common;

import com.sxyangsuper.exunifier.common.fixture.TestException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import static com.sxyangsuper.exunifier.common.fixture.TestExceptionEnum.ERROR_1;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AssertsTest {

    IAsserts<BaseException> asserts;
    BaseException suppliedException;
    Object[] args;
    RuntimeException cause;
    Object data;

    @BeforeEach
    void setUp() {
        //noinspection unchecked
        asserts = Mockito.spy(IAsserts.class);

        suppliedException = TestException.of(TestException::new, ERROR_1, "Test");
        args = new Object[]{ new Object() };
        cause = new RuntimeException();
        data = new Object();

        when(asserts.newE(args)).thenReturn(suppliedException);
        when(asserts.newEWithCause(cause, args)).thenReturn(suppliedException);
        when(asserts.newEWithData(data, args)).thenReturn(suppliedException);
        when(asserts.newEWithCauseAndData(cause, data, args)).thenReturn(suppliedException);
    }

    @Test
    void throwE() {
        final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.throwE(args));

        assertSame(suppliedException, thrownException);
        verify(asserts, times(1)).newE(args);
    }

    @Test
    void throwEWithCause() {
        final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.throwEWithCause(cause, args));

        assertSame(suppliedException, thrownException);
        verify(asserts, times(1)).throwEWithCause(cause, args);
    }

    @Test
    void throwEWithData() {
        final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.throwEWithData(data, args));

        assertSame(suppliedException, thrownException);
        verify(asserts, times(1)).throwEWithData(data, args);
    }

    @Test
    void throwEWithCauseAndData() {
        final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.throwEWithCauseAndData(cause, data, args));

        assertSame(suppliedException, thrownException);
        verify(asserts, times(1)).throwEWithCauseAndData(cause, data, args);
    }

    @Nested
    class TestAssertNotNullAndReturn {

        @Test
        void should_throw_exception_given_target_is_null() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotNull(null, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_null() {
            final Object target = new Object();
            final Object result = asserts.assertNotNull(target, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertNotNullWithData {

        @Test
        void should_throw_exception_given_target_is_null() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotNullWithData(null, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_null() {
            final Object target = new Object();
            final Object result = asserts.assertNotNullWithData(target, data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any(), any());
        }
    }

    @Nested
    class TestAssertNotNullWithDataSupplier {

        @Test
        void should_throw_exception_given_target_is_null() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotNullWithDataSupplier(null, () -> data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_null() {
            final Object target = new Object();
            final Object result = asserts.assertNotNullWithDataSupplier(target, () -> data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any(), any());
        }
    }

    @Nested
    class TestAssertNull {

        @Test
        void should_throw_exception_given_target_is_not_null() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNull(new Object(), args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_null() {
            assertDoesNotThrow(() -> asserts.assertNull(null, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertNullWithData {

        @Test
        void should_throw_exception_given_target_is_not_null() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNullWithData(new Object(), data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_null() {
            assertDoesNotThrow(() -> asserts.assertNullWithData(null, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertTrue {

        @Test
        void should_throw_exception_given_target_false() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertTrue(false, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_true() {
            assertDoesNotThrow(() -> asserts.assertTrue(true, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertTrueWithData {

        @Test
        void should_throw_exception_given_target_false() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertTrueWithData(false, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_true() {
            assertDoesNotThrow(() -> asserts.assertTrueWithData(true, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertFalse {

        @Test
        void should_throw_exception_given_target_true() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertFalse(true, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_false() {
            assertDoesNotThrow(() -> asserts.assertFalse(false, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertFalseWithData {

        @Test
        void should_throw_exception_given_target_true() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertFalseWithData(true, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_false() {
            assertDoesNotThrow(() -> asserts.assertFalseWithData(false, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertEqual {

        @Test
        void should_throw_exception_given_source_not_equal_target() {
            final int source = 0;
            final int target = 1;

            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertEqual(source, target, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_source_equal_target() {
            final int source = 0;
            final int target = 0;

            assertDoesNotThrow(() -> asserts.assertEqual(source, target, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertEqualWithData {

        @Test
        void should_throw_exception_given_source_not_equal_target() {
            final int source = 0;
            final int target = 1;

            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertEqualWithData(source, target, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_source_equal_target() {
            final int source = 0;
            final int target = 0;

            assertDoesNotThrow(() -> asserts.assertEqualWithData(source, target, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertNotEqual {

        @Test
        void should_throw_exception_given_source_equal_target() {
            final int source = 0;
            final int target = 0;

            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotEqual(source, target, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_source_not_equal_target() {
            final int source = 0;
            final int target = 1;

            assertDoesNotThrow(() -> asserts.assertNotEqual(source, target, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertNotEqualWithData {

        @Test
        void should_throw_exception_given_source_equal_target() {
            final int source = 0;
            final int target = 0;

            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotEqualWithData(source, target, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_source_not_equal_target() {
            final int source = 0;
            final int target = 1;

            assertDoesNotThrow(() -> asserts.assertNotEqualWithData(source, target, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertNotBlank {

        @Test
        void should_throw_exception_given_target_is_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotBlank("  ", args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_throw_exception_given_target_is_not_char_sequence() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotBlank(new Object(), args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_blank() {
            final Object target = "A";
            final CharSequence result = asserts.assertNotBlank(target, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertNotBlankWithData {

        @Test
        void should_throw_exception_given_target_is_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotBlankWithData("  ", data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_blank() {
            final Object target = "A";
            final CharSequence result = asserts.assertNotBlankWithData(target, data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertNotBlankWithDataSupplier {


        @Test
        void should_throw_exception_given_target_is_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotBlankWithDataSupplier("  ", () -> data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_blank() {
            final Object target = "A";
            final CharSequence result = asserts.assertNotBlankWithDataSupplier(target, () -> data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertBlank {

        @Test
        void should_throw_exception_given_target_is_not_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertBlank("A", args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_throw_exception_given_target_is_not_char_sequence() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertBlank(new Object(), args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_blank() {
            final String target = "  ";
            final CharSequence result = asserts.assertBlank(target, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertBlankWithData {

        @Test
        void should_throw_exception_given_target_is_not_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertBlankWithData("A", data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_blank() {
            final String target = "  ";
            final CharSequence result = asserts.assertBlankWithData(target, data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertBlankWithDataSupplier {

        @Test
        void should_throw_exception_given_target_is_not_blank() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertBlankWithDataSupplier("A", () -> data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_blank() {
            final String target = "  ";
            final CharSequence result = asserts.assertBlankWithDataSupplier(target, () -> data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestMatches {
        Pattern pattern;
        String notMatchTarget = "B";
        String matchTarget = "A";

        @BeforeEach
        void setUp() {
            pattern = Pattern.compile("^A$");
        }

        @Nested
        class TestAssertMatches {

            @Test
            void should_throw_exception_given_target_not_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertMatches(notMatchTarget, pattern, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newE(args);
            }

            @Test
            void should_throw_exception_given_pattern_is_null() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertMatches(matchTarget, null, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newE(args);
            }

            @Test
            void should_throw_exception_given_target_is_null() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertMatches(null, pattern, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newE(args);
            }

            @Test
            void should_not_throw_exception_given_target_match_pattern() {
                final CharSequence result = asserts.assertMatches(matchTarget, pattern, args);

                assertSame(matchTarget, result);
                verify(asserts, times(0)).newE(any());
            }
        }

        @Nested
        class TestAssertMatchesWithData {

            @Test
            void should_throw_exception_given_target_not_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertMatchesWithData(notMatchTarget, pattern, data, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newEWithData(data, args);
            }

            @Test
            void should_not_throw_exception_given_target_match_pattern() {
                final CharSequence result = asserts.assertMatchesWithData(matchTarget, pattern, data, args);

                assertSame(matchTarget, result);
                verify(asserts, times(0)).newEWithData(any());
            }
        }

        @Nested
        class TestAssertMatchesWithDataSupplier {

            @Test
            void should_throw_exception_given_target_not_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertMatchesWithDataSupplier(notMatchTarget, pattern, () -> data, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newEWithData(data, args);
            }

            @Test
            void should_not_throw_exception_given_target_match_pattern() {
                final CharSequence result = asserts.assertMatchesWithDataSupplier(matchTarget, pattern, () -> data, args);

                assertSame(matchTarget, result);
                verify(asserts, times(0)).newEWithData(any());
            }
        }

        @Nested
        class TestAssertNotMatches {

            @Test
            void should_throw_exception_given_target_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotMatches(matchTarget, pattern, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newE(args);
            }

            @Test
            void should_not_throw_exception_given_target_not_match_pattern() {
                final CharSequence result = asserts.assertNotMatches(notMatchTarget, pattern, args);

                assertSame(notMatchTarget, result);
                verify(asserts, times(0)).newE(any());
            }
        }

        @Nested
        class TestAssertNotMatchesWithData {

            @Test
            void should_throw_exception_given_target_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotMatchesWithData(matchTarget, pattern, data, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newEWithData(data, args);
            }

            @Test
            void should_not_throw_exception_given_target_not_match_pattern() {
                final CharSequence result = asserts.assertNotMatchesWithData(notMatchTarget, pattern, data, args);

                assertSame(notMatchTarget, result);
                verify(asserts, times(0)).newEWithData(any(), any());
            }
        }

        @Nested
        class TestAssertNotMatchesWithDataSupplier {

            @Test
            void should_throw_exception_given_target_match_pattern() {
                final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertNotMatchesWithDataSupplier(matchTarget, pattern, () -> data, args));

                assertSame(suppliedException, thrownException);
                verify(asserts, times(1)).newEWithData(data, args);
            }

            @Test
            void should_not_throw_exception_given_target_not_match_pattern() {
                final CharSequence result = asserts.assertNotMatchesWithDataSupplier(notMatchTarget, pattern, () -> data, args);

                assertSame(notMatchTarget, result);
                verify(asserts, times(0)).newEWithData(any(), any());
            }
        }
    }

    @Nested
    class TestAssertIterableNotEmpty {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmpty((Iterable<?>) null, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final List<Object> target = Collections.singletonList(new Object());
            final Iterable<?> result = asserts.assertNotEmpty(target, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertIterableNotEmptyWithData {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmptyWithData((Iterable<?>) null, data, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final List<Object> target = Collections.singletonList(new Object());
            final Iterable<?> result = asserts.assertNotEmptyWithData(target, data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any(), any());
        }
    }

    @Nested
    class TestAssertIterableNotEmptyWithDataSupplier {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmptyWithDataSupplier((Iterable<?>) null, () -> data, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final List<Object> target = Collections.singletonList(new Object());
            final Iterable<?> result = asserts.assertNotEmptyWithDataSupplier(target, () -> data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any(), any());
        }
    }

    @Nested
    class TestAssertIterableEmpty {

        @Test
        void should_throw_exception_given_target_is_not_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertEmpty(Collections.singletonList(new Object()), args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_empty() {
            assertDoesNotThrow(() -> asserts.assertEmpty(null, args));

            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertIterableEmptyWithData {

        @Test
        void should_throw_exception_given_target_is_not_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertEmptyWithData(Collections.singletonList(new Object()), data, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_empty() {
            assertDoesNotThrow(() -> asserts.assertEmptyWithData(null, data, args));

            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertArrayNotEmpty {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmpty((Object[]) null, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final Object[] target = { new Object() };
            final Object[] result = asserts.assertNotEmpty(target, args);

            assertSame(target, result);
            verify(asserts, times(0)).newE(any());
        }
    }

    @Nested
    class TestAssertArrayNotEmptyWithData {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmptyWithData((Object[]) null, data, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final Object[] target = { new Object() };
            final Object[] result = asserts.assertNotEmptyWithData(target, data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertArrayNotEmptyWithDataSupplier {

        @Test
        void should_throw_exception_given_target_is_empty() {
            final BaseException thrownException = assertThrows(BaseException.class,
                () -> asserts.assertNotEmptyWithDataSupplier((Object[]) null, () -> data, args)
            );

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithData(data, args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_empty() {
            final Object[] target = { new Object() };
            final Object[] result = asserts.assertNotEmptyWithDataSupplier(target, () -> data, args);

            assertSame(target, result);
            verify(asserts, times(0)).newEWithData(any(), any());
        }
    }

    @Nested
    class TestAssertSupplySuccessfully {

        @Test
        void should_throw_exception_given_supply_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertSupplySuccessfully(() -> {
                throw cause;
            }, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCause(cause, args);
        }

        @Test
        void should_return_supplied_value_given_supply_successfully() {
            final Object suppliedTarget = new Object();
            final Object result = asserts.assertSupplySuccessfully(() -> suppliedTarget, args);

            assertSame(suppliedTarget, result);
            verify(asserts, times(0)).newEWithCause(any(), any());
        }
    }

    @Nested
    class TestAssertSupplySuccessfullyWithData {

        @Test
        void should_throw_exception_given_supply_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertSupplySuccessfullyWithData(() -> {
                throw cause;
            }, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCauseAndData(cause, data, args);
        }

        @Test
        void should_return_supplied_value_given_supply_successfully() {
            final Object suppliedTarget = new Object();
            final Object result = asserts.assertSupplySuccessfullyWithData(() -> suppliedTarget, data, args);

            assertSame(suppliedTarget, result);
            verify(asserts, times(0)).newEWithCauseAndData(any(), any(), any());
        }
    }

    @Nested
    class TestAssertSupplySuccessfullyWithDataSupplier {

        @Test
        void should_throw_exception_given_supply_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertSupplySuccessfullyWithDataSupplier(() -> {
                throw cause;
            }, () -> data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCauseAndData(cause, data, args);
        }

        @Test
        void should_return_supplied_value_given_supply_successfully() {
            final Object suppliedTarget = new Object();
            final Object result = asserts.assertSupplySuccessfullyWithDataSupplier(() -> suppliedTarget, () -> data, args);

            assertSame(suppliedTarget, result);
            verify(asserts, times(0)).newEWithCauseAndData(any(), any(), any());
        }
    }

    @Nested
    class TestAssertExecuteSuccessfully {

        @Test
        void should_throw_exception_given_execute_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertExecuteSuccessfully(() -> {
                throw cause;
            }, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCause(cause, args);
        }

        @Test
        void should_return_supplied_value_given_execute_successfully() {
            asserts.assertExecuteSuccessfully(() -> {
            }, args);

            verify(asserts, times(0)).newEWithCause(any(), any());
        }
    }

    @Nested
    class TestAssertExecuteSuccessfullyWithData {

        @Test
        void should_throw_exception_given_execute_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertExecuteSuccessfullyWithData(() -> {
                throw cause;
            }, data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCauseAndData(cause, data, args);
        }

        @Test
        void should_return_supplied_value_given_execute_successfully() {
            asserts.assertExecuteSuccessfullyWithData(() -> {
            }, data, args);

            verify(asserts, times(0)).newEWithCauseAndData(any(), any(), any());
        }
    }

    @Nested
    class TestAssertExecuteSuccessfullyWithDataSupplier {

        @Test
        void should_throw_exception_given_execute_fails() {
            final BaseException thrownException = assertThrows(BaseException.class, () -> asserts.assertExecuteSuccessfullyWithDataSupplier(() -> {
                throw cause;
            }, () -> data, args));

            assertSame(suppliedException, thrownException);
            verify(asserts, times(1)).newEWithCauseAndData(cause, data, args);
        }

        @Test
        void should_return_supplied_value_given_execute_successfully() {
            asserts.assertExecuteSuccessfullyWithDataSupplier(() -> {
            }, () -> data, args);

            verify(asserts, times(0)).newEWithCauseAndData(any(), any(), any());
        }
    }
}

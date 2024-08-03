package com.sxyangsuper.exunifier.common;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

class AssertsTest {

    IAsserts<BaseException> asserts;
    BaseException suppliedException;
    Object[] args;
    Exception cause;
    Object data;

    @BeforeEach
    void setUp() {
        //noinspection unchecked
        asserts = Mockito.spy(IAsserts.class);

        suppliedException = new BaseException();
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
        final BaseException threwException = assertThrows(BaseException.class, () -> asserts.throwE(args));

        assertSame(suppliedException, threwException);
        Mockito.verify(asserts, times(1)).newE(args);
    }

    @Test
    void throwEWithCause() {
        final BaseException threwException = assertThrows(BaseException.class, () -> asserts.throwEWithCause(cause, args));

        assertSame(suppliedException, threwException);
        Mockito.verify(asserts, times(1)).throwEWithCause(cause, args);
    }

    @Test
    void throwEWithData() {
        final BaseException threwException = assertThrows(BaseException.class, () -> asserts.throwEWithData(data, args));

        assertSame(suppliedException, threwException);
        Mockito.verify(asserts, times(1)).throwEWithData(data, args);
    }

    @Test
    void throwEWithCauseAndData() {
        final BaseException threwException = assertThrows(BaseException.class, () -> asserts.throwEWithCauseAndData(cause, data, args));

        assertSame(suppliedException, threwException);
        Mockito.verify(asserts, times(1)).throwEWithCauseAndData(cause, data, args);
    }

    @Nested
    class TestAssertNotNull {

        @Test
        void should_throw_exception_given_target_is_null() {
            final BaseException threwException = assertThrows(BaseException.class, () -> asserts.assertNotNull(null, args));

            assertSame(suppliedException, threwException);
            Mockito.verify(asserts, times(1)).newE(args);
        }

        @Test
        void should_not_throw_exception_given_target_is_not_null() {
            assertDoesNotThrow(() -> asserts.assertNotNull(new Object(), args));

            Mockito.verify(asserts, times(0)).newE(any());
        }
    }

    @Test
    void assertNotNullWithData() {
    }

    @Test
    void assertNotNullAndReturn() {
    }

    @Test
    void assertNotNullWithDataAndReturn() {
    }

    @Test
    void assertNotNullWithDataSupplierAndReturn() {
    }

    @Test
    void assertNull() {
    }

    @Test
    void assertNullWithData() {
    }

    @Test
    void assertTrue() {
    }

    @Test
    void assertTrueWithData() {
    }

    @Test
    void assertFalse() {
    }

    @Test
    void assertFalseWithData() {
    }

    @Test
    void assertEqual() {
    }

    @Test
    void assertEqualWithData() {
    }

    @Test
    void assertNotEqual() {
    }

    @Test
    void assertNotEqualWithData() {
    }

    @Test
    void assertNotBlank() {
    }

    @Test
    void assertNotBlankWithData() {
    }

    @Test
    void assertNotBlankAndReturn() {
    }

    @Test
    void assertNotBlankWithDataAndReturn() {
    }

    @Test
    void assertNotBlankWithDataSupplierAndReturn() {
    }

    @Test
    void assertBlank() {
    }

    @Test
    void assertBlankWithData() {
    }

    @Test
    void assertBlankAndReturn() {
    }

    @Test
    void assertBlankWithDataAndReturn() {
    }

    @Test
    void assertBlankWithDataSupplierAndReturn() {
    }

    @Test
    void assertMatches() {
    }

    @Test
    void assertMatchesWithData() {
    }

    @Test
    void assertMatchesAndReturn() {
    }

    @Test
    void assertMatchesWithDataAndReturn() {
    }

    @Test
    void assertMatchesWithDataSupplierAndReturn() {
    }

    @Test
    void assertNotMatches() {
    }

    @Test
    void assertNotMatchesWithData() {
    }

    @Test
    void assertNotMatchesAndReturn() {
    }

    @Test
    void assertNotMatchesWithDataAndReturn() {
    }

    @Test
    void assertNotMatchesWithDataSupplierAndReturn() {
    }

    @Test
    void assertNotEmpty() {
    }

    @Test
    void assertNotEmptyWithData() {
    }

    @Test
    void assertNotEmptyAndReturn() {
    }

    @Test
    void assertNotEmptyWithDataAndReturn() {
    }

    @Test
    void assertNotEmptyWithDataSupplierAndReturn() {
    }

    @Test
    void assertEmpty() {
    }

    @Test
    void assertEmptyWithData() {
    }

    @Test
    void testAssertNotEmpty() {
    }

    @Test
    void testAssertNotEmptyWithData() {
    }

    @Test
    void testAssertNotEmptyAndReturn() {
    }

    @Test
    void testAssertNotEmptyWithDataAndReturn() {
    }

    @Test
    void testAssertNotEmptyWithDataSupplierAndReturn() {
    }

    @Test
    void returnOrWrapperE() {
    }

    @Test
    void returnOrWrapperEWithData() {
    }

    @Test
    void executeOrWrapperE() {
    }
}

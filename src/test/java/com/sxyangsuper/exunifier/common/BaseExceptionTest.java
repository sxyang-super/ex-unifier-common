package com.sxyangsuper.exunifier.common;

import com.sxyangsuper.exunifier.common.fixture.TestException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.sxyangsuper.exunifier.common.fixture.TestExceptionEnum.ERROR_1;
import static com.sxyangsuper.exunifier.common.fixture.TestExceptionEnum.ERROR_2;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

class BaseExceptionTest {

    @Test
    void of() {
        String messageArg = new Object().toString();
        BaseException exception = BaseException.of(BaseException::new, ERROR_1, messageArg);

        assertEquals(ERROR_1, exception.getExceptionEnum());
        assertEquals("error 1 " + messageArg, exception.getMessage());
    }

    @Test
    void ofWithCause() {
        String messageArg = new Object().toString();
        RuntimeException cause = new RuntimeException();

        BaseException exception = BaseException.ofWithCause(BaseException::new, ERROR_1, cause, messageArg);

        assertEquals(ERROR_1, exception.getExceptionEnum());
        assertEquals("error 1 " + messageArg, exception.getMessage());
        assertEquals(cause, exception.getCause());
    }

    @Test
    void ofWithData() {
        String messageArg = new Object().toString();
        Object data = new Object();

        BaseException exception = BaseException.ofWithData(BaseException::new, ERROR_1, data, messageArg);

        assertEquals(ERROR_1, exception.getExceptionEnum());
        assertEquals("error 1 " + messageArg, exception.getMessage());
        assertEquals(data, exception.getData());
    }

    @Test
    void ofWithCauseAndData() {
        String messageArg = new Object().toString();
        Object data = new Object();
        Exception cause = new RuntimeException();

        BaseException exception = BaseException.ofWithCauseAndData(BaseException::new, ERROR_1, cause, data, messageArg);

        assertEquals(ERROR_1, exception.getExceptionEnum());
        assertEquals("error 1 " + messageArg, exception.getMessage());
        assertEquals(cause, exception.getCause());
        assertEquals(data, exception.getData());
    }

    @Nested
    class TestEquals {
        BaseException sourceException;
        BaseException targetException;
        IExceptionEnum exceptionEnum;
        String messageArg;
        Object data;
        Exception cause;

        @BeforeEach
        void setUp() {
            exceptionEnum = ERROR_1;
            messageArg = new Object().toString();
            data = new Object();
            cause = new RuntimeException();

            sourceException = BaseException.ofWithCauseAndData(BaseException::new, exceptionEnum, cause, data, messageArg);
            targetException = BaseException.ofWithCauseAndData(BaseException::new, exceptionEnum, cause, data, messageArg);
        }

        @Test
        void should_return_true() {
            assertEquals(sourceException, targetException);
        }

        @Test
        void should_return_true_given_same_reference() {
            targetException = sourceException;

            assertEquals(sourceException, targetException);
        }

        @Test
        void should_return_false_given_target_is_null() {
            assertNotEquals(sourceException, null);
        }

        @Test
        void should_return_false_given_target_exception_is_a_different_class() {
            targetException = BaseException.ofWithCauseAndData(TestException::new, exceptionEnum, cause, data, messageArg);

            assertNotEquals(sourceException, targetException);
        }

        @Test
        void should_return_false_given_exception_enum_is_different() {
            targetException.exceptionEnum = ERROR_2;

            assertNotEquals(sourceException, targetException);
        }

        @Test
        void should_return_false_given_message_is_different() {
            targetException.message = new Object().toString();

            assertNotEquals(sourceException, targetException);
        }

        @Test
        void should_return_false_given_data_is_different() {
            targetException.data = new Object();

            assertNotEquals(sourceException, targetException);
        }
    }

    @Nested
    class TestHashcode {
        BaseException exception;
        int hasCode;

        @BeforeEach
        void setUp() {
            Exception cause = new RuntimeException();

            exception = BaseException.ofWithCauseAndData(BaseException::new, ERROR_1, cause, 1, 'a');
            hasCode = 328038120;
        }

        @Test
        void should_return_correct_hash_code() {
            assertEquals(hasCode, exception.hashCode());
        }

        @Test
        void should_ignore_args() {
            exception.args = new String[]{ new Object().toString() };

            assertEquals(hasCode, exception.hashCode());
        }

        @Test
        void should_ignore_cause() {
            // cause will change in setup method

            assertEquals(hasCode, exception.hashCode());
        }

        @Test
        void should_change_given_exception_enum_changed() {
            exception.exceptionEnum = ERROR_2;

            assertNotEquals(hasCode, exception.hashCode());
        }

        @Test
        void should_change_given_message_changed() {
            exception.message = new Object().toString();

            assertNotEquals(hasCode, exception.hashCode());
        }

        @Test
        void should_change_given_data_changed() {
            exception.data = new Object();

            assertNotEquals(hasCode, exception.hashCode());
        }
    }
}

package com.sxyangsuper.exunifier.common.fixture;

import com.sxyangsuper.exunifier.common.BaseException;
import com.sxyangsuper.exunifier.common.IExceptionEnum;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PUBLIC)
public class TestException extends BaseException {

    public static TestException of(
        final IExceptionEnum responseEnum,
        final Object... args
    ) {
        return of(TestException::new, responseEnum, args);
    }
}

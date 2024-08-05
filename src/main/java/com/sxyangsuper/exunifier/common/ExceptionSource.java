package com.sxyangsuper.exunifier.common;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * If this class qualified name changed,
 * need to update {@link ExUnifierConsts#EXCEPTION_SOURCE_QUALIFIED_NAME}.
 */
@Documented
@Retention(RetentionPolicy.CLASS)
@Target({ ElementType.TYPE })
public @interface ExceptionSource {
    Class<? extends IExceptionSourceSupplier> value();
}

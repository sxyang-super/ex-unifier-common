package com.sxyangsuper.exunifier.common;

import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@NoArgsConstructor
public class Sample {
    public String helloWorld() {
        log.debug("this is helloWorld method");
        return "Hello world";
    }
}

package com.sxyangsuper.exunifier.common;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SampleTest {

    Sample sample;

    @BeforeEach
    void setUp() {
        sample = new Sample();
    }

    @AfterEach
    void tearDown() {
        sample = null;
    }

    @Nested
    class HelloWorldTest {

        @Test
        void should_return_hello_world() {
            Assertions.assertEquals(sample.helloWorld(), "Hello world");
        }
    }
}

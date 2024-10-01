/*
 * Copyright (c) 2024 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package org.eclipse.lsp.cobol.usecases;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.eclipse.lsp.cobol.common.error.ErrorSource;
import org.eclipse.lsp.cobol.test.engine.UseCaseEngine;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.Range;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Test CICS START commands. Documentation link: <a
 * href="https://www.ibm.com/docs/en/cics-ts/6.x?topic=summary-start">START Command</a>
 *
 * <p>This class tests all variations of the START command found in the link above.
 */
public class TestCICSStartStatement {

    // Main Building Blocks
    private static final String BASE_TEXT =
            "       IDENTIFICATION DIVISION.\n"
                    + "       PROGRAM-ID. ABCDEF.\n"
                    + "       DATA DIVISION.\n"
                    + "       WORKING-STORAGE SECTION.\n"
                    + "       01 {$*transidVar} PIC S9 VALUE +100.\n"
                    + "       01 {$*brdataVar} PIC S9 VALUE +100.\n"
                    + "       01 {$*brdataLenVar} PIC S9 VALUE +100.\n"
                    + "       PROCEDURE DIVISION.\n"
                    + "            EXEC CICS \n"
                    + "            END-EXEC.";

    private static final String START_VALID = "START TRANSID(123) INTERVAL(0) REQID(123) FROM(123) LENGTH(123) FMH TERMID(123) SYSID(123) RTRANSID(123) RTERMID(123) QUEUE(123) NOCHECK PROTECT";
    private static final String START_ATTACH_VALID = "START ATTACH TRANSID(123)";
    private static final String START_BREXIT_VALID = "START BREXIT TRANSID({$transidVar}) BRDATA({$brdataVar}) BRDATALENGTH({$brdataLenVar}) USERID(123)";
    private static final String START_CHANNEL_VALID = "START TRANSID(123) CHANNEL(3) USERID(123) SYSID(123) NOCHECK PROTECT";

    private static final String START_INVALID = "START TRANSID(123) INTERVAL(0) {INTERVAL(120110)|errorOne}";

    // Utility Functions
    private static void noErrorTest(String newCommand) {
        UseCaseEngine.runTest(getTestString(newCommand), ImmutableList.of(), ImmutableMap.of());
    }

    private static void errorTest(String newCommand, String errorMessage) {
        UseCaseEngine.runTest(getTestString(newCommand), ImmutableList.of(), ImmutableMap.of(
                "errorOne",
                new Diagnostic(
                        new Range(),
                        errorMessage,
                        DiagnosticSeverity.Error,
                        ErrorSource.PARSING.getText()))
        );
    }

    private static String getTestString(String newCommand) {
        List<String> instances = Arrays.asList(newCommand.split("\\s"));
        instances.replaceAll(String.join("", Collections.nCopies(12, " "))::concat);
        ArrayList<String> base = new ArrayList<String>(Arrays.asList(BASE_TEXT.split("\n")));
        base.addAll(base.size() - 1, instances);
        return String.join("\n", base);
    }

    // Test Functions
    @Test
    void testStart() {
        noErrorTest(START_VALID);
    }

    @Test
    void testStartAttach() {
        noErrorTest(START_ATTACH_VALID);
    }

    @Test
    void testStartBrexit() {
        noErrorTest(START_BREXIT_VALID);
    }

    @Test
    void testStartChannel() {
        noErrorTest(START_CHANNEL_VALID);
    }

    // Invalid Tests
    @Test
    void testStartInvalid() {
        errorTest(START_INVALID, "Only one time-related block allowed.");
    }

}

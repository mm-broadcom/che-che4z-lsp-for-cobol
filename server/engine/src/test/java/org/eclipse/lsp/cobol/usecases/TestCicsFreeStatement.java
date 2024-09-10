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
 * Test CICS FREE command. Documentation link: <a
 * href="https://www.ibm.com/docs/en/cics-ts/6.x?topic=summary-free">FREE
 * Command</a>
 *
 * <p>This class tests all variations of the FREE command found in the link above.
 */
public class TestCicsFreeStatement {
    // Main Building Blocks
    private static final String BASE_TEXT =
            "       IDENTIFICATION DIVISION.\n"
                    + "       PROGRAM-ID. ABCDEF.\n"
                    + "       DATA DIVISION.\n"
                    + "       WORKING-STORAGE SECTION.\n"
                    + "       01 {$*fromVar} PIC S9 VALUE +100.\n"
                    + "       01 {$*fromLen} PIC S9 VALUE +10.\n"
                    + "       01 {$*maxLen} PIC S9 VALUE +10.\n"
                    + "       PROCEDURE DIVISION.\n"
                    + "            EXEC CICS \n"
                    + "            END-EXEC.";

    private static final String FREE = "FREE ";
    private static final String FREE_APPC = FREE + "CONVID(123) STATE(123)";
    private static final String FREE_LU61 = FREE + "SESSION(1234)";
    private static final String FREE_MRO = FREE + "SESSION(1234 STATE(123)";
    private static final String FREE_CHILD = FREE + "CHILD(123)";

    // Utility Functions
    private static void noErrorTest(String newCommand) {
        UseCaseEngine.runTest(getTestString(newCommand), ImmutableList.of(), ImmutableMap.of());
    }

    private static String getTestString(String newCommand) {
        List<String> instances = Arrays.asList(newCommand.split("\\s"));
        instances.replaceAll(String.join("", Collections.nCopies(12, " "))::concat);
        ArrayList<String> base = new ArrayList<String>(Arrays.asList(BASE_TEXT.split("\n")));
        base.addAll(base.size() - 1, instances);
        return String.join("\n", base);
    }

    // Tests
    void testFree() {
        noErrorTest(FREE);
    }

    void testFreeAppc() {
        noErrorTest(FREE_APPC);
    }

    void testFreeLu61() {
        noErrorTest(FREE_LU61);
    }

    void testFreeMro() {
        noErrorTest(FREE_MRO);
    }

    void testFreeChild() {
        noErrorTest(FREE_CHILD);
    }
}

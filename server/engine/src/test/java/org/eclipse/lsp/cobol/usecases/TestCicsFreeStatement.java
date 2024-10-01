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

import com.google.common.collect.ImmutableMap;
import org.eclipse.lsp.cobol.common.error.ErrorSource;
import org.eclipse.lsp.cobol.usecases.common.CICSTestUtils;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.Range;
import org.junit.jupiter.api.Test;

/**
 * Test FREE command. Documentation link: <a
 * href="https://www.ibm.com/docs/en/cics-ts/6.x?topic=summary-getnext-zos-communications-server-default">FREE
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
                    + "       PROCEDURE DIVISION.\n"
                    + "            EXEC CICS \n"
                    + "            END-EXEC.";

    private static final String FREE_VALID = "FREE";
    private static final String FREE_APPC_VALID = "FREE CONVID(123) STATE(123)";
    private static final String FREE_LU61_VALID = "FREE SESSION(123)";
    private static final String FREE_MRO_VALID = "FREE SESSION(123) STATE(123)";
    private static final String FREE_CHILD_VALID = "FREE CHILD(123)";

    private static final String FREE_CHILD_INVALID = "FREE CHILD(123) {STATE|errorOne}(123)";

    // Test Functions
    @Test
    void testFree() {
        CICSTestUtils.noErrorTest(FREE_VALID);
        CICSTestUtils.noErrorTest(FREE_APPC_VALID);
        CICSTestUtils.noErrorTest(FREE_LU61_VALID);
        CICSTestUtils.noErrorTest(FREE_MRO_VALID);
        CICSTestUtils.noErrorTest(FREE_CHILD_VALID);
    }

    @Test
    void testFreeInvalid() {
        ImmutableMap<String, Diagnostic> tempDiagnostic = ImmutableMap.of(
                "errorOne",
                new Diagnostic(
                        new Range(),
                        "Options \"CHILD or STATE\" are mutually exclusive.",
                        DiagnosticSeverity.Error,
                        ErrorSource.PARSING.getText()));

        CICSTestUtils.errorTest(FREE_CHILD_INVALID, tempDiagnostic);
    }
}

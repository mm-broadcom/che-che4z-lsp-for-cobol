/*
 * Copyright (c) 2022 Broadcom.
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
import org.eclipse.lsp.cobol.test.engine.UseCaseEngine;
import org.junit.jupiter.api.Test;

/** Tests the allocate statements
 *  Ref - https://www.ibm.com/docs/en/cobol-zos/6.3?topic=statements-allocate-statement
 * */
public class TestAllocateStatement {
  public static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. 'DUMMY' RECURSIVE.\n"
          + "       ENVIRONMENT DIVISION.\n"
          + "       INPUT-OUTPUT SECTION.\n"
          + "       FILE-CONTROL.\n"
          + "       DATA DIVISION.\n"
          + "       FILE SECTION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       LOCAL-STORAGE SECTION.\n"
          + "       1 {$*ulen} PIC S9(9) COMP-5.\n"
          + "       1 {$*p} USAGE POINTER.\n"
          + "       PROCEDURE DIVISION.\n"
          + "           ALLOCATE {$ulen} CHARACTERS INITIALIZED RETURNING {$p}.\n"
          + "       END PROGRAM 'DUMMY'.";

  @Test
  void test() {
    UseCaseEngine.runTest(TEXT, ImmutableList.of(), ImmutableMap.of());

  }
}

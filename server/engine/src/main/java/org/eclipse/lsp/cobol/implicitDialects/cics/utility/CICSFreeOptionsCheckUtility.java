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

package org.eclipse.lsp.cobol.implicitDialects.cics.utility;

import org.antlr.v4.runtime.ParserRuleContext;
import org.eclipse.lsp.cobol.common.dialects.DialectProcessingContext;
import org.eclipse.lsp.cobol.common.error.ErrorSeverity;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser.RULE_cics_free;

/** Checks CICS Free rules for required and invalid options */
public class CICSFreeOptionsCheckUtility extends CICSOptionsCheckBaseUtility {

    public static final int RULE_INDEX = RULE_cics_free;

    private static final Map<String, ErrorSeverity> DUPLICATE_CHECK_OPTIONS = new HashMap<>();

    /**
     * Checks CICS FREE rule options.
     *
     * @param dialectProcessingContext dialect processing context
     * @param errors a list of errors
     */
    public CICSFreeOptionsCheckUtility(
        DialectProcessingContext dialectProcessingContext,
        List<SyntaxError> errors) {
      super(dialectProcessingContext, errors, DUPLICATE_CHECK_OPTIONS);
    }

    /**
     * Entrypoint to check CICS FREE rule options
     *
     * @param ctx ParserRuleContext subclass containing options
     * @param <E> A subclass of ParserRuleContext
     */
    public <E extends ParserRuleContext> void checkOptions(E ctx) {
        if (ctx.getRuleIndex() == CICSParser.RULE_cics_free_subgroup) {
            checkFreeSubgroup((CICSParser.Cics_free_subgroupContext) ctx);
        }

        checkDuplicates(ctx);
    }


    private void checkFreeSubgroup(CICSParser.Cics_free_subgroupContext ctx) {
        checkMutuallyExclusiveOptions("CHILD or CONVID", ctx.CHILD(), ctx.CONVID());
        checkMutuallyExclusiveOptions("CHILD or SESSION", ctx.CHILD(), ctx.SESSION());
        checkMutuallyExclusiveOptions("CHILD or STATE", ctx.CHILD(), ctx.STATE());
    }
}

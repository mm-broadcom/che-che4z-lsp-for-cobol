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
import org.eclipse.lsp.cobol.implicitDialects.cics.CICSLexer;
import org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.eclipse.lsp.cobol.implicitDialects.cics.CICSParser.RULE_cics_start;

/** Checks CICS Start rules for required and invalid options */
public class CICSStartOptionsCheckUtility extends CICSOptionsCheckBaseUtility {

    public static final int RULE_INDEX = RULE_cics_start;

    private static final Map<Integer, ErrorSeverity> DUPLICATE_CHECK_OPTIONS = new HashMap<Integer, ErrorSeverity>() {
        {
            put(CICSLexer.FMH, ErrorSeverity.ERROR);
            put(CICSLexer.INTERVAL, ErrorSeverity.ERROR);
            put(CICSLexer.NOCHECK, ErrorSeverity.ERROR);
            put(CICSLexer.PROTECT, ErrorSeverity.ERROR);
        }
    };

    protected static final HashMap<Integer, String> DUPLICATE_RULE_OPTIONS = new HashMap<Integer, String>() {
        {
            put(CICSParser.RULE_cics_start_time_block, "Only one time-related block allowed.");
        }
    };

    public CICSStartOptionsCheckUtility(
        DialectProcessingContext dialectProcessingContext,
        List<SyntaxError> errors) {
        super(dialectProcessingContext, errors, DUPLICATE_CHECK_OPTIONS, DUPLICATE_RULE_OPTIONS);
    }

    /** Entrypoint to check CICS START rule options
     *
     * @param ctx ParserRuleContext subclass containing options
     * @param <E> A subclass of ParserRuleContext
     */
    public <E extends ParserRuleContext> void checkOptions(E ctx) {
        switch (ctx.getRuleIndex()) {
            case CICSParser.RULE_cics_start:
                checkStart((CICSParser.Cics_startContext) ctx);
                break;
            case CICSParser.RULE_cics_start_transid:
                checkStartTransid((CICSParser.Cics_start_transidContext) ctx);
                break;
            case CICSParser.RULE_cics_start_attach:
                checkStartAttach((CICSParser.Cics_start_attachContext) ctx);
                break;
            case CICSParser.RULE_cics_start_brexit:
                checkStartBrexit((CICSParser.Cics_start_brexitContext) ctx);
                break;
            case CICSParser.RULE_cics_start_channel:
                checkStartChannel((CICSParser.Cics_start_channelContext) ctx);
                break;
            case CICSParser.RULE_cics_start_time_block:
                checkStartTimeBlock((CICSParser.Cics_start_time_blockContext) ctx);
                break;
            default:
                break;
        }

        checkDuplicates(ctx);
    }

    private void checkStart(CICSParser.Cics_startContext ctx) {
        checkHasMandatoryOptions(ctx.START(), ctx, "START");
    }

    private void checkStartTransid(CICSParser.Cics_start_transidContext ctx) {
        checkHasRequiredOption(ctx.FROM(), ctx.LENGTH(), ctx, "LENGTH requires FROM");
        checkHasRequiredOption(ctx.FROM(), ctx.FMH(), ctx, "FMH requires FROM");

        checkMutuallyExclusiveOptions("TERMID or USERID", ctx.TERMID(), ctx.USERID());
    }

    private void checkStartAttach(CICSParser.Cics_start_attachContext ctx) {
        checkHasMandatoryOptions(ctx.TRANSID(), ctx, "TRANSID");
        checkHasRequiredOption(ctx.FROM(), ctx.LENGTH(), ctx, "LENGTH requires FROM");
    }

    private void checkStartBrexit(CICSParser.Cics_start_brexitContext ctx) {
        checkHasMandatoryOptions(ctx.TRANSID(), ctx, "TRANSID");
        checkHasRequiredOption(ctx.BRDATA(), ctx.BRDATALENGTH(), ctx, "BRDATALENGTH requires BRDATA");
    }

    private void checkStartChannel(CICSParser.Cics_start_channelContext ctx) {
        checkMutuallyExclusiveOptions("TERMID or USERID", ctx.TERMID(), ctx.USERID());
    }

    private void checkStartTimeBlock(CICSParser.Cics_start_time_blockContext ctx) {
        checkMutuallyExclusiveOptions("INTERVAL, TIME, AFTER, AT", ctx.INTERVAL(), ctx.TIME(), ctx.AFTER(), ctx.AT());
        if (!ctx.INTERVAL().getText().isEmpty()) {
            checkMutuallyExclusiveOptions("INTERVAL (0) OR INTERVAL(hhmmss)", ctx.cics_zero_digit(), ctx.cics_hhmmss());
        }
    }

}

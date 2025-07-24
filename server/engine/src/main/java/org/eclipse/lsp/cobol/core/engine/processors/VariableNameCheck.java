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
package org.eclipse.lsp.cobol.core.engine.processors;

import java.util.Locale;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.message.MessageTemplate;
import org.eclipse.lsp.cobol.common.model.NodeType;
import org.eclipse.lsp.cobol.common.model.tree.ParagraphNameNode;
import org.eclipse.lsp.cobol.common.model.tree.ProgramNode;
import org.eclipse.lsp.cobol.common.model.tree.variable.VariableWithLevelNode;
import org.eclipse.lsp.cobol.common.processor.ProcessingContext;
import org.eclipse.lsp.cobol.common.processor.Processor;

/** perform semantics check on the name of a VariableNode. */
public class VariableNameCheck implements Processor<VariableWithLevelNode> {

  @Override
  public void accept(
      VariableWithLevelNode variableWithLevelNode, ProcessingContext processingContext) {
    checkVarFunctionCollision(variableWithLevelNode, processingContext);
    checkVarParagraphCollision(variableWithLevelNode, processingContext);
  }

  /** If a function is declared in the repository, the same name can't be used as a variable name */
  private void checkVarFunctionCollision(
      VariableWithLevelNode variableWithLevelNode, ProcessingContext processingContext) {
    variableWithLevelNode
        .getNearestParentByType(NodeType.PROGRAM)
        .map(ProgramNode.class::cast)
        .filter(
            pgm ->
                pgm.getRepository()
                    .containsKey(variableWithLevelNode.getName().toUpperCase(Locale.ROOT)))
        .map(
            x ->
                MessageTemplate.of(
                    "variableNameCheck.notAllowedVariableName", variableWithLevelNode.getName()))
        .map(variableWithLevelNode::getError)
        .ifPresent(processingContext.getErrors()::add);
  }

  /** If a variable is declared, the same name can't be used as a paragraph name */
  private void checkVarParagraphCollision(
      VariableWithLevelNode variableWithLevelNode, ProcessingContext processingContext) {
    variableWithLevelNode
        .getNearestParentByType(NodeType.PROGRAM)
        .map(ProgramNode.class::cast)
        .filter(
            pgm -> {
              pgm.getDepthFirstList(ParagraphNameNode.class::isInstance).stream()
                  .map(ParagraphNameNode.class::cast)
                  .forEach(
                      paragraphNameNode -> {
                        if (paragraphNameNode.getName().equals(variableWithLevelNode.getName())) {
                          SyntaxError err =
                              paragraphNameNode.getError(
                                  MessageTemplate.of(
                                      "paragraphNameCheck.notAllowedParagraphName",
                                      paragraphNameNode.getName()));
                          processingContext.getErrors().add(err);
                        }
                      });
              return true;
            });
  }
}

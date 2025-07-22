/*
 * Copyright (c) 2025 Broadcom.
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

import java.util.*;
import java.util.stream.Collectors;
import org.eclipse.lsp.cobol.common.error.SyntaxError;
import org.eclipse.lsp.cobol.common.message.MessageTemplate;
import org.eclipse.lsp.cobol.common.model.NodeType;
import org.eclipse.lsp.cobol.common.model.tree.Node;
import org.eclipse.lsp.cobol.common.model.tree.ParagraphNameNode;
import org.eclipse.lsp.cobol.common.model.tree.ProgramNode;
import org.eclipse.lsp.cobol.common.model.tree.variable.VariableNode;
import org.eclipse.lsp.cobol.common.model.tree.variable.VariableWithLevelNode;
import org.eclipse.lsp.cobol.common.processor.ProcessingContext;
import org.eclipse.lsp.cobol.common.processor.Processor;

/**
 * Perform semantics check on the name of a ParagraphNameNode. If a variable is declared in the
 * repository, the same name can't be used as a variable name
 */
public class ParagraphNameCheck implements Processor<ParagraphNameNode> {

  @Override
  public void accept(ParagraphNameNode paragraphNameNode, ProcessingContext processingContext) {
    Optional<Node> nearestParentByType = paragraphNameNode.getNearestParentByType(NodeType.PROGRAM);

    if (nearestParentByType.isPresent()) {
      Optional<ProgramNode> castResult = nearestParentByType.map(ProgramNode.class::cast);
      if (castResult.isPresent()) {
        ProgramNode programNode = castResult.get();
        List<Node> varNodes = programNode.getDepthFirstList(VariableNode.class::isInstance);
        List<Node> varLeveLNodes =
            programNode.getDepthFirstList(VariableWithLevelNode.class::isInstance);

        List<SyntaxError> varNodeErr =
            varNodes.stream()
                .filter(
                    var -> {
                      VariableNode variableNode = (VariableNode) var;
                      return variableNode.getName().equalsIgnoreCase(paragraphNameNode.getName());
                    })
                .map(
                    err ->
                        MessageTemplate.of(
                            "paragraphNameCheck.notAllowedVariableName",
                            paragraphNameNode.getName()))
                .map(paragraphNameNode::getError)
                .collect(Collectors.toList());

        List<SyntaxError> varNodeLevelErr =
            varLeveLNodes.stream()
                .filter(
                    var -> {
                      VariableWithLevelNode variableWithLevelNode = (VariableWithLevelNode) var;
                      return variableWithLevelNode
                          .getName()
                          .equalsIgnoreCase(paragraphNameNode.getName());
                    })
                .map(
                    err ->
                        MessageTemplate.of(
                            "paragraphNameCheck.notAllowedVariableName",
                            paragraphNameNode.getName()))
                .map(paragraphNameNode::getError)
                .collect(Collectors.toList());

        if (!varNodeErr.isEmpty() || !varNodeLevelErr.isEmpty()) {
          SyntaxError err =
              paragraphNameNode.getError(
                  MessageTemplate.of(
                      "paragraphNameCheck.notAllowedVariableName", paragraphNameNode.getName()));
          processingContext.getErrors().add(err);
        }
      }
    }

    paragraphNameNode
        .getNearestParentByType(NodeType.PROGRAM)
        .map(ProgramNode.class::cast)
        .filter(
            pgm ->
                pgm.getRepository()
                    .containsKey(paragraphNameNode.getName().toUpperCase(Locale.ROOT)))
        .map(
            err ->
                MessageTemplate.of(
                    "paragraphNameCheck.notAllowedVariableName", paragraphNameNode.getName()))
        .map(paragraphNameNode::getError)
        .ifPresent(processingContext.getErrors()::add);
  }
}

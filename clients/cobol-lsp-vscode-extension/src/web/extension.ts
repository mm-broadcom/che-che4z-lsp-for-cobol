// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import { SnippetCompletionProvider } from "../services/snippetcompletion/SnippetCompletionProvider";
import { LANGUAGE_ID } from "../constants";
import { DialectRegistry } from "../services/DialectRegistry";
import { __ExtensionApi } from "@code4z/cobol-dialect-api";

let outputChannel: vscode.OutputChannel;

interface __AnalysisApi {
  analysis(uri: string, text: string): Promise<any>;
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(
  context: vscode.ExtensionContext,
): Promise<__ExtensionApi & __AnalysisApi> {
  outputChannel = vscode.window.createOutputChannel("COBOL Language Support");

  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      { language: LANGUAGE_ID },
      new SnippetCompletionProvider(),
    ),
  );

  // 'export' public api-surface
  return {
    v1: {
      async registerDialect(extensionId: string, dialect: any) {
        return registerNewDialect(extensionId, {
          name: dialect.name,
          description: dialect.description,
          jar: vscode.Uri.parse(dialect.jar, true),
          snippets: vscode.Uri.parse(dialect.snippets, true),
        });
      },
    },
    version: "1.0.0",
    analysis(uri: string, text: string): Promise<any> {
      return Promise.resolve();
    },
  };
}

interface DialectDetail {
  name: string;
  description: string;
  jar: vscode.Uri;
  snippets: vscode.Uri;
}

const registerNewDialect = async (
  extensionId: string,
  dialect: DialectDetail,
) => {
  outputChannel.appendLine(
    "Register new dialect: \r\n" + JSON.stringify(dialect),
  );

  try {
    await vscode.workspace.fs.stat(dialect.jar);
  } catch (_error) {
    return Error(`Dialect jar file ${dialect.jar.fsPath} does not exist`);
  }

  try {
    await vscode.workspace.fs.stat(dialect.snippets);
  } catch (_error) {
    return Error(`Dialect snippets file ${dialect.jar.fsPath} does not exist`);
  }

  DialectRegistry.register(
    extensionId,
    dialect.name,
    dialect.jar,
    dialect.description,
    dialect.snippets.fsPath,
  );
  outputChannel.appendLine("Restart analysis");

  const unregisterDialect = () => {
    DialectRegistry.unregister(dialect.name);
  };

  return unregisterDialect;
};

// This method is called when your extension is deactivated
export function deactivate() {}

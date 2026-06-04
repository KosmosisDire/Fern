import * as vscode from 'vscode';
import * as lspClient from './lspClient';
import * as sectionFolding from './sectionFolding';
import * as commentEdits from './commentEdits';

export function activate(context: vscode.ExtensionContext): void
{
    lspClient.register(context);
    sectionFolding.register(context);
    commentEdits.register(context);
}

export function deactivate(): Thenable<void> | undefined
{
    lspClient.clearDecorations();
    return lspClient.deactivate();
}

import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

async function startServer(): Promise<boolean> {
    const config = vscode.workspace.getConfiguration('fern');
    const serverPath = config.get<string>('lspPath');

    if (!serverPath) {
        const action = await vscode.window.showWarningMessage(
            'Fern LSP server path not configured.',
            'Open Settings'
        );
        if (action === 'Open Settings') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'fern.lspPath');
        }
        return false;
    }

    if (!fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(`Fern LSP server not found at: ${serverPath}`);
        return false;
    }

    const serverOptions: ServerOptions = {
        run: { command: serverPath, transport: TransportKind.stdio },
        debug: { command: serverPath, transport: TransportKind.stdio }
    };

    const includes = config.get<string[]>('includes') || [];

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'fern' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{fn,fern}')
        },
        initializationOptions: {
            includes
        }
    };

    client = new LanguageClient(
        'fernLanguageServer',
        'Fern Language Server',
        serverOptions,
        clientOptions
    );

    await client.start();
    return true;
}

async function stopServer(): Promise<void> {
    if (client) {
        await client.stop();
        client = undefined;
    }
}

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.commands.registerCommand('fern.toggleLanguageServer', async () => {
            if (client) {
                await stopServer();
                vscode.window.showInformationMessage('Fern Language Server stopped.');
            } else {
                const started = await startServer();
                if (started) {
                    vscode.window.showInformationMessage('Fern Language Server started.');
                }
            }
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(e => {
            if (e.document.languageId !== 'fern') return;
            if (e.contentChanges.length !== 1) return;

            const change = e.contentChanges[0];
            if (change.text !== '-') return;

            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document !== e.document) return;

            const line = e.document.lineAt(change.range.start.line);
            const lineText = line.text;

            const blockMatch = lineText.match(/^(\s*)--- - ---$/);
            if (blockMatch)
            {
                const indent = blockMatch[1];
                editor.insertSnippet(
                    new vscode.SnippetString(indent + '---\n' + indent + '$0\n' + indent + '---'),
                    line.range
                );
                return;
            }

            const textBefore = lineText.substring(0, change.range.start.character) + '-';
            if (!textBefore.endsWith('---')) return;
            if (textBefore.length > 3 && textBefore[textBefore.length - 4] === '-') return;

            const insertPos = new vscode.Position(change.range.start.line, change.range.start.character + 1);
            editor.insertSnippet(
                new vscode.SnippetString(' $0 ---'),
                insertPos
            );
        })
    );

    startServer();
}

export function deactivate(): Thenable<void> | undefined {
    return stopServer();
}

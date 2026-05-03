import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

type HoverMode = 'normal' | 'ast' | 'fhir';

let client: LanguageClient | undefined;
let hoverMode: HoverMode = 'normal';
let statusBarItem: vscode.StatusBarItem | undefined;
let debugDecorationType: vscode.TextEditorDecorationType | undefined;

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
        },
        middleware: {
            provideHover: async (document, position, token, next) => {
                const hover = await next(document, position, token);
                if (hoverMode === 'normal' || !hover || !debugDecorationType)
                {
                    return hover;
                }
                const editor = vscode.window.visibleTextEditors.find(e => e.document === document);
                if (editor && hover.range)
                {
                    editor.setDecorations(debugDecorationType, [hover.range]);
                }
                return hover;
            }
        }
    };

    client = new LanguageClient(
        'fernLanguageServer',
        'Fern Language Server',
        serverOptions,
        clientOptions
    );

    await client.start();
    await sendHoverMode(hoverMode);
    return true;
}

async function stopServer(): Promise<void> {
    if (client) {
        await client.stop();
        client = undefined;
    }
}

async function sendHoverMode(mode: HoverMode): Promise<void> {
    if (!client) return;
    try {
        await client.sendNotification('fern/setHoverMode', { mode });
    } catch (err) {
        // ignore: server may not be ready yet
    }
}

function updateStatusBar(): void {
    if (!statusBarItem) return;
    if (hoverMode === 'normal') {
        statusBarItem.hide();
    } else {
        statusBarItem.text = `Fern: ${hoverMode.toUpperCase()} debug`;
        statusBarItem.tooltip = 'Click to switch back to normal hover';
        statusBarItem.command = 'fern.setHoverModeNormal';
        statusBarItem.show();
    }
}

function clearDecorations(): void {
    if (!debugDecorationType) return;
    for (const editor of vscode.window.visibleTextEditors) {
        editor.setDecorations(debugDecorationType, []);
    }
}

async function setHoverMode(mode: HoverMode): Promise<void> {
    hoverMode = mode;
    updateStatusBar();
    await sendHoverMode(mode);
    if (mode === 'normal') {
        clearDecorations();
    }
}

export function activate(context: vscode.ExtensionContext) {
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    context.subscriptions.push(statusBarItem);

    debugDecorationType = vscode.window.createTextEditorDecorationType({
        backgroundColor: 'rgba(255, 200, 0, 0.25)',
        borderRadius: '2px'
    });
    context.subscriptions.push(debugDecorationType);

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
        vscode.commands.registerCommand('fern.setHoverModeAst', () => setHoverMode('ast')),
        vscode.commands.registerCommand('fern.setHoverModeFhir', () => setHoverMode('fhir')),
        vscode.commands.registerCommand('fern.setHoverModeNormal', () => setHoverMode('normal'))
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
    clearDecorations();
    return stopServer();
}

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

async function startServer(): Promise<boolean>
{
    const config = vscode.workspace.getConfiguration('fern');
    const serverPath = config.get<string>('lspPath');

    if (!serverPath)
    {
        const action = await vscode.window.showWarningMessage(
            'Fern LSP server path not configured.',
            'Open Settings'
        );
        if (action === 'Open Settings')
        {
            vscode.commands.executeCommand('workbench.action.openSettings', 'fern.lspPath');
        }
        return false;
    }

    if (!fs.existsSync(serverPath))
    {
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
            provideHover: async (document, position, token, next) =>
            {
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

async function stopServer(): Promise<void>
{
    if (client)
    {
        await client.stop();
        client = undefined;
    }
}

async function sendHoverMode(mode: HoverMode): Promise<void>
{
    if (!client) return;
    try
    {
        await client.sendNotification('fern/setHoverMode', { mode });
    }
    catch (err)
    {
    }
}

function updateStatusBar(): void
{
    if (!statusBarItem) return;
    if (hoverMode === 'normal')
    {
        statusBarItem.hide();
    }
    else
    {
        statusBarItem.text = `Fern: ${hoverMode.toUpperCase()} debug`;
        statusBarItem.tooltip = 'Click to switch back to normal hover';
        statusBarItem.command = 'fern.setHoverModeNormal';
        statusBarItem.show();
    }
}

export function clearDecorations(): void
{
    if (!debugDecorationType) return;
    for (const editor of vscode.window.visibleTextEditors)
    {
        editor.setDecorations(debugDecorationType, []);
    }
}

async function setHoverMode(mode: HoverMode): Promise<void>
{
    hoverMode = mode;
    updateStatusBar();
    await sendHoverMode(mode);
    if (mode === 'normal')
    {
        clearDecorations();
    }
}

export function register(context: vscode.ExtensionContext): void
{
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    context.subscriptions.push(statusBarItem);

    debugDecorationType = vscode.window.createTextEditorDecorationType({
        backgroundColor: 'rgba(255, 200, 0, 0.25)',
        borderRadius: '2px'
    });
    context.subscriptions.push(debugDecorationType);

    context.subscriptions.push(
        vscode.commands.registerCommand('fern.toggleLanguageServer', async () =>
        {
            if (client)
            {
                await stopServer();
                vscode.window.showInformationMessage('Fern Language Server stopped.');
            }
            else
            {
                const started = await startServer();
                if (started)
                {
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

    startServer();
}

export function deactivate(): Thenable<void> | undefined
{
    return stopServer();
}

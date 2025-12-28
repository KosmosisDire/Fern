import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

function getIncludes(): string[] {
    const config = vscode.workspace.getConfiguration('fern');
    return config.get<string[]>('includes') || [];
}

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('fern');

    let serverPath = config.get<string>('lspPath');

    if (!serverPath) {
        const possiblePaths = [
            path.join(context.extensionPath, '..', 'build', 'Debug', 'FernLSP.exe'),
            path.join(context.extensionPath, '..', 'build', 'Release', 'FernLSP.exe'),
            path.join(context.extensionPath, 'bin', 'FernLSP.exe'),
        ];

        for (const p of possiblePaths) {
            try {
                const fs = require('fs');
                if (fs.existsSync(p)) {
                    serverPath = p;
                    break;
                }
            } catch {
                // Continue to next path
            }
        }
    }

    if (!serverPath) {
        vscode.window.showWarningMessage(
            'Fern LSP server not found. Set fern.lspPath in settings or build the compiler.'
        );
        return;
    }

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'fern' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{fn,fern}'),
            configurationSection: 'fern'
        },
        initializationOptions: {
            includes: getIncludes()
        }
    };

    client = new LanguageClient(
        'fernLanguageServer',
        'Fern Language Server',
        serverOptions,
        clientOptions
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(e => {
            if (e.affectsConfiguration('fern.includes')) {
                client.sendNotification('fern/updateIncludes', {
                    includes: getIncludes()
                });
            }
        })
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

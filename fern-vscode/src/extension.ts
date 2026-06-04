import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

type HoverMode = 'normal' | 'ast' | 'fhir';

const sectionHeaderRegex = /^\s*---\s+.+?\s+---\s*$/;

interface LineDepthInfo
{
    startDepth: number;
    minDepth: number;
    isHeader: boolean;
}

function computeLineDepthInfo(document: vscode.TextDocument): LineDepthInfo[]
{
    const lineCount = document.lineCount;
    const info: LineDepthInfo[] = new Array(lineCount);

    let depth = 0;
    let inBlockComment = false;
    let inMultilineString = false;
    let inRawMultilineString = false;

    for (let i = 0; i < lineCount; i++)
    {
        const text = document.lineAt(i).text;
        const startDepth = depth;
        let minDepth = depth;
        const lineIsClean = !inBlockComment && !inMultilineString && !inRawMultilineString;
        const isHeader = lineIsClean && sectionHeaderRegex.test(text);

        let inString = false;
        let inRawString = false;
        let inChar = false;
        let j = 0;

        while (j < text.length)
        {
            const c = text[j];
            const c2 = text[j + 1];
            const c3 = text[j + 2];

            if (inBlockComment)
            {
                if (c === '-' && c2 === '-' && c3 === '-')
                {
                    inBlockComment = false;
                    j += 3;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (inMultilineString)
            {
                if (c === '"' && c2 === '"' && c3 === '"')
                {
                    inMultilineString = false;
                    j += 3;
                }
                else if (c === '\\')
                {
                    j += 2;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (inRawMultilineString)
            {
                if (c === '`' && c2 === '`' && c3 === '`')
                {
                    inRawMultilineString = false;
                    j += 3;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (inString)
            {
                if (c === '\\')
                {
                    j += 2;
                }
                else if (c === '"')
                {
                    inString = false;
                    j++;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (inRawString)
            {
                if (c === '`')
                {
                    inRawString = false;
                    j++;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (inChar)
            {
                if (c === '\\')
                {
                    j += 2;
                }
                else if (c === '\'')
                {
                    inChar = false;
                    j++;
                }
                else
                {
                    j++;
                }
                continue;
            }

            if (c === '-' && c2 === '-' && c3 === '-')
            {
                inBlockComment = true;
                j += 3;
                continue;
            }
            if (c === '-' && c2 === '-')
            {
                break;
            }
            if (c === '"' && c2 === '"' && c3 === '"')
            {
                inMultilineString = true;
                j += 3;
                continue;
            }
            if (c === '`' && c2 === '`' && c3 === '`')
            {
                inRawMultilineString = true;
                j += 3;
                continue;
            }
            if (c === '"')
            {
                inString = true;
                j++;
                continue;
            }
            if (c === '`')
            {
                inRawString = true;
                j++;
                continue;
            }
            if (c === '\'')
            {
                inChar = true;
                j++;
                continue;
            }
            if (c === '{')
            {
                depth++;
                j++;
                continue;
            }
            if (c === '}')
            {
                depth--;
                if (depth < minDepth) minDepth = depth;
                j++;
                continue;
            }
            j++;
        }

        info[i] = { startDepth, minDepth, isHeader };
    }

    return info;
}

class SectionHeaderFoldingProvider implements vscode.FoldingRangeProvider
{
    provideFoldingRanges(document: vscode.TextDocument): vscode.FoldingRange[]
    {
        const info = computeLineDepthInfo(document);
        const lineCount = info.length;
        const ranges: vscode.FoldingRange[] = [];

        for (let i = 0; i < lineCount; i++)
        {
            if (!info[i].isHeader) continue;
            const headerDepth = info[i].startDepth;
            let end = lineCount - 1;

            for (let j = i + 1; j < lineCount; j++)
            {
                if (info[j].startDepth < headerDepth)
                {
                    end = j - 1;
                    break;
                }
                if (info[j].minDepth < headerDepth)
                {
                    end = j - 1;
                    break;
                }
                if (info[j].isHeader && info[j].startDepth <= headerDepth)
                {
                    end = j - 1;
                    break;
                }
            }

            while (end > i && document.lineAt(end).text.trim().length === 0)
            {
                end--;
            }

            if (end > i)
            {
                ranges.push(new vscode.FoldingRange(i, end, vscode.FoldingRangeKind.Region));
            }
        }

        return ranges;
    }
}

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
        vscode.languages.registerFoldingRangeProvider(
            { scheme: 'file', language: 'fern' },
            new SectionHeaderFoldingProvider()
        )
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

            const blockMatch = lineText.match(/^(\s*)(?:--- - ---|----)$/);
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

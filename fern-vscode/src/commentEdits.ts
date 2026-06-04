import * as vscode from 'vscode';

export function register(context: vscode.ExtensionContext): void
{
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(e =>
        {
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
}

import * as vscode from 'vscode';

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

export function register(context: vscode.ExtensionContext): void
{
    context.subscriptions.push(
        vscode.languages.registerFoldingRangeProvider(
            { scheme: 'file', language: 'fern' },
            new SectionHeaderFoldingProvider()
        )
    );
}

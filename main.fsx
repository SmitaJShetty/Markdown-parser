let concatfunc a b =
    let result = a + b
    result

let encloseWithTags text startTag endTag =
    let result = concatfunc text endTag |> concatfunc startTag
    result

let bindTextWithMarkdown text markdownChar=
    let input:string = text
    let md:char = markdownChar
    let charStartPosition = input.IndexOf(markdownChar)
    let charEndPosition = text.LastIndexOf(markdownChar)
    let length = charEndPosition - charStartPosition
    let subString = text.Substring(charStartPosition+1,length-1)
    let markedUpSubstring = encloseWithTags subString "<i>" "</i>"
    let firstHalfStr = text.Substring(0,charStartPosition)
    let secondHalfStr = text.Substring(charEndPosition,text.Length-charEndPosition-1)
    concatfunc secondHalfStr (concatfunc firstHalfStr markedUpSubstring)

let checkCharExists text toSearchChar =
    let result = String.exists(fun c -> c.Equals(toSearchChar:char)) text
    result

let ReplaceWithMarkdown (text:string) =
    if(checkCharExists text '#') then
        bindTextWithMarkdown text '#'
    elif(checkCharExists text '*') then
        bindTextWithMarkdown text '*'
    else
        text + "no output"

ReplaceWithMarkdown "def * abc *"

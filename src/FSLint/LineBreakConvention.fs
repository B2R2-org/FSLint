module B2R2.FSLint.LineBreakConvention

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Diagnostics

/// What a separator list is told when its members do not agree on their
/// breaks. Shared so that a list checked elsewhere answers by the same name.
let [<Literal>] Message = "Use consistent line breaks"

/// Returns true when the gap between two neighbouring items holds a line
/// break, i.e. the separator that joins them was broken across lines.
let private isBrokenGap (prev: range, next: range) =
  next.StartLine > prev.EndLine

/// Joins two lines of a closed-up stretch. F# wants a space between most
/// neighbours, but not where a bracket meets what it fences in, nor before a
/// separator, so those junctions close up tight.
let private joinTight (left: string) (right: string) =
  let opensLeft = left.Length > 0 && "([<{".Contains left[left.Length - 1]
  let closesRight = right.Length > 0 && ")]>},;".Contains right[0]
  if opensLeft || closesRight then left + right else left + " " + right

/// The width the stretch would take once every break inside it is closed up:
/// its own line up to where it begins, then all of it through to the end of its
/// last line, each break and the indentation behind it collapsed away. Whatever
/// trails it there, a closing paren and an '=' say, lands on that line too and
/// counts.
let private closedWidth (src: ISourceText) (span: range) =
  let lastLine = src.GetLineString(span.EndLine - 1)
  let closed =
    Position.mkPos span.EndLine (lastLine.TrimEnd().Length)
    |> Range.mkRange "" span.Start
    |> src.GetSubTextFromRange
    |> fun text -> text.Split '\n'
    |> Array.map (fun line -> line.Trim())
    |> Array.reduce joinTight
  span.StartColumn + closed.Length

/// Returns true when the stretch is free to close up onto one line: it has to
/// fit the line budget, and nothing may sit inside it that closing up would
/// swallow: a comment, or a compiler directive whose own line cannot move.
let private isClosable src (span: range) =
  closedWidth src span <= getCurrentMaxLineLength ()
  && (findCommentsBetween span.StartRange span.EndRange |> Option.isNone)
  && (findDirectivesBetween span.StartRange span.EndRange |> Option.isNone)

/// True when the stretch would stand on one line inside the line budget.
let closesUpWithin src (span: range) =
  closedWidth src span <= getCurrentMaxLineLength ()

/// The column the line a range begins on starts in. For a list divided by a
/// leading separator that is the separator rather than the member behind it,
/// so both kinds of list answer on the same terms.
let private lineIndent (src: ISourceText) (r: range) =
  let line = src.GetLineString(r.StartLine - 1)
  line.Length - line.TrimStart().Length

/// Every line a list spread down the page runs to begins in the one column.
/// That column is what makes the members read as a list at all; wandering left
/// and right they read as unrelated lines that happen to follow one another.
/// The first line to leave the column takes the report.
///
/// A first member sharing its line with whatever opened the list is left out
/// of this. Its column was settled by the opener, an `if` or an opening
/// bracket, and the lines below answer to each other rather than to it.
let private checkColumnAgreement src ranges =
  let opensItsLine (r: range) = lineIndent src r = r.StartColumn
  let members =
    match ranges with
    | head :: tail when not (opensItsLine head) -> tail
    | _ -> ranges
  match members with
  | first :: rest ->
    let column = lineIndent src first
    rest
    |> List.tryFind (fun r -> lineIndent src r <> column)
    |> function
      | Some stray ->
        reportColumnAgreement src stray
        true
      | None ->
        false
  | [] ->
    false

/// Every gap between neighbours must agree: either they all carry a line break
/// or none of them does. The first gap to break ranks with the rest takes the
/// report. Returns true when it reported, so that the caller can leave its
/// finer checks alone: a stretch already answering for its own shape has
/// nothing further to say about the pieces inside it.
let checkGapAgreement src ranges =
  let gaps = ranges |> List.pairwise
  match gaps with
  | first :: _ ->
    let firstIsBroken = isBrokenGap first
    gaps
    |> List.tryFind (fun gap -> isBrokenGap gap <> firstIsBroken)
    |> function
      | Some(_, next) ->
        reportWarn src next Message
        true
      | None ->
        false
  | [] ->
    false

/// The gaps of a list of members, then the column its members stand in. Only
/// a list whose ranges are the members themselves is asked the second: a chain
/// of branches hands over the keywords that open its links, and `then` sitting
/// mid-line beside `else` at the head of one has no column to share.
let checkMemberPlacement src ranges =
  if checkGapAgreement src ranges then
    true
  else
    match ranges |> List.pairwise with
    | first :: _ when isBrokenGap first -> checkColumnAgreement src ranges
    | _ -> false

/// Reports a construct spread over several lines though the whole of it would
/// close up onto one inside the line budget. `span` is everything it occupies,
/// and `joints` are the places a break could have landed; the first of them to
/// have fallen past the opening line takes the report. Returns true when it
/// reported, so that the caller can leave its finer checks alone: a construct
/// that belongs on one line has nothing further to answer for.
let checkClosesUp src (span: range) (joints: range list) =
  if not isStrict || span.StartLine = span.EndLine then
    false
  elif not (isClosable src span) then
    false
  else
    match joints |> List.tryFind (fun r -> r.StartLine > span.StartLine) with
    | Some joint ->
      reportNewLine src joint
      true
    | None ->
      false

/// Reports on a list laid out inside `span`, the whole stretch it occupies with
/// its brackets. Fitting on one line settles it first: while the stretch would
/// close up inside the line budget it has to stay closed up, and only once it
/// would not does the weaker demand take over, that every gap between
/// neighbours agree.
///
/// The brackets belong to the stretch because a break landing just inside one
/// is a break in the list, and with a single element it is the only place a
/// break can land at all.
let checkBracketedPlacement src (span: range) ranges =
  if not isStrict || List.isEmpty ranges then
    ()
  elif not (isClosable src span) then
    checkMemberPlacement src ranges |> ignore
  elif span.StartLine <> span.EndLine then
    ranges
    |> List.tryFind (fun (r: range) -> r.StartLine > span.StartLine)
    |> Option.defaultValue (List.last ranges)
    |> reportNewLine src
  else
    ()

/// Reports on a list whose fence the author may open into a block. Sending
/// the first element to a line below the one that opens the fence says the
/// list is to be read that way, and a block is a layout in its own right: it
/// is not asked to close up. What it is asked is that the closing bracket
/// answer the opening one, standing below the last element as the first
/// stands below the opening, and that the elements between them agree among
/// themselves. Whether they take one line together or one line each is then
/// the author's to choose.
///
/// A fence left shut is judged as any other bracketed list: it closes up
/// while it can, and once it cannot its gaps have to agree.
let checkOpenableFence src (span: range) ranges =
  if not isStrict || List.isEmpty ranges then
    ()
  else
    let first: range = List.head ranges
    let last: range = List.last ranges
    let openedUp = first.StartLine > span.StartLine
    let closedDown = span.EndLine > last.EndLine
    if openedUp <> closedDown then
      Range.mkRange "" last.End span.End |> reportBracketSymmetry src
    elif openedUp then
      checkMemberPlacement src ranges |> ignore
    else
      checkBracketedPlacement src span ranges

/// Reports on a list with no brackets of its own, such as a chain of '&&'
/// operands or a curried parameter list. The gaps between elements are all
/// there is to it, so a list of one has no layout to judge.
let checkUniformPlacement src (ranges: range list) =
  match ranges with
  | _ :: _ :: _ when isStrict ->
    let span = List.reduce Range.unionRanges ranges
    if isClosable src span then
      ranges
      |> List.pairwise
      |> List.tryFind isBrokenGap
      |> Option.iter (fun (_, next) -> reportNewLine src next)
    else
      checkMemberPlacement src ranges |> ignore
  | _ ->
    ()

/// Reports a list spread over lines though the whole of it would close up
/// onto one, and asks nothing of it once it would not. Where a list too wide
/// for its line is broken is left to whoever wrote it: a value built out of
/// bits may be read as a row of fields or as a set of flags, and the two want
/// opposite layouts with nothing in the syntax to tell them apart.
let checkClosesUpOnly src (ranges: range list) =
  match ranges with
  | _ :: _ :: _ when isStrict ->
    let span = List.reduce Range.unionRanges ranges
    if isClosable src span then
      ranges
      |> List.pairwise
      |> List.tryFind isBrokenGap
      |> Option.iter (fun (_, next) -> reportNewLine src next)
    else
      ()
  | _ ->
    ()

/// Returns true when the body was left on the line its keyword ends, rather
/// than broken onto a line of its own.
let private isInline (keyword: range, body: range) =
  keyword.EndLine = body.StartLine

/// The width the item would take once its body sits beside its keyword: the
/// keyword's line up to the keyword, one space, then the body's line from where
/// the body begins. Measuring the body by its line rather than by its own range
/// keeps whatever trails it, a comment above all, inside the budget.
let private joinedWidth (src: ISourceText) (keyword: range) (body: range) =
  let bodyLine = src.GetLineString(body.StartLine - 1)
  keyword.EndColumn + 1 + (bodyLine.TrimEnd().Length - body.StartColumn)

/// Returns true when the body is free to sit beside its keyword. It has to fit
/// the line budget, and one that already broke away has to be a single line
/// with nothing but whitespace behind it: a body needing several lines of its
/// own, a `let` or a sequence say, can never come back up. Neither can one
/// standing behind a comment that the join would swallow, nor one reached only
/// through a compiler directive, whose own line has to stay where it is.
let private isJoinable src ((keyword, body) as item) =
  joinedWidth src keyword body <= getCurrentMaxLineLength ()
  && (isInline item
      || (body.StartLine = body.EndLine
          && findCommentsBetween keyword body |> Option.isNone
          && findDirectivesBetween keyword body |> Option.isNone))

/// The shared body of the two keyword-group checks. Fitting on one line comes
/// first: when every body in the group could sit beside its keyword, every one
/// of them has to, and only once at least one cannot does the group fall back
/// to the weaker demand that all of them break away.
/// The group's own range, taken from the keyword opening it. That keyword
/// stands in every build alike, so it names the group across the readings
/// even where the bodies below it do not match.
let private groupRange (items: (range * range) list) = items |> List.head |> fst

/// True when a conditional directive stands inside the group, so that its
/// members are not the same set in every build.
let private spansDirective (items: (range * range) list) =
  match items with
  | (firstKeyword, _) :: _ ->
    let last = items |> List.map (fun (_, body: range) -> body.EndLine)
    straddlesDirective firstKeyword.StartLine (List.max last)
  | [] ->
    false

/// Whether every body of a group could sit beside its keyword is a question
/// with a different answer per build once a directive stands inside it, so a
/// group reaching across one does not answer for the file on its own: its
/// demand is held back until every build has been read, and raised only if
/// every one of them made it. A build that can close up thus asks nothing of
/// a build that cannot, while a group every build can close up is still
/// closed up.
let private checkGroup src joinable items =
  if not isStrict || List.isEmpty items then
    ()
  else
    let canJoin = joinable && items |> List.forall (isJoinable src)
    let straddles = spansDirective items
    if straddles && not canJoin then blockJoin (groupRange items) else ()
    if canJoin then
      items
      |> List.tryFind (isInline >> not)
      |> Option.iter (fun (_, body) ->
        if straddles then deferJoin (groupRange items) body
        else reportNewLine src body)
    elif List.length items > 1 then
      items
      |> List.tryFind isInline
      |> Option.iter (fun (_, body) -> reportWarn src body Message)
    else
      ()

/// Judges sibling bodies that hang off a keyword such as '->', 'then' or
/// 'else'. Each item pairs that keyword's range with the body's range.
let checkUniformBreak src (items: (range * range) list) =
  checkGroup src true items

/// Judges a group whose members can never share their keyword's line, such as
/// the '|' of a barred handler sitting under its 'with'. Joining is off the
/// table, so all that is left to ask is whether every one of them broke away.
let checkUniformlyBroken src (items: (range * range) list) =
  checkGroup src false items

/// Checks a parameter list, covering both the tupled form `(a, b, c)` and the
/// curried form `a b c`. A tupled list is measured by its elements inside the
/// parentheses that fence them in; a curried one has no fences of its own, its
/// parameters standing side by side.
let checkParameters src (pats: SynPat list) =
  match pats with
  | [ SynPat.Paren(pat = SynPat.Tuple(elementPats = elements)
                   range = fence) ] ->
    elements
    |> List.map (fun pat -> pat.Range)
    |> checkBracketedPlacement src fence
  | _ ->
    pats |> List.map (fun pat -> pat.Range) |> checkUniformPlacement src

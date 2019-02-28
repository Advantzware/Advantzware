/* =====================================================================
   file    : src2html.p
   purpose : read HTML file and add syntax-coloring to sourcecode-examples 
   ====================================================================== 
*/

define input parameter pcOriginalDir as character no-undo.
define input parameter pcNewDir      as character no-undo.
define input parameter pcFilename    as character no-undo.

/* define the html-tags you want to insert. actual styles will be defined in stylesheet */

/* comments */
&global-define tag_start_comment '<span class="src-comment">'
&global-define tag_end_comment '</span><!--src-comment-->'
/* progress 4gl keywords */
&global-define tag_start_keyword '<span class="src-keyword">'
&global-define tag_end_keyword '</span><!--src-keyword-->'
/* literal integers and decimals */
&global-define tag_start_number '<span class="src-number">'
&global-define tag_end_number '</span><!--src-number-->'
/* literal strings */
&global-define tag_start_char '<span class="src-char">'
&global-define tag_end_char '</span><!--src-char-->'
/* preprocessors (things between {} or starting with &) */
&global-define tag_start_prep '<span class="src-prep">'
&global-define tag_end_prep '</span><!--src-prep-->'

                                                   
define stream oldfile.
define stream newfile.

define variable cLine         as character no-undo.
define variable iCommentDepth as integer   no-undo.
define variable lInSource     as logical   no-undo.
define variable cPrevTag      as character no-undo.
define variable iPrevPosition as integer   no-undo.

input stream oldfile from value(pcOriginalDir + '/' + pcFilename).
output stream newfile to value(pcNewDir + '/' + pcFilename).

repeat :
  import stream oldfile unformatted cLine no-error.
  if (index(cLine, "</div><!--progresscode-->") > 0) and lInSource then
     lInSource = false.
     
  if lInSource then
     run parseLine.
     
  if index(cLine,"<div")>0 and index(cLine,'class="progresscode"')>0 then
     lInSource = true.
     
  if cLine="" then cLine=" ".
  put stream newfile unformatted cLine skip.
end.


procedure parseLine :
  define variable lInDoubleString as logical   no-undo.
  define variable lInSingleString as logical   no-undo.
  define variable cToken          as character no-undo.
  define variable dNumber         as decimal   no-undo.
  define variable lInHtml         as logical   no-undo.
  define variable lStringed       as logical   no-undo.
  define variable iChar           as integer   no-undo.
  define variable cChunk              as character no-undo.

  /* step 0: remove old html tags */
  cLine = replace(cLine, {&tag_start_comment}, "").
  cLine = replace(cLine, {&tag_end_comment}, "").
  cLine = replace(cLine, {&tag_start_char}, "").
  cLine = replace(cLine, {&tag_end_char}, "").
  cLine = replace(cLine, {&tag_start_prep}, "").
  cLine = replace(cLine, {&tag_end_prep}, "").
  cLine = replace(cLine, {&tag_start_keyword}, "").
  cLine = replace(cLine, {&tag_end_keyword}, "").
  cLine = replace(cLine, {&tag_start_number}, "").
  cLine = replace(cLine, {&tag_end_number}, "").

  assign cPrevTag      = ""
         iPrevPosition = 0.

  /* step 0.5: correct for correct HTML */
  cLine = replace(cLine, '&lt;', '<').
  cLine = replace(cLine, '&amp;', '&').
  cLine = replace(cLine, '&gt;', '>').
  cLine = replace(cLine, '&', '&amp;').
  cLine = replace(cLine, '<', '&lt;').
  cLine = replace(cLine, '>', '&gt;').
        
  /* step 1: find (nested) comments */
  iChar = 0.
  loop_chars:
  do while iChar < length(cLine) :
    iChar = iChar + 1.
    lStringed = false.

    cChunk = substring(cLine, iChar , 2).

    if cChunk = "/*":u  then 
    do:
      iCommentDepth = iCommentDepth + 1.
      if iCommentDepth = 1 then 
      do:
        cLine = substring(cLine, 1, iChar - 1)
                 + {&tag_start_comment}
                 + substring(cLine, iChar ). 
        iChar = iChar + length({&tag_start_comment}).
        assign cPrevTag      = ""
               iPrevPosition = 0.
      end.
      next loop_chars.
    end.

    if cChunk = "*/":u  then 
    do:
      iCommentDepth = iCommentDepth - 1.
      if iCommentDepth = 0 then 
      do:
        cLine = substring(cLine, 1, iChar + 1)
                 + {&tag_end_comment}
                 + substring(cLine, iChar + 2). 
        iChar = iChar + length({&tag_end_comment}).
        assign cPrevTag      = ""
               iPrevPosition = 0.
      end.
      next loop_chars.
    end.
   
    if iCommentDepth > 0 then next loop_chars.

    /* ignore existing HTML tags */
    cChunk = substring(cLine, iChar,1).
    if cChunk = "<" then 
    do: 
      lInHtml = true.
      next loop_chars.
    end.
    
    if cChunk = ">" then 
    do: 
      lInHtml = false.
      next loop_chars.
    end.
    
    if lInHtml then 
      next loop_chars.

    /* step 2a: find string literals ("" - double quotes) */
    if not lInSingleString then 
    do:
      cChunk = substring(cLine, iChar,1).
      if cChunk = '"' then 
      do:
        if not lInDoubleString then 
        do:
          cLine = substring(cLine, 1, iChar - 1)
                            + {&tag_start_char}
                            + substring(cLine, iChar). 
          iChar = iChar + length({&tag_start_char}).
        end.
        else 
        do:
          cLine = substring(cLine, 1, iChar)
                  + {&tag_end_char}
                  + substring(cLine, iChar + 1). 
          iChar = iChar + length({&tag_end_char}) + 1.
        end.
        
        lInDoubleString = not lInDoubleString.
        lStringed = true.
        assign cPrevTag      = ""
               iPrevPosition = 0.
      end.
    end.

    /* step 2b: find string literals ('' - single quotes) */
    if not lInDoubleString then 
    do:
      cChunk = substring(cLine, iChar,1).
      if cChunk = "'" then 
      do:
        if not lInSingleString then 
        do:
          cLine = substring(cLine, 1, iChar - 1)
                   + {&tag_start_char}
                   + substring(cLine, iChar). 
          iChar = iChar + length({&tag_start_char}).
        end.
        else 
        do:
          cLine = substring(cLine, 1, iChar)
                   + {&tag_end_char}
                   + substring(cLine, iChar + 1). 
          iChar = iChar + length({&tag_end_char}).
        end.
        lInSingleString = not lInSingleString.
        lStringed = true.
        assign cPrevTag      = ""
               iPrevPosition = 0.
      end.
    end.

    /* step 3: find preprocessors and include directives */
    cChunk = substring(cLine, iChar,1).
    if cChunk = "~{" then 
    do:
      cLine = substring(cLine, 1, iChar - 1)
               + {&tag_start_prep}
               + substring(cLine, iChar ). 
      iChar = iChar + length({&tag_start_prep}).
      assign cPrevTag      = ""
             iPrevPosition = 0.
    end.
    if cChunk = "~}" then 
    do:
      cLine = substring(cLine, 1, iChar )
               + {&tag_end_prep}
               + substring(cLine, iChar + 1). 
      iChar = iChar + length({&tag_end_prep}) + 1.
      assign cPrevTag      = ""
             iPrevPosition = 0.
    end.

    /* step 4: find words (could be keywords or numbers) */
    if not (lInDoubleString or lInSingleString) then 
    do:
      cChunk = substring(cLine, iChar,1).
      if index(" (),/+.:=" + chr(9), cChunk)>0 or lStringed then 
      do:
        if keyword-all(cToken) ne ? then
          run addKeywordTags(input-output iChar, cToken).
        else 
        if cToken matches "&*" then
          run addPreprocessorTags(input-output iChar, cToken).
        else 
        if cToken <> '' then 
        do:
          dNumber = ?.
          assign dNumber = decimal(cToken) no-error.
          if dNumber<>? then
            run addNumberTag(input-output iChar, cToken).
          else 
            run addStandardTags(input-output iChar, cToken).
        end.
        
        cToken = ''.

        if (not lStringed) and (cChunk <> " ") then 
        do:
          /* the word-separator itself has to be colored too */
          run addStandardTags(input-output iChar, cChunk).
        end.

      end.
      else 
        cToken = cToken + cChunk.
    end.
  end.

  /* step 5: remaining token at end of line */
  iChar = length(cLine) + 1.
  if (not (lInDoubleString or lInSingleString)) and trim(cToken)<>'' then 
  do:
    if keyword-all(cToken) ne ? then
      run addKeywordTags (input-output iChar, cToken).
    else 
    if cToken matches "&*" then
      run addPreprocessorTags(input-output iChar, cToken).
    else 
    do:
      dNumber = ?.
      assign dNumber=decimal(cToken) no-error.
      if dNumber<>? then
        run addNumberTag(input-output iChar, cToken).
    end.
  end.

  assign cPrevTag      = ""
         iPrevPosition = 0.
end procedure. /* parseLine */


procedure AddStandardTags :
  define input-output parameter piPos   as integer   no-undo.
  define input        parameter pcToken as character no-undo.
    
  cPrevTag      = "".
  iPrevPosition = 0.
end procedure. /* AddStandardTags */


procedure addNumberTag :
  define input-output parameter piPos   as integer   no-undo.
  define input        parameter pcToken as character no-undo.

  run AddSyntaxTags 
    ( input-output piPos
    , pcToken
    , {&tag_start_number}
    , {&tag_end_number}
    ).
end procedure. /* addNumberTag */


procedure addPreprocessorTags :
  define input-output parameter piPos   as integer   no-undo.
  define input        parameter pcToken as character no-undo.

  if pcToken matches "&lt;*" or pcToken matches "&gt;*" then 
  do:
    run AddKeywordTags 
      ( input-output piPos
      , input pcToken
      ).
    return.
  end.

  run AddSyntaxTags 
    ( input-output piPos
    , input pcToken
    , input {&tag_start_prep}
    , input {&tag_end_prep}
    ).
end procedure. /* addPreprocessorTags */


procedure AddKeywordTags :
  define input-output parameter piPos   as integer   no-undo.
  define input        parameter pcToken as character no-undo.

  if pcToken="?" then 
  do:
    run addNumberTag
      ( input-output piPos
      , pcToken
      ).
    return.
  end.

  run AddSyntaxTags 
    ( input-output piPos
    , pcToken
    , {&tag_start_keyword}
    , {&tag_end_keyword}
    ).    
end procedure. /* AddKeywordTags */


procedure AddSyntaxTags :
  define input-output parameter piPos      as integer   no-undo.
  define input        parameter pcToken    as character no-undo.
  define input        parameter pcStartTag as character no-undo.
  define input        parameter pcEndTag   as character no-undo.

  if cPrevTag=pcEndTag then 
  do:
    substring(cLine, iPrevPosition - length(pcEndTag), length(pcEndTag)) = "".
    piPos = piPos - length(pcEndTag).
    cLine = substring(cLine, 1, piPos - 1 - length(pcToken))
             + pcToken
             + pcEndTag
             + substring(cLine, piPos ).
    piPos = piPos + length(pcEndTag).
  end.
  else 
  do:
    cLine = substring(cLine, 1, piPos - 1 - length(pcToken))
             + pcStartTag
             + pcToken
             + pcEndTag
             + substring(cLine, piPos ).
    piPos = piPos + length(pcStartTag) + length(pcEndTag).
  end.

  cPrevTag      = pcEndTag.
  iPrevPosition = piPos.
end procedure. /* AddSyntaxTags */
  

/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\tdf\csh.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa AS char NO-UNDO.
DEF OUTPUT PARAM erc AS int NO-UNDO.
DEF SHARED STREAM s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
IF ws_segment <> "CSH" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/substr.i    misc_elem[1]             18 02} /* sales requirement code */
      {rc/substr.i    misc_elem[2]             20 02} /* action code */
      {ed/tdf/substrde.i misc_number1          22 16 2} /* amount */
      {rc/substr.i    misc_elem[4]             38 35} /* account number */
      {rc/substr.i    misc_elem[5]             73 08} /* date */
      {rc/substr.i    misc_elem[6]             81 02} /* agency qual code */
      {rc/substr.i    misc_elem[7]             83 10} /* special services code */
      {rc/substr.i    misc_elem[8]             93 02} /* product/service subst code */
      {ed/tdf/substrde.i misc_number2          95 11 9} /* percent */
      {rc/substr.i    misc_elem[10]            106 02} /* percent qualifier */
      .
  END.
END CASE.
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  CASE ws_version:
  WHEN "4010" THEN
  DO:
    ASSIGN
      {rc/outstr.i    misc_elem[1]             18 02} /* sales requirement code */
      {rc/outstr.i    misc_elem[2]             20 02} /* action code */
      {ed/tdf/outstrde.i misc_number1          22 16 2} /* amount */
      {rc/outstr.i    misc_elem[4]             38 35} /* account number */
      {rc/outstr.i    misc_elem[5]             73 08} /* date */
      {rc/outstr.i    misc_elem[6]             81 02} /* agency qual code */
      {rc/outstr.i    misc_elem[7]             83 10} /* special services code */
      {rc/outstr.i    misc_elem[8]             93 02} /* product/service subst code */
      {ed/tdf/outstrde.i misc_number2          95 11 9} /* percent */
      {rc/outstr.i    misc_elem[10]            106 02} /* percent qualifier */
      .
  END.
END CASE.
END.    /* O */
IF command matches "*P*" THEN
DO:
  DISPLAY STREAM s-out
    ws_segment
    misc_elem[1]             FORMAT "X(02)" LABEL "Req Code"
    misc_elem[2]             FORMAT "X(02)" LABEL "Action Code"
    misc_number1                            LABEL "Amount"
    misc_elem[4]             FORMAT "X(35)" LABEL "Account#"
    misc_elem[5]             FORMAT "X(08)" LABEL "Date"
    misc_elem[6]             FORMAT "X(02)" LABEL "Agency Qual"
    misc_elem[7]             FORMAT "X(10)" LABEL "Special Svc Code"
    misc_elem[8]             FORMAT "X(02)" LABEL "Substitution Code "
    misc_number2                            LABEL "Pct"
    misc_elem[10]            FORMAT "X(02)" LABEL "Pct Qual"
    WITH side-labels width 144 no-box.
END.

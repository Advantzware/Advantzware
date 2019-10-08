DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
def shared stream s-out.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
DEF var ws_int AS int NO-UNDO.
IF ws_segment <> "000" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
  ASSIGN
    {rc/substr.i header_setid          01 06}
    {rc/substr.i header_sep-code       07 11}
    {rc/substr.i header_int-cd         18 22}
    {rc/substr.i header_fgid           40 02}
    {rc/substr.i header_partner        57 15}
    {rc/substr.i header_isa            87 09 integer}
    {rc/substr.i header_gs             96 09 integer}
    {rc/substr.i header_st            105 09 integer}
    {rc/substr.i header_std-ver       114 12}
    {rc/substr.i header_std-rcvd      141 02}
    {rc/substr.i header_std-used      143 01}
    {rc/substr.i header_rcvd-test-prod 187 1}
    {rc/substr.i header_part-test-prod 189 1}
    .
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
  ASSIGN
    str_buffa = FILL(" ",189)
    {rc/outstr.i header_setid          01 06}
    {rc/outstr.i header_sep-code       07 11}
    {rc/outstr.i header_int-cd         18 22}
    {rc/outstr.i header_partner        57 15}
    {rc/outstr.i header_std-ver       114 12}
    {rc/outstr.i header_std-used      143 01}
    {rc/outstr.i header_part-test-prod 189 1}
    .
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
        ws_segment
        header_partner
        header_setid
        header_fgid         label "FG-ID"
        header_std-ver      label "Version"
        header_std-rcvd     label "Std"
        header_std-used     label "Std Used"
        header_rcvd-test-prod   label "T/P Rcvd"
        header_part-test-prod   label "T/P Set"
        skip space(9)
        header_isa          label "ISA"
        header_gs           label "GS"
        header_st           label "ST"
        header_int-cd       label "Int-CD"
    with side-labels width 144. 
end.

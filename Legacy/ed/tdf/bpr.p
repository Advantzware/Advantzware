DEF INPUT  PARAM command    AS char NO-UNDO.
DEF INPUT-OUTPUT  PARAM str_buffa  AS char NO-UNDO.
DEF OUTPUT PARAM erc        AS int NO-UNDO.
{ed/edivars.i       "shared"}
{ed/tdf/sharedv.i   "shared"}
{rc/datev.i}
def shared stream s-out.
def var bpr01   as char     format "x(02)" label "Trx Handling Code"    no-undo.
def var bpr02   as decimal  format ">>>,>>>,>>>.99DR" label "Amount"     no-undo.
def var bpr03   as char     format "X"      label "CR/DR Flag"          no-undo.
def var bpr04   as char     format "x(03)"  label "Pmt Method"          no-undo.
def var bpr05   as char     format "x(10)"  label "Pmt Format"          no-undo.
def var bpr06   as char     format "x(02)"  LABEL "Snd ID Qual"         no-undo.
def var bpr07   as char     format "x(12)"  label "Snd ID#"             no-undo.
def var bpr08   as char     format "x(03)"  label "Snd Acct# Qual"      no-undo.
def var bpr09   as char     format "x(35)"  label "Snd Acct#"           no-undo.
def var bpr10   as char     format "x(10)"  label "Orig Company"        no-undo.
def var bpr11   as char     format "x(09)"  label "Orig Supplemental"   no-undo.
def var bpr12   as char     format "x(02)"  label "Rcv ID Qual"         no-undo.
def var bpr13   as char     format "x(12)"  label "Rcv ID"              no-undo.
def var bpr14   as char     format "x(03)"  label "Rcv Acct# Qual"      no-undo.
def var bpr15   as char     format "x(35)"  label "Rcv Acct#"           no-undo.
def var bpr16   as char                     label "Settlement Date"     no-undo.
def var bpr17   as char     no-undo.
def var bpr18   as char     no-undo.
def var bpr19   as char     no-undo.
def var bpr20   as char     no-undo.
def var bpr21   as char     no-undo.
IF ws_segment <> "BPR" THEN
RETURN error.
IF command matches "*I*" THEN
DO:
    CASE ws_version:
    OTHERWISE DO:
  ASSIGN
        {rc/substr.i    bpr01       18  02}
        {ed/tdf/substrde.i bpr02    20  19}
        {rc/substr.i    bpr03       39  01}
        {rc/substr.i    bpr04       40  03}
        {rc/substr.i    bpr05       43  10}
        {rc/substr.i    bpr06       53  02}
        {rc/substr.i    bpr07       55  12}
        {rc/substr.i    bpr08       67  03}
        {rc/substr.i    bpr09       70  35}
        {rc/substr.i    bpr10      105  10}
        {rc/substr.i    bpr11      115  09}
        {rc/substr.i    bpr12      124  02}
        {rc/substr.i    bpr13      126  12}
        {rc/substr.i    bpr14      138  03}
        {rc/substr.i    bpr15      141  35}
        {rc/substr.i    bpr16      176  08}
        {rc/substr.i    bpr17      184  03}
        {rc/substr.i    bpr18      187  02}
        {rc/substr.i    bpr19      189  12}
        {rc/substr.i    bpr20      201  03}
        {rc/substr.i    bpr21      204  35}
        .
        
    end.
    END CASE.
    if bpr03 = "C" then bpr02 = -1 * bpr02.
    {rc/xyymmdd.i bpr16 extra_date#}         
END.    /* I */
ELSE
IF command matches "*O*" THEN
DO:
    bpr16 = if extra_date# = ? then ""
    else 
       (string(year(extra_date#),"9999")
        + string(month(extra_date#),"99")
        + string(day(extra_date#),"99") ).
        
    CASE ws_version:
    OTHERWISE DO:
  ASSIGN
        {rc/outstr.i    bpr01       18  02}
        {ed/tdf/outstrde.i bpr02    20  19 2}
        {rc/outstr.i    bpr03       39  01}
        {rc/outstr.i    bpr04       40  03}
        {rc/outstr.i    bpr05       43  10}
        {rc/outstr.i    bpr06       53  02}
        {rc/outstr.i    bpr07       55  12}
        {rc/outstr.i    bpr08       67  03}
        {rc/outstr.i    bpr09       70  35}
        {rc/outstr.i    bpr10      105  10}
        {rc/outstr.i    bpr11      115  09}
        {rc/outstr.i    bpr12      124  02}
        {rc/outstr.i    bpr13      126  12}
        {rc/outstr.i    bpr14      138  03}
        {rc/outstr.i    bpr15      141  35}
        {rc/outstr.i    string(bpr16) 176  08}
        {rc/outstr.i    bpr17      184  03}
        {rc/outstr.i    bpr18      187  02}
        {rc/outstr.i    bpr19      189  12}
        {rc/outstr.i    bpr20      201  03}
        {rc/outstr.i    bpr21      204  35}
    .
    END.
    END CASE.
END.    /* O */
if command matches "*P*" then do:
    display stream s-out
 ws_segment    
 bpr01
 bpr02
/*  bpr03 */
 bpr04
 bpr05
 bpr07  space(0) '/' space(0) bpr06 no-label
 bpr09  space(0) '/' space(0) bpr08 no-label
 bpr10
/*  bpr11 */
 bpr13  space(0) '/' space(0) bpr12 no-label
 bpr15  space(0) '/' space(0) bpr14 no-label
 extra_date# label "Settlement date"
/*
 bpr16
 bpr17
 bpr18
 bpr19
 bpr20
 bpr21
 */
    with no-box side-labels width 144.
        
end.

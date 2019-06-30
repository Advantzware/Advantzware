

/*------------------------------------------------------------------------
    File        : StyleReport.p
    Purpose     : Style File
    Main File   : cerep\r-style.w
    Syntax      :

    Description : Return a Dataset of Request For Style

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttStyleFileReport NO-UNDO
        FIELD style-file AS CHAR.

    DEFINE DATASET dsStyleFileReport FOR ttStyleFileReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmCor             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFold            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStyleFileReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


IF  prmUser      = ?        THEN ASSIGN     prmUser        = "".
IF  prmCor       = ?        THEN ASSIGN     prmCor         = "".
IF  prmFold      = ?        THEN ASSIGN     prmFold        = "".
IF  prmOutexcel  = ?        THEN ASSIGN     prmOutexcel    = "".
    
    def var list-name as cha no-undo.
    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE tb_corr AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_fold AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEF STREAM excel.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp
 locode = "MAIN" 
 v-today = TODAY . 

    
    ASSIGN
      tb_corr = IF prmCor = "True" THEN TRUE ELSE FALSE
      tb_fold = IF prmFold = "True" THEN TRUE ELSE FALSE 
      tb_excel =  IF prmOutexcel = "Yes" THEN TRUE ELSE FALSE .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "style" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "style" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "style" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  

        
  IF prmAction = "style" THEN DO:
    
      run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttStyleFileReport.
        ASSIGN ttStyleFileReport.style-file = excel-file.
    END.
    ELSE DO:
        CREATE ttStyleFileReport.
        ASSIGN ttStyleFileReport.style-file = vtextfile.
    END.

  END.

  
    /*****************************************PROCEDURE run-report :*****************************************************/
 PROCEDURE run-report :
{sys/form/r-top.f}

def var head as ch format "x(78)" extent 2.

FORM
  " STYLE No:" style.style  "Description:" to 45 style.dscr skip
head[1]
skip
" Glue Lap" style.dim-gl at 13  "Board" at 25 style.material[1] at 37
    "1" at 52 style.m-code[1] style.m-dscr[1] skip
" Tuck    " style.dim-tk at 13  "Ink"   at 25 style.material[2] at 37
    "2" at 52 style.m-code[2] style.m-dscr[2] skip
" DK Length" style.dim-dkl at 13 "Ink Cov %" at 25 style.material[3] at 37
    "3" at 52 style.m-code[3] style.m-dscr[3] skip
" DK Width" style.dim-dkw at 13 "Film" at 25 style.material[4] at 37
    "4" at 52 style.m-code[4] style.m-dscr[4] skip
" 5th Panel" style.dim-pan5 at 13 "Leaf" at 25 style.material[5] at 37
    "5" at 52 style.m-code[5] style.m-dscr[5] skip
" Fit"      style.dim-fit at 13 "Coating" at 25 style.material[6] at 37
    "6" at 52 style.m-code[6] style.m-dscr[6] skip

                  "Adhesive" at 25 style.material[7] at 37
                    "7" at 52 style.m-code[7] style.m-dscr[7] skip
                  /*
                  "Misc" at 25 style.material[8] at 37
                    "8" at 52 style.m-code[8] style.m-dscr[8] skip
                 */
head[2]  skip

" 1) Lower Left W 1." style.formula[1]  format "x(20)"
  "5) Lid Len  9.="   at 41 style.formula[9]  format "x(20)" skip
"               L 2." style.formula[2]  format "x(20)"
     "Lid Wid 10.="   at 44 style.formula[10] format "x(20)" skip
" 2) Nesting    W 3." style.formula[3]  format "x(20)"
     "Lid Die 11.="   at 44 style.formula[11]  format "x(20)" skip
"               L 4." style.formula[4]  format "x(20)"
     "Box Die 12.="   at 44 style.formula[12] format "x(20)" skip
" 3) Stagger    W 5." style.formula[5]  format "x(20)"
    "# on Wid" at 41  "2 3 4 5 6 7 8 9 10 11 12 13" skip
"               L 6." style.formula[6]  format "x(20)"
    " Formula" at 41 style.use-w[2 for 8] format ">" space(2)
                     style.use-w[10] space(2) style.use-w[11] space(2)
                     style.use-w[12] space(2) style.use-w[13] skip
" 4) Sq Inches  W 7." style.formula[7]  format "x(20)"
    "# on Len" at 41 "2 3 4 5 6 7 8 9 10 11 12 13" skip
"               L 8." style.formula[8]  format "x(20)"
    " Formula" at 41 style.use-l[2 for 8] space(2)
                     style.use-l[10] space(2) style.use-l[11] space(2)
                     style.use-l[12] space(2) style.use-l[13]
  WITH FRAME style OVERLAY NO-LABELS row 2 no-attr-space no-box
             stream-io width 80.

assign
 str-tit2 = "Style File"
 str-tit2 = fill(" ",int((56 - length(trim(str-tit2))) / 2)) + trim(str-tit2)
 
 head[1]  = " DEFAULT  DIMENSIONS    DEFAULT MATERIAL CODES     DEFAULT MACHINE ROUTING".
 head[2]  = " = Single Item Straight Path FORMULAS =  ===== Two Piece Box FORMULAS =====".


if tmp-dir = "" then tmp-dir = v-webrootpath .

assign list-name = tmp-dir + vtextfile . 
       

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

/* gdm - 10130801 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    EXPORT STREAM excel DELIMITER ","
        "STYLE No" "Description" "Glue Lap" "Tuck" "DK Length" "DK Width" 
        "5th Panel" "Fit" "Board" "Ink" "Ink Cov %" "Film" "Leaf" "Coating" 
        "Adhesive" "1" "2" "3" "4" "5" "6" "7" "Lower Left W" "Lower Left L" 
        "Nesting W" "Nesting L" "Stagger W" "Stagger L" "Sq Inches W" 
        "Sq Inches L" "Lid Len" "Lid Wid" "Lid Die" "Box Die" "# on Wid" 
        "Formula" "# on Len" "Formula" 
        SKIP.

END.

display str-tit with frame r-top.

/*display skip(1).*/


for each style
    where (style.company eq cocode)
      and ((style.industry eq "1" and tb_fold) or
           (style.industry eq "2" and tb_corr))
    with frame style:
  display skip(2).
  {sys/ref/style.v}
  down.
  if line-counter gt 35 then page.

  /* gdm - 10130801 */
  IF tb_excel THEN DO:

    EXPORT STREAM excel DELIMITER ','
        style.style style.dscr style.dim-gl style.dim-tk style.dim-dkl 
        style.dim-dkw style.dim-pan5 style.dim-fit style.material[1] 
        style.material[2] style.material[3] style.material[4] style.material[5] 
        style.material[6] style.material[7]                                               
        STRING(STRING(style.m-code[1]) +  " " + STRING(style.m-dscr[1]))
        STRING(STRING(style.m-code[2]) +  " " + STRING(style.m-dscr[2]))
        STRING(STRING(style.m-code[3]) +  " " + STRING(style.m-dscr[3]))
        STRING(STRING(style.m-code[4]) +  " " + STRING(style.m-dscr[4]))
        STRING(STRING(style.m-code[5]) +  " " + STRING(style.m-dscr[5]))
        STRING(STRING(style.m-code[6]) +  " " + STRING(style.m-dscr[6]))
        STRING(STRING(style.m-code[7]) +  " " + STRING(style.m-dscr[7]))
        style.formula[1] style.formula[2] style.formula[3] style.formula[4]
        style.formula[5] style.formula[6] style.formula[7] style.formula[8]
        style.formula[9] style.formula[10] style.formula[11] style.formula[12]
        "2 3 4 5 6 7 8 9 10 11 12 13"
        STRING(string(style.use-w[2])  + ' '  + string(style.use-w[3])  + ' '  + 
               string(style.use-w[4])  + ' '  + string(style.use-w[5])  + ' '  + 
               string(style.use-w[6])  + ' '  + string(style.use-w[7])  + ' '  +
               string(style.use-w[8])  + ' '  + string(style.use-w[9])  + ' '  + 
               string(style.use-w[10]) + '   '   + 
               string(style.use-w[11]) + '    '  + 
               string(style.use-w[12]) + '   '   + 
               string(style.use-w[13]) )
        "2 3 4 5 6 7 8 9 10 11 12 13"
        STRING(string(style.use-l[2]) + ' '  + string(style.use-l[3]) + ' '  + 
               string(style.use-l[4]) + ' '  + string(style.use-l[5]) + ' '  + 
               string(style.use-l[6]) + ' '  + string(style.use-l[7]) + ' '  +
               string(style.use-l[8]) + ' '  + string(style.use-l[9]) + ' '  + 
               string(style.use-l[10]) + '   '   +
               string(style.use-l[11]) + '    '  + 
               string(style.use-l[12]) + '   '   + 
               string(style.use-l[13]) ).
    
  END.
  /*IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
  END.*/

end.


end procedure.

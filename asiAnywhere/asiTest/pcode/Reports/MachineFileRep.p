

/*------------------------------------------------------------------------
    File        : MachineFileRep.p
    Purpose     : Machine File
    Main File   : cerep\r-mach.w
    Syntax      :

    Description : Return a Dataset of Request For Machine

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttMachineFileReport NO-UNDO
        FIELD vFile AS CHAR
        FIELD dfjdskj AS CHAR
        .

    DEFINE DATASET dsMachineFileReport FOR ttMachineFileReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegMachine      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndMachine      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDeptCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDeptCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmShowStandard    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachineFileReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegMachine   = ?        THEN ASSIGN     prmBegMachine   = "".
    IF  prmEndMachine   = ?        THEN ASSIGN     prmEndMachine   = "".
    IF  prmBegDeptCode  = ?        THEN ASSIGN     prmBegDeptCode  = "".
    IF  prmEndDeptCode  = ?        THEN ASSIGN     prmEndDeptCode  = "".
    IF  prmShowStandard = ?        THEN ASSIGN     prmShowStandard = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut     = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
    def new shared var sp-dscr as ch format "x(25)" extent 2.
    def new shared var head1   as ch format "x(78)" init
      "   STANDARDS:  X axis (Columns) ===========  Y Axis (Rows) =============".

    def new shared var head as ch format "x(78)" extent 4.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VARIABLE begin_dept AS CHARACTER FORMAT "X(2)" NO-UNDO.
    DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(6)" NO-UNDO.
    DEFINE VARIABLE end_dept AS CHARACTER FORMAT "X(2)" INITIAL "zz" NO-UNDO.
    DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(6)" INITIAL "zzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_show-stds AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99.  
    DEF STREAM excel.



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp.


 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 
 ASSIGN    
 v-today = TODAY . 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "MachineFileRep" THEN DO:
    ASSIGN 
        begin_dept  = prmBegDeptCode
        begin_mach  = prmBegMachine 
        end_dept    = prmEndDeptCode 
        end_mach    = prmEndMachine. 


    ASSIGN
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_show-stds  = IF prmShowStandard = "yes" THEN TRUE ELSE FALSE  
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "machfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "machfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "machfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttMachineFileReport.
        ASSIGN ttMachineFileReport.vFile = excel-file.
    END.
    ELSE DO:
        CREATE ttMachineFileReport.
        ASSIGN ttMachineFileReport.vFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
 PROCEDURE run-report :
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
time_stamp = string(time, "hh:mmam").

def var fmac like mach.m-code NO-UNDO.
def var tmac like fmac init "zzzzzz" NO-UNDO.
def var fdep like est-op.dept NO-UNDO.
def var tdep like fdep init "zz" NO-UNDO.
def var sho-stds as log format "Y/N" init NO NO-UNDO.
DEF VAR lv-need-display AS LOG NO-UNDO.

/* gdm - 10130802 */
DEF VAR v_head     AS CHAR NO-UNDO.
DEF VAR v_numlw    AS CHAR NO-UNDO.
DEF VAR v_col-pass AS CHAR NO-UNDO.
DEF VAR v_coater   AS CHAR NO-UNDO.

head[1] =
" =============================  R  A  T  E  S  =============================  "
.
head[2] = "  ==== LIMITS     Min       Max ==="   .
head[3] = "==========  Printing Press  =========" .


{sys/form/r-top.f}
{sys/ref/mstd.f}
{sys/ref/mmtx.f}
{sys/ref/mmtx2.f}
{sys/ref/mmty.f}
{sys/ref/mach.i}

format
  "Code:" to 7 mach.m-code
  "Desc:" to 33 mach.m-dscr
  "Rigid:" to 72 mach.setup skip

  "Loc:" to 7 mach.loc
  "Dept:" to 33 mach.dept[1] mach.dept[2] mach.dept[3] mach.dept[4]
  "Sequence:" to 57 mach.d-seq mach.m-seq
  "Fold:" to 72 mach.fc skip

  "Feed:" to 7 space(1) mach.p-type
  "Run Spoil. %:" to 33 space(1) mach.run-spoil
  "MR Waste:" to 57 mach.mr-waste
  "Corr:" to 72 mach.corr skip

  /* "Die Rule :"     to 33 mach.die-cost
  "Die Mat'l:"     to 57 mach.matl-cost */
  "Therm:" to 72 mach.therm skip

  head[1] format "x(78)"

  /* line 4 */
  " LABOR Rate1:" mach.lab-rate[1] FORM ">>>9.99"
  "Market:" to 31 mach.mrk-rate    FORM ">>>9.99"
  "MR D.L.:" to 48 mach.mr-rate    FORM ">>>9.99"
  "RUN D.L.:" to 69 mach.run-rate FORM ">>>9.99" skip

  "       Rate2:" mach.lab-rate[2] FORM ">>>9.99"
  "MR  Crew:" to 31 space(2) mach.mr-crusiz FORM ">>9.99"
  "Var OH:" to 48 mach.mr-varoh     FORM ">>>9.99"
  "Var OH:" to 69 mach.run-varoh FORM ">>>9.99" skip

  "       Rate3:" mach.lab-rate[3] FORM ">>>9.99"
  "RUN Crew:" to 31 space(2) mach.run-crusiz FORM ">>9.99"
  "Fix OH:" to 48 mach.mr-fixoh  FORM ">>>9.99"
  "Fix OH:" to 69 mach.run-fixoh FORM ">>>9.99" skip

  "     Default:" mach.lab-drate
  "   TOTAL:" to 48 mach.mr-trate FORM ">>>9.99"
  "TOTAL:" to 69 mach.run-trate FORM ">>>9.99" skip(1)

  head[2] format "x(35)" head[3] format "x(37)" AT 38
  v-label[1]   to 12 mach.min-len to 23  mach.max-len to 35
  "Printer Type:" to 54 space(2) mach.pr-type
  v-label[2]   to 12 mach.min-wid to 23  mach.max-wid to 35
  "Washup Hours:" to 54 mach.washup "Col/Pass:" to 70 mach.col-pass
  v-label[3]   to 12 mach.min-triml to 23
  "Max # colors:" to 54 space(1) mach.max-color
  "Coater On:" to 70 mach.coater
  v-label[4]   to 12 mach.min-trimw to 23
  " MR Waste/color:" to 54 space(2) mach.col-wastesh
  "Caliper:"   to 12 space(3) mach.min-cal to 23  mach.max-cal to 35
  " INK Waste Lbs/MR:" to 54 mach.ink-waste "Lbs/Col:" mach.col-wastelb
  "Run Qty:"   to 12 mach.min-run   mach.max-run to 35
  " Tandem MR/Plate :" to 54 mach.Tan-MRP   " /Fountn:" mach.tan-MRF
  skip(2)
  with frame mach overlay no-labels width 90 STREAM-IO
  /*color value(col-bg) prompt-for value(col-input)*/ .

str-tit  = coname + " - " + loname.
str-tit2 = "Machines List".
str-tit3 = "".
x = (56 - length(str-tit)) / 2.
str-tit  = fill(" ",x) + str-tit .
x = (56 - length(str-tit2)) / 2.
str-tit2 = fill(" ",x) + str-tit2 .
x = (80 - length(str-tit3)) / 2.
str-tit3 = fill(" ",x) + str-tit3 .
ASSIGN fmac     = begin_mach
       tmac     = end_mach
       fdep     = begin_dept
       tdep     = end_dept
       sho-stds = tb_show-stds.

    /*{sys/inc/print1.i}*/

    if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

    {sys/inc/outprint.i value(lines-per-page)}

    /*if td-show-parm then run show-param.*/

/*display str-tit with frame r-top.*/
    view frame r-top.

/* gdm - 10130802 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED
        "Company Name,Warehouse - Descr,Code,Loc,Feed,Desc,Dept,Run Spoil. %,Sequence," 
        "MR Waste,Rigid,Fold,Corr,Therm,LABOR Rate1,Rate2,Rate3,Default," 
        "Market,MR Crew,RUN Crew,MR D.L.,Var OH,Fix OH,TOTAL,RUN D.L.," 
        "Var OH,Fix OH,TOTAL,Lngth Min,Lngth Max,Width Min,Width Max," 
        "Trim Len,Trim Wid,Caliper Min,Caliper Max,Run Qty Min,Run Qty Max,"
        "Printer Type,Washup Hours,Max # colors,MR Waste/color,INK Waste Lbs/MR," 
        "Tandem MR/Plate,Col/Pass,Coater On,Lbs/Col,/Fountn,Panel Len,Panel Wid,"
        "Sheets/Run,Blanks/Run,Lin.Ft/Run"
     SKIP.

END.    

    for each mach
        where mach.company  eq cocode
          and mach.loc      eq locode
          and mach.m-code   ge fmac
          and mach.m-code   le tmac
          and mach.dept[1]  ge fdep
          and mach.dept[1]  le tdep
        no-lock,
        first company
        where company.company eq mach.company
        no-lock,
        first loc
        where loc.company eq mach.company
          and loc.loc     eq mach.loc
        no-lock with frame mach BREAK BY mach.m-code:
        
      clear frame mach all no-pause.
      /*if line-counter gt 40 then page.     */
      IF FIRST-OF(mach.m-code) THEN PAGE.

      put "  Company: " mach.company space(5) company.name  skip
          "Warehouse: " loc.loc      space(3) loc.dscr      skip.
      {sys/ref/mach.v}
      down.

      /* gdm - 10130802 */
      ASSIGN 
       v_numlw = ""
       v_numlw = IF mach.num-len EQ 0 THEN "" ELSE STRING(mach.num-len)
       v_numlw = v_numlw + " / " +
                 IF mach.num-wid EQ 0 THEN "" ELSE STRING(mach.num-wid)
       v_numlw = IF TRIM(v_numlw) = "/" THEN "" ELSE v_numlw
       v_col-pass = IF STRING(TRIM(mach.col-pass)) = ? 
                      THEN '' ELSE STRING(mach.col-pass) 
       v_coater = IF mach.coater = ? THEN '' ELSE STRING(mach.coater).

      IF tb_excel THEN DO:
          PUT STREAM excel UNFORMATTED
              '"' STRING(mach.company + ' - ' + STRING(company.name)) '",'
              '"' String(string(loc.loc) + ' - ' + STRING(loc.dscr))  '",'
              '"' REPLACE(mach.m-code,'"','')         '",'
              '"' REPLACE(mach.loc,'"','')            '",'
              '"' mach.p-type                         '",'
              '"' REPLACE(mach.m-dscr,'"','')         '",'
              '"' STRING(mach.dept[1] + " " + mach.dept[2] + " " + 
                         mach.dept[3] + " " + mach.dept[4])  '",'
              '"' mach.run-spoil                      '",'
              '"' mach.d-seq mach.m-seq               '",'
              '"' mach.mr-waste                       '",'
              '"' mach.setup                          '",'
              '"' mach.fc                             '",'
              '"' mach.corr                           '",'
              '"' mach.therm                          '",'
              '"' STRING(mach.lab-rate[1],">>>9.99")  '",'
              '"' STRING(mach.lab-rate[2],">>>9.99")  '",'
              '"' STRING(mach.lab-rate[3],">>>9.99")  '",'
              '"' STRING(mach.lab-drate,">>>9.99")    '",'
              '"' STRING(mach.mrk-rate,">>>9.99")     '",'
              '"' STRING(mach.mr-crusiz,">>9.99")     '",'
              '"' STRING(mach.run-crusiz,">>9.99")    '",'
              '"' STRING(mach.mr-rate,">>>9.99")      '",'
              '"' STRING(mach.mr-varoh,">>>9.99")     '",'
              '"' STRING(mach.mr-fixoh,">>>9.99")     '",'
              '"' STRING(mach.mr-trate,">>>9.99")     '",'
              '"' STRING(mach.run-rate,">>>9.99")     '",'
              '"' STRING(mach.run-varoh,">>>9.99")    '",'
              '"' STRING(mach.run-fixoh,">>>9.99")    '",'
              '"' STRING(mach.run-trate,">>>9.99")    '",'
              '"' mach.min-len                        '",'
              '"' mach.max-len                        '",'
              '"' mach.min-wid                        '",'
              '"' mach.max-wid                        '",'
              '"' mach.min-triml                      '",'
              '"' mach.min-trimw                      '",'
              '"' mach.min-cal                        '",'
              '"' mach.max-cal                        '",'
              '"' mach.min-run                        '",'
              '"' mach.max-run                        '",'
              '"' mach.pr-type                        '",'
              '"' mach.washup                         '",'
              '"' mach.max-color                      '",'
              '"' mach.col-wastesh                    '",'
              '"' mach.ink-waste                      '",'
              '"' mach.Tan-MRP                        '",'
              '"' v_col-pass                          '",'
              '"' v_coater                            '",'
              '"' mach.col-wastelb                    '",'
              '"' mach.tan-MRF                        '",'
              '"' mach.min-pan-l                      '",'
              '"' mach.min-pan-w                      '",'.

          IF mach.p-type eq "B"  
            THEN 
              PUT STREAM excel UNFORMATTED
              "," v_numlw SKIP.
            ELSE 
             IF mach.therm AND mach.p-type EQ "R" 
               THEN 
                 PUT STREAM excel UNFORMATTED
                 ",," v_numlw SKIP.
               ELSE 
                 PUT STREAM excel UNFORMATTED
                     v_numlw SKIP.

      END.

      if sho-stds then do:
        
        for each mstd of mach with frame mstd:
          clear frame mstd all no-pause.
          
          /*if line-counter gt 40 then page.*/
          display skip(1).
          find first dept
              where dept.company eq cocode
                and dept.code    eq mstd.dept
              no-lock no-error.
          find first style
             where style.company eq cocode
               and style.style   eq mstd.style
             no-lock no-error.
          display mstd.m-code
                  mach.m-dscr when avail mach
                  mstd.dept
                  dept.dscr   when avail dept
                  mstd.style FORM "x(6)"
                  style.dscr when avail style.
          find std-code where std-code.code eq string(mr-x,"99") no-lock no-error.
          display mr-x std-code.dscr when avail std-code @ mx-dscr[1].
          find std-code where std-code.code eq string(mr-y,"99") no-lock no-error.
          display mr-y std-code.dscr when avail std-code @ mx-dscr[2].
          find std-code where std-code.code eq string(rs-x,"99") no-lock no-error.
          display rs-x std-code.dscr when avail std-code @ mx-dscr[3].
          find std-code where std-code.code eq string(rs-y,"99") no-lock no-error.
          display rs-y std-code.dscr when avail std-code @ mx-dscr[4].
          find std-code where std-code.code eq string(sp-x,"99") no-lock no-error.
          display sp-x std-code.dscr when avail std-code @ sp-dscr[1].
          find std-code where std-code.code eq string(sp-y,"99") no-lock no-error.
          display sp-y std-code.dscr when avail std-code @ sp-dscr[2].

/*
          display run-qty[1 for 9]
                  X-sheets[1 for 9]
                  board-cal[1 for 9]
                  spd-reduc[1 for 9].
*/
          run sys/ref/mach-1.p (recid(mstd)).

          for each mmty of mstd with frame mmty:
            clear frame mmty all no-pause.
            /*if line-counter gt 55 then */ page.
            display mmty.m-code
                    mach.m-dscr
                    mmty.style
                    style.dscr  when avail style
                    mmty.c-title
                    mmty.rtit[1 for 15].
            do i = 1 to 15:
              display mmty.row-value[i] when mmty.row-value[i] ne 0.
            end.
            do i = 1 to 10:
              display mmty.col-value[i] when mmty.col-value[i] ne 0
                      mmty.vals[10  + i] when mmty.vals[10  + i] ne 0
                      mmty.vals[20  + i] when mmty.vals[20  + i] ne 0
                      mmty.vals[30  + i] when mmty.vals[30  + i] ne 0
                      mmty.vals[40  + i] when mmty.vals[40  + i] ne 0
                      mmty.vals[50  + i] when mmty.vals[50  + i] ne 0
                      mmty.vals[60  + i] when mmty.vals[60  + i] ne 0
                      mmty.vals[70  + i] when mmty.vals[70  + i] ne 0
                      mmty.vals[80  + i] when mmty.vals[80  + i] ne 0
                      mmty.vals[90  + i] when mmty.vals[90  + i] ne 0
                      mmty.vals[100 + i] when mmty.vals[100 + i] ne 0
                      mmty.vals[110 + i] when mmty.vals[110 + i] ne 0
                      mmty.vals[120 + i] when mmty.vals[120 + i] ne 0
                      mmty.vals[130 + i] when mmty.vals[130 + i] ne 0
                      mmty.vals[140 + i] when mmty.vals[140 + i] ne 0
                      mmty.vals[150 + i] when mmty.vals[150 + i] ne 0.
            end.
            
            down.
          end.
          
          for each mmtx of mstd with frame mmtx :
          /*  clear frame mmtx all no-pause. */
        
            if line-counter gt 55 then page.

            lv-need-display = IF mmtx.page-no = 0 AND mmtx.across-no = 0 THEN YES ELSE NO.
            IF mmtx.page-no <> 0 or mmtx.across-no <> 0 THEN DO:
                 DO i = 11 TO 160:
                    IF mmtx.vals[i] <> 0 THEN lv-need-display = YES.
                    IF lv-need-display THEN LEAVE.
                 END.
            END.

            IF lv-need-display THEN DO:
               display mmtx.m-code
                    mach.m-dscr
                    mmtx.style
                    style.dscr  when avail style
                    mmtx.mr-run when not mr-run
                    "SPOIL" when mr-run @ mmtx.mr-run
                    mmtx.c-title
                    with frame mmtx.
               do i = 1 to 15:
                  display mmtx.rtit[i] mmtx.row-value[i] when mmtx.row-value[i] ne 0
                  with frame mmtx.
               END.
            end.
            /* The following is for RUN matrix's only */
            if not mr-run then
            do:
                IF lv-need-display THEN do i = 1 to 10:
                display mmtx.col-value[i] mmtx.vals[10 + i]
                        mmtx.vals[20 + i]
                        mmtx.vals[30 + i]
                        mmtx.vals[40 + i]
                        mmtx.vals[50 + i]
                        mmtx.vals[60 + i]
                        mmtx.vals[70 + i]
                        mmtx.vals[80 + i]
                        mmtx.vals[90 + i]
                        mmtx.vals[100 + i]
                        mmtx.vals[110 + i]
                        mmtx.vals[120 + i]
                        mmtx.vals[130 + i]
                        mmtx.vals[140 + i]
                        mmtx.vals[150 + i]
                    with frame mmtx.
                
              end.
            /*  run sys/ref/mach-1.p (recid(mstd)). */
            end.
            else
            /* This is for RUN SPOILAGE matrix's only */
            do i = 1 to 10:
              display mmtx.col-value[i] when mmtx.col-value[i] ne 0
                      mmtx.vals[10 + i] when mmtx.vals[10 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[10 + i]
                      mmtx.vals[20 + i] when mmtx.vals[20 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[20 + i]
                      mmtx.vals[30 + i] when mmtx.vals[30 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[30 + i]
                      mmtx.vals[40 + i] when mmtx.vals[40 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[40 + i]
                      mmtx.vals[50 + i] when mmtx.vals[50 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[50 + i]
                      mmtx.vals[60 + i] when mmtx.vals[60 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[60 + i]
                      mmtx.vals[70 + i] when mmtx.vals[70 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[70 + i]
                      mmtx.vals[80 + i] when mmtx.vals[80 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[80 + i]
                      mmtx.vals[90 + i] when mmtx.vals[90 + i] ne 0
                        format ">>9.99"  @ mmtx.vals[90 + i]
                      mmtx.vals[100 + i] when mmtx.vals[100 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[100 + i]
                      mmtx.vals[110 + i] when mmtx.vals[110 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[110 + i]
                      mmtx.vals[120 + i] when mmtx.vals[120 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[120 + i]
                      mmtx.vals[130 + i] when mmtx.vals[130 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[130 + i]
                      mmtx.vals[140 + i] when mmtx.vals[140 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[140 + i]
                      mmtx.vals[150 + i] when mmtx.vals[150 + i] ne 0
                        format ">>9.99"   @ mmtx.vals[150 + i]
                  with frame mmtx.
            end.
           
            IF lv-need-display OR mmtx.mr-run THEN down.      
          end.
          PUT SKIP(2).
        end.

        /*page.*/
      end.
    end.
    /* gdm - 10130802 */
    /*IF tb_excel THEN DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
    END.

    OUTPUT CLOSE.

    /* gdm - 10130802 */
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    */

end procedure.

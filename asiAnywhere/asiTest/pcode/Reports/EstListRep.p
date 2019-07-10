

/*------------------------------------------------------------------------
    File        : EstListRep.p
    Purpose     : Estimates List
    Main File   : cerep\r-est.w
    Syntax      :

    Description : Return a Dataset of Request For Estimates

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttEstListReport NO-UNDO
        FIELD vFile AS CHAR
        FIELD lfjoovqvsj AS CHAR
        .

    DEFINE DATASET dsEstListReport FOR ttEstListReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegAddDate      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndAddDate      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegModDate      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndModDate      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegEst          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndEst          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegMach         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndMach         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBreak           AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmBooked          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmNotBooked       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSort            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstListReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegCustomer  = ?        THEN ASSIGN     prmBegCustomer  = "".
    IF  prmEndCustomer  = ?        THEN ASSIGN     prmEndCustomer  = "".
    IF  prmBegAddDate   = ?        THEN ASSIGN     prmBegAddDate   = "".
    IF  prmEndAddDate   = ?        THEN ASSIGN     prmEndAddDate   = "".
    IF  prmBegModDate   = ?        THEN ASSIGN     prmBegModDate   = "".
    IF  prmEndModDate   = ?        THEN ASSIGN     prmEndModDate   = "".
    IF  prmBegEst       = ?        THEN ASSIGN     prmBegEst       = "".
    IF  prmEndEst       = ?        THEN ASSIGN     prmEndEst       = "".
    IF  prmBegMach      = ?        THEN ASSIGN     prmBegMach      = "".
    IF  prmEndMach      = ?        THEN ASSIGN     prmEndMach      = "".
    IF  prmBegSalrep    = ?        THEN ASSIGN     prmBegSalrep    = "".
    IF  prmEndSalrep    = ?        THEN ASSIGN     prmEndSalrep    = "".
    IF  prmBreak        = ?        THEN ASSIGN     prmBreak        = "".
    IF  prmBooked       = ?        THEN ASSIGN     prmBooked       = "".
    IF  prmNotBooked    = ?        THEN ASSIGN     prmNotBooked    = "".
    IF  prmSort         = ?        THEN ASSIGN     prmSort         = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut          = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEF TEMP-TABLE tt-eb  FIELD m-code LIKE est-op.m-code
                      FIELD row-id AS ROWID
                      INDEX m-code m-code.

    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_date-2 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_est AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_mach AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_date-2 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_est AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_mach AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE tb_booked AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_break AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_not-booked AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no NO-UNDO.    
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.
    DEFINE VAR custcount AS CHAR NO-UNDO.


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


 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCustomer NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCustomer  OR prmEndCustomer = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
     usercust.company = prmComp  NO-LOCK:
    ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "EstListRep" THEN DO:
    ASSIGN 
        begin_cust-no  = prmBegCustomer                
        begin_date     = datetime(prmBegAddDate)        
        begin_date-2   = datetime(prmBegModDate)                   
        begin_est      = prmBegEst                 
        begin_mach     = prmBegMach           
        begin_slsmn    = prmBegSalrep                 
        end_cust-no    = prmEndCustomer                      
        end_date       = datetime(prmEndAddDate)                                     
        end_date-2     = datetime(prmEndModDate)                                      
        end_est        = prmEndEst                                    
        end_mach       = prmEndMach                                  
        end_slsmn      = prmEndSalrep                                  
        .

    ASSIGN
        tb_excel        = IF prmOut = "yes" THEN TRUE ELSE FALSE               
        tb_booked       = IF prmBooked = "yes" THEN TRUE ELSE FALSE         
        tb_break        = IF prmBreak = "yes" THEN TRUE ELSE FALSE                                                 
        tb_not-booked   = IF prmNotBooked = "yes" THEN TRUE ELSE FALSE                                                     
        tb_sort         = IF prmSort = "yes" THEN TRUE ELSE FALSE
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "estlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) +  ".csv".  
        excel-file   = "estlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "estlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttEstListReport.
        ASSIGN ttEstListReport.vFile = excel-file.
    END.
    ELSE DO:
        CREATE ttEstListReport.
        ASSIGN ttEstListReport.vFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var fcust like eb.cust-no init "".
def var tcust like fcust init "zzzzzzzz".
def var fsman like eb.sman init "" no-undo.
def var tsman like fsman init "zzz" no-undo.
def var fdate like est.mod-date format "99/99/9999" init 01/01/0001.
def var tdate like fdate init today.
def var fest  like est.est-no init "" no-undo.
def var test  like fest init "zzzzz" no-undo.

def var v-blk-dim as char no-undo.
def var v-booked  as char no-undo.
def var v-qty     as int  no-undo.
def var v-b-qty   as int  no-undo.

def var x as int no-undo.
def var xxx as dec no-undo.
DEF VAR lv-hdr-m-dscr AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.

/* gdm - 10130804 */
DEF VAR v_est-no LIKE eb.est-no    NO-UNDO.
DEF VAR v_custnm LIKE cust.name    NO-UNDO.
DEF VAR v_shpnm  LIKE eb.ship-name NO-UNDO.
DEF VAR v_stydsc LIKE style.dscr   NO-UNDO.
DEF VAR v_i-name LIKE item.i-name  NO-UNDO.

DEF VAR v_blksz    AS CHAR FORMAT 'x(25)'      NO-UNDO.
DEF VAR v_mod-date AS CHAR FORMAT '99/99/9999' NO-UNDO.

FORM HEADER
     "Machine:"
     lv-hdr-m-dscr
     SKIP(1)
      
    WITH FRAME r-top2 NO-LABELS NO-BOX WIDTH 132 STREAM-IO NO-UNDERLINE PAGE-TOP.

form skip(1)
     eb.est-no                      format "x(8)"
     cust.name                    at 10
     eb.part-no                    at 41
     style.dscr                     at 72
     v-blk-dim format "x(27)" at 99
     v-booked                      to 138 format "x(6)"
     skip
     "Last used:"                 at 10
     est.mod-date
     eb.part-dscr1                at 41
     eb.t-wid                       at 72                                       
     " x "                            at 82                                       
     eb.t-len                        at 85                                       
     "Print:"                        at 99
     eb.i-coldscr skip    
     eb.part-dscr2               at 41
     "Board:" at 99
     item.i-name format "x(26)"
     skip
header "Est#     Customer Name                  Part # / Description           Style/Blank size           Item Size/Print/Board             Status"

    with frame est no-labels no-box centered width 150 down stream-io.


/*SESSION:SET-WAIT-STATE ("general").    */
    
assign
 str-tit2 = "Estimates List" + " - by Estimate Number"
 {sys/inc/ctrtext.i str-tit2 112}

 fcust    = begin_cust-no
 tcust    = end_cust-no
 fsman    = begin_slsmn
 tsman    = end_slsmn
 fdate    = begin_date
 tdate    = end_date
 fest     = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
 test     = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est).


/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

    {sys/inc/outprint.i  value(lines-per-page)}


/* gdm - 10130804 */
IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).  
   IF NOT tb_break THEN
       PUT STREAM excel UNFORMATTED
         "Est#,Customer Name,Last used,Part #,Description 1,Description 2,Style,Blank size,Item Size,Print,Board,Status"
        SKIP.

END.
    

/*if td-show-parm then run show-param.*/

VIEW FRAME r-top.

FOR EACH tt-eb:
  DELETE tt-eb.
END.

FOR EACH est
    WHERE est.company     EQ cocode
      AND est.est-no      GE fest
      AND est.est-no      LE test
      AND est.est-date    GE fdate
      AND est.est-date    LE tdate
      AND est.mod-date    GE begin_date-2
      AND est.mod-date    LE end_date-2
      AND ((tb_booked AND
            CAN-FIND(FIRST oe-ordl
                     WHERE oe-ordl.company EQ est.company
                       AND oe-ordl.est-no  EQ est.est-no)) OR
           (tb_not-booked AND
            NOT CAN-FIND(FIRST oe-ordl
                         WHERE oe-ordl.company EQ est.company
                           AND oe-ordl.est-no  EQ est.est-no)))
    NO-LOCK,
 
    FIRST est-qty
    WHERE est-qty.company EQ est.company
      AND est-qty.est-no  EQ est.est-no
    NO-LOCK,

    EACH eb
    WHERE eb.company EQ est.company
      AND eb.est-no  EQ est.est-no
      AND eb.cust-no GE fcust
      AND eb.cust-no LE tcust
      AND eb.sman    GE fsman
      AND eb.sman    LE tsman
      AND LOOKUP(eb.cust-no, custcount) <> 0   
    NO-LOCK,
    
    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK
    
    BREAK BY est.est-no:

  IF FIRST-OF(est.est-no) THEN ll = YES.

  FOR EACH est-op
      WHERE est-op.company EQ eb.company
        AND est-op.est-no  EQ eb.est-no
        AND est-op.s-num   EQ eb.form-no
        AND (est-op.b-num  EQ eb.blank-no OR est-op.b-num EQ 0)
        AND est-op.m-code  GE begin_mach
        AND est-op.m-code  LE end_mach
        AND est-op.line    LT 500
      NO-LOCK:

    IF ll THEN DO:
      ASSIGN
       ll    = NO
       v-qty = v-qty + 1.

      IF CAN-FIND(FIRST oe-ordl
                  WHERE oe-ordl.company EQ est.company
                    AND oe-ordl.est-no  EQ est.est-no)
      THEN v-b-qty = v-b-qty + 1.
    END.

    FIND FIRST tt-eb
        WHERE tt-eb.m-code EQ est-op.m-code
          AND tt-eb.row-id EQ ROWID(eb)
        NO-ERROR.
    IF NOT AVAIL tt-eb THEN CREATE tt-eb.
    tt-eb.row-id = ROWID(eb).

    IF tb_break THEN tt-eb.m-code = est-op.m-code.
    ELSE LEAVE.
  END.

  RELEASE tt-eb.
END.

FOR EACH tt-eb,

    FIRST eb WHERE ROWID(eb) EQ tt-eb.row-id NO-LOCK,
    
    FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK,
 
    FIRST est
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-LOCK

    BREAK BY tt-eb.m-code
          BY (IF tb_sort THEN "" ELSE eb.est-no)
          BY eb.cust-no
          BY eb.part-no
          BY (IF tb_sort THEN eb.est-no ELSE "")

    WITH FRAME est:

  IF tb_break AND FIRST-OF(tt-eb.m-code) THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ eb.company
          AND mach.m-code  EQ tt-eb.m-code
        NO-LOCK NO-ERROR.
    lv-hdr-m-dscr = TRIM(tt-eb.m-code) + " " +
                    (IF AVAIL mach THEN mach.m-dscr ELSE "Not on File...").

    IF FIRST(tt-eb.m-code) THEN VIEW FRAME r-top2.
    
    PAGE.

    /* gdm - 10130804 */
    IF tb_excel
      THEN 
        PUT STREAM excel UNFORMATTED
          'Machine:'
          ',"' lv-hdr-m-dscr '"' 
         SKIP
          "Est#,Customer Name,Last used,Part #,Description 1,Description 2,Style,Blank size,Item Size,Print,Board,Status"
         SKIP.

  END.
  
  find first item
      where item.company eq cocode
        and item.i-no    eq ef.board
      no-lock no-error.
  find first cust
      where cust.company eq cocode
        and cust.cust-no eq eb.cust-no
      no-lock no-error.
  find first style
      where style.company eq cocode
        and style.style   eq eb.style
      no-lock no-error.

  v-blk-dim = "".
  {sys/inc/fraction.i eb.len v-blk-dim}
  {sys/inc/fraction.i eb.wid v-blk-dim}
  {sys/inc/fraction.i eb.dep v-blk-dim}

  /* gdm - 10130804 */
  ASSIGN
    v_est-no = trim(eb.est-no)
    v_custnm = IF AVAIL cust THEN cust.name ELSE eb.ship-name    
    v_stydsc = IF avail style THEN style.dscr ELSE ""
    v-booked = IF CAN-FIND(FIRST oe-ordl
                   WHERE oe-ordl.company EQ est.company
                  AND oe-ordl.est-no  EQ est.est-no)
                 THEN "Booked" ELSE ""
    v_i-name = IF AVAIL item THEN item.i-name ELSE ""
    v_blksz  = STRING(eb.t-wid) + " x " + STRING(eb.t-len)
    v_mod-date = STRING(est.mod-date,'99/99/9999').



  if first-of(eb.cust-no) or tb_sort THEN DO:
  
    display trim(eb.est-no)     @ eb.est-no
             cust.name          when avail cust
               eb.ship-name     when not avail cust or eb.cust-no eq "TEMP"
                                @ cust.name
             eb.part-no
             style.dscr         when avail style
             v-blk-dim
             "Booked"           WHEN CAN-FIND(FIRST oe-ordl
                                              WHERE oe-ordl.company EQ est.company
                                                AND oe-ordl.est-no  EQ est.est-no)
                                @ v-booked
             skip
             est.mod-date
             eb.part-dscr1
             eb.t-wid                                                                              
             eb.t-len                                                                               
             eb.i-coldscr
             skip
             eb.part-dscr2
             item.i-name        when avail item
             
         with frame est.

    /* gdm - 10130804 */
    IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' v_est-no                    '",'
            '"' v_custnm                    '",'
            '"' v_mod-date                  '",'
            '"' eb.part-no                  '",'
            '"' eb.part-dscr1               '",'
            '"' eb.part-dscr2               '",'
            '"' v_stydsc                    '",'
            '"' v_blksz                     '",'
            '"' v-blk-dim                   '",'
            '"' eb.i-coldscr                '",'
            '"' v_i-name                    '",'
            '"' v-booked                    '"'
          SKIP.

  END.
  else if first-of(eb.part-no) then do:
      
      put eb.part-dscr2 at 41.

      /* gdm - 10130804 */
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            ',,,'
            '"' eb.part-dscr2                '"'
          SKIP.
            

  END.

  if last-of(eb.cust-no) or tb_sort then down with frame est.
  
  /* gdm - 10130804 */
  IF tb_break AND tb_excel AND LAST-OF(tt-eb.m-code)
    THEN PUT STREAM excel UNFORMATTED SKIP(1).

  if last(tt-eb.m-code) THEN DO:
  
    put skip(2)
        space(10)
        "Total Estimates:" + " " + trim(string(v-qty,">>>,>>>,>>9"))
                        format "x(30)"
        skip
        space(10)
        "   Total Booked:" + " " + trim(string(v-b-qty,">>>,>>>,>>9"))
                        format "x(30)"
        skip(1).

    /* gdm - 10130804 */
      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
          SKIP(2)
          'Total Estimates:' + ',' + 
          trim(string(v-qty,">>>,>>>,>>9"))
         SKIP
          '   Total Booked:' + ',' + 
         trim(string(v-b-qty,">>>,>>>,>>9")) 
          SKIP.


  END.

end.

end procedure.

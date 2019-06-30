



/*------------------------------------------------------------------------
    File        : Ontimerep.p
    Purpose     :  On-Time Deliveries

    Syntax      :

    Description : Return a Dataset For Order Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
{sys/inc/var.i new shared}
DEFINE TEMP-TABLE ttOntimeDel NO-UNDO
FIELD vFile AS CHAR
FIELD abc AS CHAR.
DEFINE DATASET dsOntimeDel FOR ttOntimeDel .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmActTime        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegindate      AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER prmEnddate        AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginBol       AS DATE      NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndBol         AS DATE      NO-UNDO.      
    DEFINE INPUT PARAMETER prmPrintwei       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintmsf       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrinttra       AS CHARACTER NO-UNDO.
        
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOntimeDel.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
    

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF prmBeginCust   = ?   THEN ASSIGN prmBeginCust = "".
    IF prmEndCust = ? THEN ASSIGN prmEndCust = "".
    IF prmPrintwei  = ?  THEN ASSIGN prmPrintwei = "".
    IF prmPrintmsf  = ?  THEN ASSIGN prmPrintmsf = "".
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem = "".
    IF prmEndItem  = ?  THEN ASSIGN prmEndItem = "".
    IF prmPrinttra  = ?  THEN ASSIGN prmPrinttra = "".
    IF prmActTime    =  ?  THEN ASSIGN prmActTime = "".
    

MESSAGE "var"  prmUser       prmActTime    prmBeginCust  prmEndCust    prmBeginItem  prmEndItem    prmBegindate  prmEnddate    prmBeginBol   prmEndBol     prmPrintwei   prmPrintmsf   prmPrinttra   .

DEFINE VARIABLE begin_bol-date AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE end_bol-date AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)"  NO-UNDO.
DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /*INITIAL "C:\Inetpub\wwwroot\pdfs\ontime.csv"*/  NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "99/99/9999" NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" NO-UNDO.
DEFINE VARIABLE tb_pmsf AS LOGICAL INITIAL no NO-UNDO.  
DEFINE VARIABLE tb_ptr  AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_pw AS LOGICAL INITIAL no NO-UNDO.
  def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

    
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF STREAM excel.


DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO. 
DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid Customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
   AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid Customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

IF prmActTime = "OntimeRel" THEN DO:

assign
    v-today         = TODAY
    cocode          = prmComp
    locode          = usercomp.loc
    begin_bol-date  = prmBeginBol
    end_bol-date    = prmEndBol
    begin_cust      = prmBeginCust
    end_cust        = prmEndCust
    begin_i-no      = prmBeginItem
    end_i-no        = prmEndItem
    begin_ord-date  = prmBegindate
    end_ord-date    = prmEnddate
    tb_pmsf         = IF prmPrintmsf = "YES" THEN TRUE ELSE FALSE
    tb_ptr          = IF prmPrinttra = "YES" THEN TRUE ELSE FALSE
    tb_pw           = IF prmPrintwei = "YES" THEN TRUE ELSE FALSE
    .

    ASSIGN
            init-dir    = v-webrootpath
        fi_file = init-dir + "OnTimedel" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) +  ".csv".  
        vPdfFile   = "OnTimedel" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
              MESSAGE "file"  vPdfFile .

    
    run run-report.
    /*RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").*/
    CREATE ttOntimeDel.
    ASSIGN ttOntimeDel.vFile = vPdfFile.
   
 END.     /*if PrmAct*/

/***********************************************************************************************************************/    
PROCEDURE run-report :
{sys/form/r-top3.f}

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
def var v-date  like oe-ord.ord-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999].

def var v-cust-no like cust.cust-no.
def var v-name    like cust.name.
def var v-del     as   int extent 2.
def var v-ont     like v-del.
DEF VAR excelheader AS CHAR NO-UNDO.
def var v-msf as dec format  "->,>>>,>>9.9999" NO-UNDO.
DEF VAR v-sqft LIKE itemfg.t-sqft NO-UNDO.

format header
       skip(1)
       "Cust:"
       v-cust-no
       v-name
       skip(1)
       
    with frame r-top.


assign
 str-tit2 = "On-Time Deliveries"
 str-tit3 = "By Customer"
 {sys/inc/ctrtext.i str-tit2 56}
 {sys/inc/ctrtext.i str-tit3 80}

 v-cust[1] = begin_cust
 v-cust[2] = end_cust
 v-date[1] = begin_ord-date
 v-date[2] = end_ord-date.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}
IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Customer Part#,FG Item#,Order#,Due Date,BOL Date,On-Time,MSF,Weight,Trailer#".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
for each oe-ord
        where oe-ord.company  eq cocode
          and oe-ord.cust-no  ge v-cust[1]
          and oe-ord.cust-no  le v-cust[2]
          and oe-ord.ord-date ge v-date[1]
          and oe-ord.ord-date le v-date[2]
        use-index cust no-lock,

        each oe-ordl of oe-ord
        where oe-ordl.i-no ge begin_i-no
          and oe-ordl.i-no le end_i-no 
        no-lock,
        
        EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.link-no ne 0
        NO-LOCK,
        
        first oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-rel.link-no
          and oe-rell.i-no    eq oe-rel.i-no
          and oe-rell.line    eq oe-rel.line
          and can-find(first oe-relh where oe-relh.r-no eq oe-rell.r-no)
        USE-INDEX r-no no-lock,
      
        each oe-boll
        where oe-boll.company  eq cocode
          and oe-boll.r-no     eq oe-rell.r-no
          and oe-boll.ord-no   eq oe-rell.ord-no
          and oe-boll.rel-no   eq oe-rell.rel-no
          and oe-boll.b-ord-no eq oe-rell.b-ord-no
          and oe-boll.i-no     eq oe-rell.i-no
          and oe-boll.line     eq oe-rell.line
        no-lock,
        
        first oe-bolh
        where oe-bolh.b-no     eq oe-boll.b-no
          /* and oe-bolh.posted   eq yes */
          AND oe-bolh.bol-date GE begin_bol-date
          AND oe-bolh.bol-date LE end_bol-date
        no-lock

        break by oe-ord.cust-no
              by oe-bolh.bol-date
              by oe-ord.ord-no
              by oe-ordl.i-no:

	IF LOOKUP(oe-ord.cust-no, custcount) = 0 THEN NEXT.
              
      FIND FIRST itemfg WHERE itemfg.company = oe-boll.company AND 
                              itemfg.i-no    = oe-boll.i-no NO-LOCK NO-ERROR.
      v-sqft = IF AVAIL itemfg THEN itemfg.t-sqft ELSE 0.

      v-msf = (oe-boll.qty * v-sqft )/ 1000. 

      if first-of(oe-ord.cust-no) then do:
        find first cust
            where cust.company eq cocode
              and cust.cust-no eq oe-ord.cust-no
            no-lock no-error.
        assign
         v-cust-no = oe-ord.cust-no
         v-name    = if avail cust then cust.name else "Not on File".

        if first(oe-ord.cust-no) THEN display "" with frame r-top.
        ELSE page.
      end.
      
      v-del[1] = v-del[1] + 1.
      
      if oe-bolh.bol-date le oe-rel.rel-date then v-ont[1] = v-ont[1] + 1.

      display oe-ordl.part-no       column-label "Customer Part#"
              space(2)
              oe-ordl.i-no          column-label "FG Item#"
              space(2)
              oe-ord.ord-no         column-label "Order#"
              space(2)
              oe-ord.ord-date       column-label "Ord Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-rel.rel-date       column-label "Due Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date      column-label "BOL Date"
                                    FORMAT "99/99/99"
              space(2)
              oe-bolh.bol-date le oe-rel.rel-date format "Y/N"
                                    column-label "On-Time"  SPACE(2)
             v-msf                  COLUMN-LABEL  "MSF" WHEN tb_pmsf SPACE(2)
             oe-boll.weight         COLUMN-LABEL  "WT"  WHEN tb_pw   SPACE(2)
             oe-bolh.trailer        COLUMN-LABEL  "Trailer#"  WHEN tb_ptr
                
            with down no-box stream-io width 200 no-attr-space.  
            
      IF tb_excel THEN    
          PUT STREAM excel UNFORMATTED
            '"' oe-ordl.part-no                                        ' ",'
            '"' oe-ordl.i-no                                           ' ",'
            '"' oe-ord.ord-no                                          '",'
            '"' (IF oe-rel.rel-date <> ? THEN STRING(oe-rel.rel-date)
                 ELSE "")                                              '",'
            '"' (IF oe-bolh.bol-date <> ? THEN STRING(oe-bolh.bol-date)
                 ELSE "")                                              '",'
            '"'  STRING(oe-bolh.bol-date le oe-rel.rel-date,"Y/N")     '",'
            '"' ( IF tb_pmsf THEN v-msf ELSE 0 )                        '",'
            '"' (IF tb_pw THEN  oe-boll.weight ELSE 0 )                 '",'
            '"' (IF tb_ptr THEN oe-bolh.trailer ELSE  "" )              '",'
            SKIP.

      if last-of(oe-ord.cust-no) then do:
        put skip(1)
            "Customer Totals:"          at 5
            space(5)
            "Deliveries: " + trim(string(v-del[1],">,>>>,>>9"))
                                        format "x(21)"
            space(3)
            "On-Time: "    + trim(string(v-ont[1],">,>>>,>>9"))
                                        format "x(18)"
            v-ont[1] / v-del[1] * 100   format ">>9.99%"
            skip(1).
            
        assign
         v-del[2] = v-del[2] + v-del[1]
         v-ont[2] = v-ont[2] + v-ont[1]
        
         v-del[1] = 0
         v-ont[1] = 0.
      end.
      
      if last(oe-ord.cust-no) then do:
        assign
         v-cust-no = ""
         v-name    = "".
         
        page.
      
        put skip(3)
            "   Grand Totals:"          at 5
            space(5)
            "Deliveries: " + trim(string(v-del[2],">,>>>,>>9"))
                                        format "x(21)"
            space(3)
            "On-Time: "    + trim(string(v-ont[2],">,>>>,>>9"))
                                        format "x(18)"
            v-ont[2] / v-del[2] * 100   format ">>9.99%"
            skip(1).
      end.
    end. /* each oe-ord */


end procedure.



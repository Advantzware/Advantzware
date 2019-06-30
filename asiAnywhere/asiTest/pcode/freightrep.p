/*------------------------------------------------------------------------
    File        : freightrep.p
    Purpose     :  Fright Report

    Syntax      :

    Description : Return a Dataset For Freight Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
DEFINE TEMP-TABLE ttFreightRep NO-UNDO
FIELD vFright AS CHAR .
DEFINE DATASET dsFreightRep FOR ttFreightRep .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmFreight        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeInvDate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndInvDate      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeJob1      AS CHARACTER      NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJob1      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeJob2        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndJob2      AS CHARACTER      NO-UNDO.
    DEFINE INPUT PARAMETER prmOut         AS CHARACTER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cError   AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFreightRep.

        IF prmUser = ?       THEN ASSIGN prmUser = "".
        IF prmBeCust = ?     THEN ASSIGN prmBeCust = "".
        IF prmEndCust = ?    THEN ASSIGN prmEndCust = "".
        IF prmBeJob1 = ?     THEN ASSIGN prmBeJob1 = "".
        IF prmEndJob1 = ?    THEN ASSIGN prmEndJob1 = "".
        IF prmBeJob2 = ?     THEN ASSIGN prmBeJob2 = "".
        IF prmEndJob2 = ?    THEN ASSIGN prmEndJob2 = "".
        IF prmFreight = ?    THEN ASSIGN prmFreight  = "".        


DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U  NO-UNDO.
DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00"  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_inv-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE end_job-no AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" NO-UNDO.
DEFINE VARIABLE end_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "99"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE v-excel-file AS CHARACTER FORMAT "X(256)" /* INITIAL "c:\Inetpub\wwwroot\pdfs\FreightRep.csv"*/  NO-UNDO. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES  NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no  NO-UNDO.
DEFINE NEW SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF STREAM st-excel.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report.
DEF VAR v-cat          LIKE item.procat      NO-UNDO.
DEF VAR v-prompt-excel AS LOG                NO-UNDO.
DEF VAR v-comma        AS CHAR FORMAT "X(1)" NO-UNDO INITIAL ",".
DEFINE NEW SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

ASSIGN
    cocode         = prmComp .
 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN 
    g_company = cocode
    g_loc     = locode .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

IF prmFreight = "FreightRep" THEN DO:

assign
 v-today        = TODAY
 begin_cust     = prmBeCust
 begin_inv-date = prmBeInvDate
 begin_job-no   = prmBeJob1
 begin_job-no2  = prmBeJob2
 end_cust       = prmEndCust
 end_inv-date   = prmEndInvDate
 end_job-no     = prmEndJob1
 end_job-no2    = prmEndJob2
    .
tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

   
    assign
        init-dir    = v-webrootpath
        v-excel-file = init-dir + "FreightRep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + ".csv".  
        vPdfFile   = "FreightRep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + ".csv". 

         DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "FreightRep" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

              

 RUN run-report.

    CREATE ttFreightRep.
    IF tb_excel  THEN
    ASSIGN ttFreightRep.vFright = vPdfFile.
      IF NOT tb_excel  THEN
    ASSIGN ttFreightRep.vFright = vTextFile.
END.


PROCEDURE run-report :
{sys/form/r-topw.f}

DEF VAR v-job AS CHAR EXTENT 2 NO-UNDO.

def var v-misc as log no-undo.
DEF VAR v-term AS CHAR NO-UNDO.
DEF VAR v-job-no AS CHAR NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frate AS DEC EXTENT 2 NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.

assign
 str-tit2 = "Freight Report"
 {sys/inc/ctrtext.i str-tit2 112}
    
 v-job[1] = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
            TRIM(begin_job-no) + STRING(begin_job-no2,"99")    
 v-job[2] = FILL(" ",6 - LENGTH(TRIM(end_job-no))) +
            TRIM(end_job-no) + STRING(end_job-no2,"99").

/*{sys/inc/print1.i}*/

 if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/


FOR EACH tt-report:
  DELETE tt-report.
END.

display "" with frame r-top.

IF tb_excel THEN
DO:
  
   OUTPUT STREAM st-excel TO VALUE(v-excel-file).

   PUT STREAM st-excel
       "Invoice#,"
       "Inv Date,"
       "Job#,"
       "FG Item#,"
       "Qty Shipped,"
       "# of Skids,"
       "Wgt/100,"
       "Freight Charge,"
       SKIP.

END.

for each ar-inv
    where ar-inv.company  eq cocode
      and ar-inv.posted   eq yes
      and ar-inv.inv-date ge begin_inv-date
      and ar-inv.inv-date le end_inv-date
      and ar-inv.cust-no  ge begin_cust
      and ar-inv.cust-no  le end_cust
      and ar-inv.type     ne "FC"
    use-index inv-date no-lock,

    first cust
    where cust.company eq ar-inv.company
      and cust.cust-no eq ar-inv.cust-no
    no-lock,

    each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
      and FILL(" ",6 - LENGTH(TRIM(ar-invl.job-no))) +
          TRIM(ar-invl.job-no) +
          STRING(ar-invl.job-no2,"99") GE v-job[1]
      and FILL(" ",6 - LENGTH(TRIM(ar-invl.job-no))) +
          TRIM(ar-invl.job-no) +
          STRING(ar-invl.job-no2,"99") LE v-job[2]
      and not ar-invl.misc
    no-lock,
    
    FIRST itemfg
    WHERE itemfg.company EQ ar-inv.company
      AND itemfg.i-no    EQ ar-invl.i-no
    NO-LOCK
      
    BREAK BY ar-inv.inv-no BY ar-inv.inv-date:

  ASSIGN
   v-pallets  = 0
   v-frate[1] = 0.

  FOR EACH oe-boll
      WHERE oe-boll.company eq cocode
        AND oe-boll.b-no    eq ar-invl.b-no
        AND oe-boll.ord-no  eq ar-invl.ord-no
        AND oe-boll.i-no    eq ar-invl.i-no
        AND oe-boll.po-no   eq ar-invl.po-no
      USE-INDEX b-no NO-LOCK,

      FIRST oe-bolh OF oe-boll NO-LOCK,
      
      FIRST shipto
      WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ oe-bolh.cust-no
        AND shipto.ship-id EQ oe-bolh.ship-id
      NO-LOCK,
      
      FIRST carrier OF oe-bolh NO-LOCK:

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.tag     EQ oe-boll.tag
          AND fg-bin.loc     EQ oe-boll.loc
          AND fg-bin.loc-bin EQ oe-boll.loc-bin
          AND fg-bin.job-no  EQ oe-boll.job-no
          AND fg-bin.job-no2 EQ oe-boll.job-no2
        NO-LOCK NO-ERROR.

    v-qty-pal = oe-boll.qty-case.    

    IF AVAIL fg-bin THEN
      v-qty-pal = (IF v-qty-pal NE 0 THEN v-qty-pal ELSE
                   IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count)   *
                  (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
                  (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).

    v-qty-pal = oe-boll.qty / v-qty-pal.    
    {sys/inc/roundup.i v-qty-pal}

    IF v-qty-pal LT 0 THEN v-qty-pal = v-qty-pal * -1.

    v-pallets = v-pallets + v-qty-pal.

    CASE carrier.chg-method:
      WHEN "W" THEN DO:                                     /* Weight in Lbs */
        v-frt-chg = itemfg.weight-100 * oe-boll.qty / 100.
      END.

      WHEN "P" THEN DO:                                     /* # of Pallets */
        v-frt-chg = v-qty-pal.                        
      END.

      OTHERWISE DO:                                         /* MSF */
        v-frt-chg = itemfg.t-sqft * oe-boll.qty / 1000.
      END.
    END CASE.

    RUN sys/inc/getfrate.p (shipto.loc, oe-bolh.carrier, shipto.dest-code,
                            v-frt-chg, 1, OUTPUT v-frt-chg).

    v-frate[1] = v-frate[1] + v-frt-chg.
  END.

  v-job-no = TRIM(ar-invl.job-no) + "-" + STRING(ar-invl.job-no2,"99").
  IF v-job-no EQ "-00" THEN v-job-no = "".

  DISPLAY ar-inv.inv-no                             COLUMN-LABEL "Invoice#"
          ar-inv.inv-date   FORMAT "99/99/9999"     COLUMN-LABEL "Inv Date"
          v-job-no          FORMAT "x(9)"           COLUMN-LABEL "Job#"
          ar-invl.i-no                              COLUMN-LABEL "FG Item#"
          ar-invl.ship-qty  FORMAT "->,>>>,>>>,>>>" COLUMN-LABEL "Qty Shipped"
          v-pallets                                 COLUMN-LABEL "# of Skids"          
          itemfg.weight-100 FORMAT "->>,>>>9.99"    COLUMN-LABEL "Wgt/100"
          v-frate[1]        FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Freight Charge"
   
      WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132. 

  IF tb_excel THEN      
      PUT STREAM st-excel
         ar-inv.inv-no                             
         v-comma
         ar-inv.inv-date   FORMAT "99/99/9999"     
         v-comma
         v-job-no          FORMAT "x(9)"          
         v-comma
         ar-invl.i-no                             
         v-comma
         ar-invl.ship-qty  FORMAT "->>>>>>>>>>"  
         v-comma
         v-pallets                               
         v-comma
         itemfg.weight-100 FORMAT "->>>>>9.99"                      
         v-comma
         v-frate[1]        FORMAT "->>>>>>>9.99"  
         SKIP.

  v-frate[2] = v-frate[2] + v-frate[1].

  IF LAST(ar-inv.inv-no) THEN DO WITH FRAME detail:
    PUT SKIP(1).
    UNDERLINE v-frate[1].
    DISPLAY "Total Freight" @ ar-invl.i-no
            v-frate[2] @ v-frate[1].
    DOWN.
  END.
END.


/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

end procedure.

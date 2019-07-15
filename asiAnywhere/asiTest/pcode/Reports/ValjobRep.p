

/*------------------------------------------------------------------------
    File        : ValjobRep.p
    Purpose     : Finished Goods Value By Job
    Main File   : fgrep\r-valjob.w
    Syntax      :

    Description : Return a Dataset of Request For Quote

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttValJobReport NO-UNDO
        FIELD vValJobFile AS CHAR
        .

    DEFINE DATASET dsValJobReport FOR ttValJobReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustPO       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustPO       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmItemCode        AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmConJob          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmIncZer          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsValJobReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegCustomer  = ?        THEN ASSIGN     prmBegCustomer  = "".
    IF  prmEndCustomer  = ?        THEN ASSIGN     prmEndCustomer  = "".
    IF  prmBegCustPO    = ?        THEN ASSIGN     prmBegCustPO    = "".
    IF  prmEndCustPO    = ?        THEN ASSIGN     prmEndCustPO    = "".
    IF  prmItemCode     = ?        THEN ASSIGN     prmItemCode     = "All".
    IF  prmConJob       = ?        THEN ASSIGN     prmConJob       = "".
    IF  prmIncZer       = ?        THEN ASSIGN     prmIncZer       = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut          = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.


    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.  
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_itm-code AS CHARACTER INITIAL "All" NO-UNDO.
    DEFINE VARIABLE tb_con-job AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
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

 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCustomer NO-ERROR.
    
 IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
 END.

 FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCustomer  OR prmEndCustomer = "zzzzzzzz" ) NO-ERROR.

 IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
 END.


 FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
 END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "ValJobRep" THEN DO:
    ASSIGN 
        begin_cust      = prmBegCustomer                                
        begin_cust-po   = prmBegCustPO                        
        end_cust        = prmEndCustomer                                    
        end_cust-po     = prmEndCustPO                                  
        rd_itm-code     = prmItemCode .                                      
                                                                             
    ASSIGN                                                                   
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_con-job    = IF prmConJob = "yes" THEN TRUE ELSE FALSE
        tb_inc-zer    = IF prmIncZer = "yes" THEN TRUE ELSE FALSE
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "valjob" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "valjob" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "valjob" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttValJobReport.
        ASSIGN ttValJobReport.vValJobFile = excel-file.
    END.
    ELSE DO:
        CREATE ttValJobReport.
        ASSIGN ttValJobReport.vValJobFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-ext   as dec format "->>>,>>>,>>9.99".
def var fcust as ch init " ".
def var tcust like fcust init "zzzzzzzzz".
def var fpo as ch format "x(15)" init " ".
def var tpo like fpo init "zzzzzzzzzzzzzzz".
def var type as ch format "!" init "A".
def var zbal as log format "Y/N".
def var v-qty-onh as dec format "->>>,>>>,>>9".
def var v-frst as log.
def var v-tot-ord  as dec format "->>>,>>>,>>9".
def var v-tot-ship as dec format "->>>,>>>,>>9".
def var v-tot-onh as dec format "->>>,>>>,>>9".
def var v-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-grand-tot-ord  as dec format "->>>,>>>,>>9".
def var v-grand-tot-ship as dec format "->>>,>>>,>>9".
def var v-grand-tot-onh as dec format "->>>,>>>,>>9".
def var v-grand-tot-ext as dec format "->>>,>>>,>>9.99".
def var v-custown as log format "Y/N" init "N".
def var v-frst-i-no as log.
def var v-print as log.
def var trans-date like fg-rcpts.trans-date.
def var rec-date as log init no.
def var v-job as char format "x(9)".
def var v-rec-found as log.
def var v-qty-job like v-qty-onh.
def var v-ext-job like v-ext.
def buffer xbin for fg-bin.                    /* DAR */
def buffer xbin2 for fg-bin.                   /* DAR */
def var v-qty-ord as int.
def var v-qty-ship as int.
def var next-rel as date init 01/01/0001.
def var v-prt-all as log format "Y/N" init no.
DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

form
    itemfg.cust-no to 8 label "CUSTOMER"
    oe-ordl.i-no to 24 label "ITEM #"
    oe-ordl.po-no to 40 label "PO #"
    v-job to 50 column-label "  JOB"
    oe-ordl.qty format "->,>>>,>>9" to 61 column-label "QUANTITY! ORDERED"
    next-rel format "99/99/99" to 77 column-label "NEXT!REL DATE"
    li-ship-qty format "->,>>>,>>9" to 88 column-label "QUANTITY! SHIPPED"
    v-qty-onh  to 101 column-label "QUANTITY! ON HAND"
    oe-ordl.price format ">>,>>>,>>9.99" to 115 column-label "SELLING! PRICE"
    v-ext format "->>>,>>>,>>9.99" to 131 column-label "TOTAL!VALUE"
    with frame itemx no-box down STREAM-IO width 132.

  form skip(1) with frame r-top.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "CUSTOMER,ITEM #,PO #,JOB,QUANTITY ORDERED,NEXT REL DATE," 
              + "QUANTITY SHIPPED,QUANTITY ON HAND,SELLING PRICE,TOTAL VALUE".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

assign
 str-tit2 = "Finished Goods Value By Job"
 {sys/inc/ctrtext.i str-tit2 112}

 fcust      = begin_cust
 tcust      = end_cust
 fpo        = begin_cust-po
 tpo        = end_cust-po 
 type       = substr(rd_itm-code,1,1) 
 zbal       = tb_inc-zer
 v-prt-all  = tb_con-job.
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

display "" with frame r-top.

    for each itemfg
        where itemfg.company eq cocode
            and itemfg.cust-no ge fcust
            and itemfg.cust-no le tcust
            and (type eq "A" or itemfg.i-code eq type)
            and LOOKUP(itemfg.cust-no, custcount) <> 0
            use-index customer no-lock

        break by itemfg.cust-no
              by itemfg.i-no:

      if first(itemfg.cust-no) then v-frst = yes.

      if first-of(itemfg.cust-no) then do:
        assign
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0.
        display itemfg.cust-no with frame itemx.
      end.

      /*  THIS MAY NEED TO BE WRITTEN A DIFFERENT WAY FOR SPEED */
      /*  GOES THROUGH EVERY FINISHED GOOD */
      v-print = no.

      for each oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.cust-no eq itemfg.cust-no
            and oe-ordl.i-no    eq itemfg.i-no
            and oe-ordl.po-no   ge fpo
            and oe-ordl.po-no   le tpo
          use-index cust no-lock,

          first oe-ord of oe-ordl
          where oe-ord.stat ne "C"
            and oe-ord.stat ne "Z"
          NO-LOCK
          break by oe-ordl.i-no:

        assign
         v-qty-ord  = 0.
         v-qty-ship = 0.

        next-rel = ?.

        for each oe-rel FIELDS(rel-date)
            where oe-rel.company eq oe-ordl.company
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.link-no eq 0
            no-lock by oe-rel.rel-date:
          next-rel = oe-rel.rel-date.
          leave.
        end.

        RUN oe/ordlsqty.p (ROWID(oe-ordl),
                           OUTPUT li-inv-qty, OUTPUT li-ship-qty).

        assign
         v-qty-ord  = v-qty-ord + oe-ordl.qty
         v-qty-ship = v-qty-ship + li-ship-qty
         v-qty-onh  = 0.

        for each xbin FIELDS(qty)
            where xbin.company eq cocode
              and xbin.i-no    eq oe-ordl.i-no
              and xbin.job-no  eq oe-ordl.job-no
              and xbin.job-no2 eq oe-ordl.job-no2
            no-lock use-index job:
          v-qty-onh = v-qty-onh + xbin.qty.
        end.

        if itemfg.sell-uom eq "CS" and itemfg.case-count ne 0 then
          v-ext = (v-qty-onh * oe-ordl.price) / itemfg.case-count.
        else do:
          find first uom
              where uom.uom  eq itemfg.sell-uom
                and uom.mult ne 0
              no-lock no-error.

          v-ext = v-qty-onh * oe-ordl.price /
                  (if avail uom then uom.mult else 1000).
        end.

        if itemfg.sell-uom eq "L" then v-ext = oe-ordl.price.

        v-job = oe-ordl.job-no + "-" + string(oe-ordl.job-no2,"99").

        if v-job = "-00" then v-job = "".

        if v-qty-onh ne 0 or zbal then do:

          display oe-ordl.i-no
                  oe-ordl.po-no
                  v-job
                  oe-ordl.qty
                  next-rel
                  li-ship-qty
                  v-qty-onh
                  oe-ordl.price
                  v-ext
              with frame itemx.
          down with frame itemx.

          IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED
                '"'  itemfg.cust-no  '",'
                '"'  oe-ordl.i-no  '",'
                '"'  oe-ordl.po-no  '",'
                '"'  v-job + '",'
                '"'  oe-ordl.qty  '",'
                '"'   (IF next-rel NE ? THEN STRING(next-rel,"99/99/99")
                         ELSE "")  '",'
                '"'  li-ship-qty  '",'
                '"'  v-qty-onh  '",'
                '"'  oe-ordl.price  '",'
                '"'  v-ext  '",'
               SKIP.

          assign
           v-tot-ext        = v-tot-ext + v-ext
           v-grand-tot-ext  = v-grand-tot-ext + v-ext
           v-tot-ord        = v-tot-ord + v-qty-ord
           v-grand-tot-ord  = v-grand-tot-ord + v-qty-ord
           v-tot-ship       = v-tot-ship + v-qty-ship
           v-grand-tot-ship = v-grand-tot-ship + v-qty-ship
           v-tot-onh        = v-tot-onh + v-qty-onh
           v-grand-tot-onh  = v-grand-tot-onh + v-qty-onh
           v-qty-onh        = 0
           v-qty-ship       = 0
           v-qty-ord        = 0
           v-print          = yes.
        end.
      end. /* for each oe-ordl */

      if (not v-print) and v-prt-all then do:
        display itemfg.i-no @ oe-ordl.i-no with frame itemx.
        down with frame itemx.

        IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' itemfg.cust-no              '",'
              '"' itemfg.i-no                 '",'
              SKIP.
      end.

      if last-of(itemfg.cust-no) then do:
        if v-print                                      and
           ((not v-frst) or (not last(itemfg.cust-no))) and
           (v-tot-onh ne 0 or zbal)                     then

          put "------------" to 61 "------------" to 88
              "-----------" to 101 "--------------" to 131 skip
              "CUSTOMER TOTALS:" to 44
              v-tot-ord to 61
              v-tot-ship to 88
              v-tot-onh to 101
              v-tot-ext to 131 skip(1).

        assign
         v-frst     = no
         v-tot-ord  = 0
         v-tot-ship = 0
         v-tot-onh  = 0
         v-tot-ext  = 0.
      end.
    end.
    put "------------" to 61 "------------" to 88 "-----------" to 101
   "--------------" to 131 skip
   "GRAND TOTALS:" to 44 v-grand-tot-ord to 61 v-grand-tot-ship to 88
              v-grand-tot-onh to 101 v-grand-tot-ext to 131 skip(1).

end procedure.

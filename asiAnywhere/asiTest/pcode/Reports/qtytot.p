
/*------------------------------------------------------------------------
    File        : qtytot.p
    Purpose     : Quantity Totals By Customer/Item
    Main File   : fgrep\r-qtytot.w
    Syntax      :

    Description : Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttReportQtytot NO-UNDO
        FIELD vQtotFile AS CHAR .

    DEFINE DATASET dsReportQtytot FOR ttReportQtytot .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmAddQty         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSellprc        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSummary        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReportQtytot.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser          = ? THEN ASSIGN prmUser          = "".
    IF prmAction        = ? THEN ASSIGN prmAction        = "".
    IF prmBeginCust     = ? THEN ASSIGN prmBeginCust     = "".
    IF prmEndCust       = ? THEN ASSIGN prmEndCust       = "".    
    IF prmAddQty        = ? THEN ASSIGN prmAddQty        = "".
    IF prmSellprc       = ? THEN ASSIGN prmSellprc       = "".
    IF prmSummary       = ? THEN ASSIGN prmSummary       = "".
    IF prmOut           = ? THEN ASSIGN prmOut           = "No" .    

    {sys/inc/var.i new shared}
    {custom/xprint.i}

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    
    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.    
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.    
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE tb_addqty AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_sellprc AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_summary AS LOGICAL INITIAL yes NO-UNDO.
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
    AND usercust.cust-no = prmBeginCust NO-ERROR.
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


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "qtytot" THEN DO:
    ASSIGN 
        begin_cust    = prmBeginCust                                                                           
        end_cust      = prmEndCust .                                
                                                                         
    ASSIGN                                                          
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_addqty     = IF prmAddQty = "yes" THEN TRUE ELSE FALSE
        tb_summary    = IF prmSellprc = "yes" THEN TRUE ELSE FALSE
        tb_summary    = IF prmSummary = "yes" THEN TRUE ELSE FALSE 
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "qtytot" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "qtytot" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "qtytot" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttReportQtytot.
        ASSIGN ttReportQtytot.vQtotFile = excel-file.
    END.
    ELSE DO:
        CREATE ttReportQtytot.
        ASSIGN ttReportQtytot.vQtotFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-top3w.f}

def var floc as ch initial " ".
def var tloc like floc initial "ZZZZZZZZZ".
def var v-frst as logical.
def var v-price like oe-ordl.price no-undo.
def var v-job-no like itemfg.cust-job-no no-undo.
def var tot-val as dec format "->>>,>>>,>>9.99" no-undo.
def var done-flag as logical initial no no-undo.
def var v-sumdet as logical format "Summary/Detail" initial yes no-undo.
DEF VAR v-qty LIKE itemfg.q-onh NO-UNDO.
DEF VAR v-ext LIKE tot-val NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

FORM header
     skip(1)
     "CUSTOMER        NAME                              SELLING   QUANTITY IN PROGRESS OPEN ORDERS          PTD      LAST YR       ORDER#"
     "ITEM#           DESCRIPTION                         PRICE    ON HAND       JOB #    CUST PO#          YTD"
     FILL("-",132) format "x(132)"
    with frame qinv-hdr no-box page-top STREAM-IO width 132.

FORM
    itemfg.i-no at 1
    itemfg.i-name format "x(30)" at 17
    v-price format ">>>,>>9.99" to 62
    itemfg.q-onh format "->,>>>,>>9" to 73
    itemfg.q-ono format "->,>>>,>>9" to 84
    itemfg.q-alloc format "->,>>>,>>9" to 95
    itemfg.q-inv-ptd format "->>,>>>,>>9" to 107
    itemfg.q-inv-ytd format "->>,>>>,>>9" to 119
    itemfg.u-inv format "->>,>>>,>>9" to 131
    with frame itemx-sum no-box no-labels down STREAM-IO width 132.

FORM
    cust.cust-no
    itemfg.i-name at 17
    v-price format ">>>,>>9.99"
    itemfg.q-onh format "->,>>>,>>9"
    itemfg.q-ono format "->,>>>,>>9" to 80
    itemfg.q-alloc format "->,>>>,>>9" to 92
    itemfg.q-inv-ptd format "->>>,>>>,>>9" to 105
    itemfg.u-inv format "->>>,>>>,>>9" to 118
    oe-ordl.ord-no to 131
    itemfg.i-no at 1
    itemfg.part-dscr1 at 17
    v-job-no to 80
    itemfg.cust-po-no to 92
    itemfg.q-inv-ytd format "->>>,>>>,>>9" to 105
    with frame itemx-det no-labels no-box down STREAM-IO width 132.

assign
 str-tit2 = "Quantity Totals By Customer/Item"
 {sys/inc/ctrtext.i str-tit2 112}

 floc      = begin_cust
 tloc      = end_cust
 v-sumdet  = tb_summary.
  
IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).

  IF v-sumdet THEN
    excelheader = "ITEM #,DESCRIPTION,SELLING PRICE,"
                + "QUANTITY ON HAND,TOTAL IN PROGRESS,ALLOCATED OPEN ORDER,"
                + "PTD,YTD,LAST YR".
  ELSE
    excelheader = "CUSTOMER/ITEM #,NAME/DESCRIPTION,SELLING PRICE,"
                + "QUANTITY ON HAND,IN PROGRESS/JOB #,OPEN ORDERS/CUST PO#,"
                + "PTD/YTD,LAST YR,ORDER #".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

SESSION:SET-WAIT-STATE ("general").
         
   if v-sumdet THEN do: 
      for each cust where cust.company = cocode and
            cust.cust-no >= floc   and  cust.cust-no <= tloc
            and  LOOKUP(cust.cust-no, custcount) <> 0
            use-index cust no-lock break by cust.cust-no:

        assign
         v-frst = yes
         str-tit3 = "Cust #: " + cust.cust-no
         x = (132 - length(str-tit3)) / 2
         str-tit3 = fill(" ",x) + str-tit3.

        FORM header
     skip(1)
     "                                                       SELLING   QUANTITY   TOTAL IN  ALLOCATED" skip
     "ITEM #          DESCRIPTION                              PRICE    ON HAND   PROGRESS OPEN ORDER         PTD         YTD     LAST YR"
     fill("-",132) format "x(132)"
    with frame qinv-hdr-sum no-box page-top STREAM-IO width 132.
 
        for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
               no-lock break by itemfg.cust-no
               by itemfg.i-no:

          if first-of(cust.cust-no) and v-frst then
          do:
            HIDE FRAME qinv-hdr-sum.
            HIDE FRAME r-top.
            page.
            display with frame r-top.
            display with frame qinv-hdr-sum.
          end.

          v-qty = IF tb_addqty THEN (itemfg.q-onh + itemfg.q-ono) ELSE itemfg.q-onh.

          if itemfg.sell-uom   eq "CS" AND itemfg.case-count ne 0 
             THEN v-ext = (v-qty * itemfg.sell-price) / itemfg.case-count.
          ELSE if itemfg.sell-uom eq "L" then v-ext = itemfg.sell-price.
          else do:
             find first uom where uom.uom  eq itemfg.sell-uom
                              and uom.mult ne 0 no-lock no-error.
             v-ext = v-qty * itemfg.sell-price /
                        (if avail uom then uom.mult else 1000).
          end.

          assign 
           v-price = itemfg.sell-price
           /*tot-val = tot-val + ((IF tb_addqty THEN (itemfg.q-onh + itemfg.q-ono) ELSE itemfg.q-onh) 
                               * v-price).
           */
           tot-val = tot-val + v-ext.

          display /* cust.cust-no when v-frst */
                  itemfg.i-no
                  itemfg.i-name
                  v-price WHEN tb_sellprc
                  itemfg.q-onh
                  itemfg.q-ono
                  itemfg.q-alloc
                  itemfg.q-inv-ptd
                  itemfg.q-inv-ytd
                  itemfg.u-inv
                  with down frame itemx-sum.

          down with frame itemx-sum.

          IF tb_excel THEN 
            PUT STREAM excel UNFORMATTED
                '"' itemfg.i-no                          '",'
                '"' itemfg.i-name                        '",'
                '"' (IF tb_sellprc THEN v-price ELSE 0)  '",'
                '"' itemfg.q-onh                         '",'
                '"' itemfg.q-ono                         '",'
                '"' itemfg.q-alloc                       '",'
                '"' itemfg.q-inv-ptd                     '",'
                '"' itemfg.q-inv-ytd                     '",'
                '"' itemfg.u-inv                         '",'
                SKIP.

          assign v-frst = no.
        end.  /* for each item */
      end.  /* for each cust */

      IF tb_addqty THEN put skip(2) "GRAND TOTAL VALUE (QtyOnHand + QtyOnOrd): " tot-val skip.
      ELSE put skip(2) "GRAND TOTAL VALUE (QtyOnHand): " tot-val skip.
    end. /* end if v-sumdet */

    else /* v-sumdet */
    do:
      for each cust where cust.company = cocode and
            cust.cust-no >= floc   and  cust.cust-no <= tloc
            use-index cust no-lock break by cust.cust-no:

        if first-of(cust.cust-no) then
        do:
          HIDE FRAME qinv-hdr.
          HIDE FRAME r-top.
          PAGE.
          display with frame r-top.
          DISPLAY WITH FRAME qinv-hdr.
          assign v-frst = yes.
        end.

        for each itemfg where itemfg.company = cocode
                           and itemfg.cust-no = cust.cust-no
               no-lock use-index customer break by itemfg.cust-no
               by itemfg.i-no:

          tot-val = tot-val + ((IF tb_addqty THEN (itemfg.q-onh + itemfg.q-ono) ELSE itemfg.q-onh) 
                               * itemfg.sell-price).

          for each oe-ordl where oe-ordl.company eq cocode
                              and oe-ordl.i-no    eq itemfg.i-no
                              and oe-ordl.cust-no eq itemfg.cust-no
                              no-lock use-index item break by oe-ordl.ord-no
                              by oe-ordl.i-no:

            assign v-price = oe-ordl.price.

            if oe-ordl.job-no <> "" then
              assign v-job-no = oe-ordl.job-no + "-" + string(oe-ordl.job-no2).
            else
              assign v-job-no = "".

            display cust.cust-no when v-frst
                    itemfg.i-name
                    v-price WHEN tb_sellprc
                    itemfg.q-onh
                    itemfg.q-ono
                    itemfg.q-alloc
                    itemfg.q-inv-ptd
                    itemfg.u-inv
                    oe-ordl.ord-no when avail oe-ordl
                    itemfg.i-no
                    itemfg.part-dscr1
                    v-job-no
                    itemfg.cust-po-no
                    itemfg.q-inv-ytd
                    with down frame itemx-det.

            down with frame itemx-det.

            IF tb_excel THEN DO:
              PUT STREAM excel UNFORMATTED
                  '"' (IF v-frst THEN cust.cust-no ELSE "")                  '",'
                  '"' itemfg.i-name                                          '",'
                  '"' (IF tb_sellprc THEN v-price ELSE 0)                    '",'
                  '"' itemfg.q-onh                                           '",'
                  '"' itemfg.q-ono                                           '",'
                  '"' itemfg.q-alloc                                         '",'
                  '"' itemfg.q-inv-ptd                                       '",'
                  '"' itemfg.u-inv                                           '",'
                  '"' (IF AVAIL oe-ordl THEN STRING(oe-ordl.ord-no) ELSE "") '",'
                  SKIP.

              PUT STREAM excel UNFORMATTED
                  '"' itemfg.i-no                                            '",'
                  '"' itemfg.part-dscr1                                      '",'
                  '"' ""                                                     '",'
                  '"' ""                                                     '",'
                  '"' v-job-no                                               '",'
                  '"' itemfg.cust-po-no                                      '",'
                  '"' itemfg.q-inv-ytd                                       '",'
                  SKIP.
            END.

            assign v-frst = no.
          end.  /* for each oe-ordl */
        end.  /* for each item */
      end.  /* for each cust */

    IF tb_addqty THEN put skip(2) "GRAND TOTAL VALUE (QtyOnHand + QtyOnOrd): " tot-val skip.
    ELSE put skip(2) "GRAND TOTAL VALUE (QtyOnHand): " tot-val skip.
    end. /* else v-sumdet */

end procedure.

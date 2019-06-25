
/*------------------------------------------------------------------------
    File        : rep_custag.p
    Purpose     : Finished Goods Sales Value By Customer By Tag
    Main File   : fgrep\r-custag.w
    Syntax      :

    Description : Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttReportCustag NO-UNDO
        FIELD vFile AS CHAR
        FIELD jklovgf AS CHAR.
    DEFINE DATASET dsReportCustag FOR ttReportCustag .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmComp           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginPo        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPo          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeSaleman      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalesman    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmZeroQty        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmInvWarehouse   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReportCustag.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser          = ? THEN ASSIGN prmUser          = "".
    IF prmAction        = ? THEN ASSIGN prmAction        = "".
    IF prmBeginCust     = ? THEN ASSIGN prmBeginCust     = "".
    IF prmEndCust       = ? THEN ASSIGN prmEndCust       = "".
    IF prmBeginPo       = ? THEN ASSIGN prmBeginPo       = "".
    IF prmEndPo         = ? THEN ASSIGN prmEndPo         = "".
    IF prmBeSaleman     = ? THEN ASSIGN prmBeSaleman     = "".
    IF prmEndSalesman   = ? THEN ASSIGN prmEndSalesman   = "".
    IF prmZeroQty       = ? THEN ASSIGN prmZeroQty       = "".
    IF prmInvWarehouse  = ? THEN ASSIGN prmInvWarehouse  = "".
    IF prmOut           = ? THEN ASSIGN prmOut           = "No" .
    IF prmComp          = ? THEN ASSIGN prmComp          = "".
    

{sys/inc/var.i new shared}
 {custom/xprint.i}

   


    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    /*DEF VAR prmComp AS CHAR NO-UNDO.*/
    

    DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U NO-UNDO.
    DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no NO-UNDO.
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


 IF prmAction = "custag" THEN DO:
    ASSIGN 
        begin_cust    = prmBeginCust                                   
        begin_cust-po = prmBeginPo                       
        begin_slm     = prmBeSaleman                                      
        end_cust      = prmEndCust                                       
        end_cust-po   = prmEndPo                       
        end_slm       = prmEndSalesman .                                
                                                                         
    ASSIGN                                                          
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_inc-cust   = IF prmInvWarehouse = "yes" THEN TRUE ELSE FALSE  
        tb_inc-zer    = IF prmZeroQty = "yes" THEN TRUE ELSE FALSE
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "custag" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "custag" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "custag" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttReportCustag.
        ASSIGN ttReportCustag.vFile = excel-file.
    END.
    ELSE DO:
        CREATE ttReportCustag.
        ASSIGN ttReportCustag.vFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-ext   as DEC NO-UNDO.
def var v-ext2  as DEC NO-UNDO.
def var v-subtot as DEC NO-UNDO.
def var v-tagtot as DEC NO-UNDO.
def var v-itemtot as DEC NO-UNDO.
def var fco like fg-rcpts.trans-date initial TODAY NO-UNDO.
def var tco like fco initial TODAY NO-UNDO.
def var floc as ch initial " " NO-UNDO.
def var tloc like floc initial "ZZZZZZZZ" NO-UNDO.
def var fcat as ch initial " " NO-UNDO.
def var tcat like fcat NO-UNDO.
def var type as ch format "X" initial "A" NO-UNDO.
def var ftyp like cust.sman initial " " NO-UNDO.
def var ttyp like cust.sman initial "ZZZ" NO-UNDO.
def var zbal as logical format "Y/N" initial NO NO-UNDO.
def var v-frst as LOGICAL NO-UNDO.
def var v-sub-qty like fg-bin.qty NO-UNDO.
def var v-sub-ext as DEC NO-UNDO.
def var v-custown as logical format "Y/N" initial "N" NO-UNDO.
def var v-print-line as LOGICAL NO-UNDO.
def var v-print-cust as LOGICAL NO-UNDO.

DEF VAR excelheader AS CHAR NO-UNDO.

form
    cust.cust-no column-label "CUSTOMER!   ID"
    cust.type label "TYPE"
    fg-bin.job-no column-label "  JOB!NUMBER"
    fg-bin.tag column-label "TAG!NUMBER"
    itemfg.cust-po-no column-label " P.O.!NUMBER"
    itemfg.i-no column-label " ITEM!NUMBER"
    itemfg.i-name label "DESCRIPTION"   format "x(20)"
    fg-bin.qty column-label "QUANTITY!ON-HAND" format "->,>>>,>>>,>>9"
    itemfg.sell-price column-label "SELL!PRICE"  format ">,>>>,>>9.99"
    v-ext column-label "TOTAL!VALUE"  format "->>>,>>>,>>9.99"
with no-box frame itemx down STREAM-IO width 132.

form
    "Grand Total" at 94 v-ext2  to 124  format "->>>,>>>,>>9.99"
with frame itemgt no-box no-labels down STREAM-IO width 132.

assign
 str-tit2 = "Finished Goods Sales Value By Customer By Tag"
 {sys/inc/ctrtext.i str-tit2 112}

 floc       = begin_cust
 tloc       = end_cust
 fcat       = begin_cust-po
 tcat       = end_cust-po
 ftyp       = begin_slm
 ttyp       = end_slm
 zbal       = tb_inc-zer
 v-custown  = tb_inc-cust.
 
/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "CUSTOMER ID,TYPE,JOB NUMBER,TAG NUMBER,P.O. NUMBER,"
              + "FG ITEM NUMBER,DESCRIPTION,QUANTITY ON-HAND,SELL PRICE,TOTAL VALUE".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

display "" with frame r-top.

    for each itemfg  no-lock where itemfg.company  =  cocode and
         itemfg.cust-no >= floc and itemfg.cust-no <= tloc and
         itemfg.cust-po-no >= fcat and itemfg.cust-po-no <= tcat
         and lookup(itemfg.cust-no,custcount) <> 0
         use-index customer break by itemfg.cust-no:
      if first-of(itemfg.cust-no) then
       assign v-subtot = 0
              v-frst = yes
              v-print-line = no
              v-print-cust = no.
      find first cust where cust.company =  cocode and
            cust.cust-no = itemfg.cust-no and
            cust.sman >= ftyp and cust.sman <= ttyp no-lock no-error.
      if available cust then do:
        for each fg-bin no-lock where fg-bin.company = cocode and
             fg-bin.i-no = itemfg.i-no use-index i-no
             break by fg-bin.tag:

          if ((fg-bin.qty  ne 0) or (zbal)) and (v-custown or
           not v-custown and fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "") then do:

            if line-counter >=56 then
              page.

            if itemfg.sell-uom = "CS" and itemfg.case-count ne 0 then
              v-ext = (fg-bin.qty * itemfg.sell-price) / itemfg.case-count.
            else
            find first uom where uom.uom = itemfg.sell-uom and
               uom.mult ne 0 no-lock no-error.
            if available uom then
              assign v-ext = (fg-bin.qty * itemfg.sell-price / uom.mult).
            else
            v-ext = (fg-bin.qty * itemfg.sell-price) / 1000.

            if itemfg.sell-uom = "L" then do:
              if first(fg-bin.tag) then v-ext = itemfg.sell-price.
                else v-ext = 0.
            end.

            assign v-subtot = v-subtot + v-ext
                   v-ext2 = v-ext2 + v-ext
                   v-tagtot = v-tagtot + v-ext
                   v-itemtot = v-itemtot + v-ext
                   v-sub-qty = v-sub-qty + fg-bin.qty
                   v-sub-ext = v-sub-ext + v-ext.
          end. /* do: */

          if last-of(fg-bin.tag)
              and ((v-sub-qty  ne 0) or (zbal)) then do:

              display cust.cust-no when v-frst
                     cust.type when v-frst
                     itemfg.i-no itemfg.i-name
                     itemfg.cust-po-no
                     fg-bin.tag fg-bin.job-no
                     v-sub-qty @ fg-bin.qty
                     itemfg.sell-price
                     v-sub-ext @ v-ext
              with frame itemx.
              down with frame itemx.

              IF tb_excel THEN 
                PUT STREAM excel UNFORMATTED
                    '"' (IF v-frst THEN cust.cust-no ELSE "")    '",'
                    '"' (IF v-frst THEN cust.type ELSE "")       '",'
                    '"' fg-bin.job-no                            '",'
                    '"' STRING(fg-bin.tag,"X(8)")                '",'
                    '"' itemfg.cust-po-no                        '",'
                    '"' itemfg.i-no                              '",'
                    '"' itemfg.i-name                            '",'
                    '"' STRING(v-sub-qty,"->,>>>,>>>,>>9")       '",'
                    '"' STRING(itemfg.sell-price,">,>>>,>>9.99") '",'
                    '"' STRING(v-sub-ext,"->>>,>>>,>>9.99")      '",'
                    SKIP.

              assign v-frst = no
                     v-sub-qty = 0
                     v-sub-ext = 0
                     v-print-line = yes.
          end.
        end. /* each bin */

        if v-print-line then do:
          put "--------------" to 124 skip
              "Item Total" at 94 v-itemtot to 124 format "->>,>>>,>>9.99"
              skip(1).
          assign v-itemtot = 0
                 v-print-line = no
                 v-print-cust = yes.
        end.
      end. /* avail cust */
      if last-of(itemfg.cust-no) and v-print-cust and
        ((v-subtot ne 0) or (v-subtot = 0 and zbal)) then
       put "--------------" to 124 skip
           "Customer Total" at 94 v-subtot to 124 format "->>,>>>,>>9.99"
           skip(1).
       assign v-print-cust = no.
    end. /* for each itemfg */


end procedure.

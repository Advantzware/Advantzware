


/*------------------------------------------------------------------------
    File        : InvBySalesman.p
    Purpose     :  Alphabetic Order

    Syntax      :

    Description : Reports

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttInvBySalesman NO-UNDO
        FIELD vFile AS CHAR
        FIELD vHkljlk AS CHAR.
    DEFINE DATASET dsInvBySalesman FOR ttInvBySalesman .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCust        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndCust          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vBeginPo          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndPo            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vBeSaleman        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndSalesman      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vQtyonHand        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vCustWarehouse    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vSort             AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmOut            AS CHARACTER NO-UNDO.
   
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvBySalesman.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser      = ?   THEN ASSIGN prmUser = "".
    IF vBeginCust   = ?   THEN ASSIGN vBeginCust = "".
    IF vEndCust     = ? THEN ASSIGN vEndCust = "".
    IF vBeginPo  = ?  THEN ASSIGN vBeginPo = "".
    IF vEndPo  = ?  THEN ASSIGN vEndPo = "".
    IF vBeSaleman = ? THEN ASSIGN vBeSaleman = "".
    IF vEndSalesman = ? THEN ASSIGN vEndSalesman = "".
    IF vQtyonHand  = ?  THEN ASSIGN vQtyonHand = "".
    IF vCustWarehouse  = ?  THEN ASSIGN vCustWarehouse = "".
    IF vSort           = ?  THEN ASSIGN vSort = "".
    IF prmOut          = ?  THEN ASSIGN prmOut = "No".
   
{sys/inc/var.i new shared}
 {custom/xprint.i}

   
/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR v-VERSION AS CHAR NO-UNDO. 
DEF STREAM excel.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by Salesman By?" NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" NO-UNDO.
DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 50 NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL YES  NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /*INITIAL "c:~\tmp~\r-invslm.csv"*/ NO-UNDO. 
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.

ASSIGN
    begin_cust   =     vBeginCust    
    end_cust     =     vEndCust      
    begin_cust-po  =   vBeginPo      
    end_cust-po  =     vEndPo        
    begin_slm    =     vBeSaleman    
    end_slm      =     vEndSalesman  
    tb_inc-zer   =  IF vQtyonHand = "Yes" THEN TRUE ELSE FALSE   
    tb_inc-cust  =  IF vCustWarehouse = "Yes" THEN TRUE ELSE FALSE
    tb_excel     =  IF prmOut = "Yes" THEN TRUE ELSE FALSE
    rd_sort      =      vSort  
    v-today      = TODAY
    .


DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

 DEF VAR salrep AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = vBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = vEndCust OR vEndCust = "zzzzzzzz")  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

    assign
 cocode = prmComp
 locode = usercomp.loc. 

     FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "X-VERSION"
         sys-ctrl.descrip  = "Server Name"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "Server 2003".
   END.
   IF AVAIL sys-ctrl  THEN
        v-VERSION = sys-ctrl.char-fld .
  RELEASE sys-ctrl.
 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "Xspool"
         sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "c:\spool\".
   END.
   IF AVAIL sys-ctrl  THEN
        tmp-path = sys-ctrl.char-fld .
  RELEASE sys-ctrl.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
IF prmAction = "sales"  THEN DO:
    IF prmOut = "No" THEN DO:
        ASSIGN  
    init-dir    = v-webrootpath
    lv-pdf-file = init-dir + 'InvSalesRep' 
    lv-pdf-file = lv-pdf-file + vBeginCust + STRING(TIME)
    vPdfFile   = 'InvSalesRep' + vBeginCust + STRING(TIME) + '.pdf'.
   
   run run-report.
   
   IF v-VERSION = "Server 2008" THEN do:
       OS-COPY VALUE(list-name) VALUE (tmp-path).
       PAUSE 1.
   END.
   ELSE
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
   
      CREATE ttInvBySalesman.
      ASSIGN ttInvBySalesman.vFile = vPdfFile.
      END.


      IF prmOut = "Yes" THEN DO:
          ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "InvSalesRep" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "InvSalesRep" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
              
               run run-report.
               
               CREATE ttInvBySalesman.
               ASSIGN ttInvBySalesman.vFile = vPdfFile.

      END.
 END.


/******************************************************************************************************/
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-ssman.p 3/94 RM */
/*  finished goods inventory by salesman report                               */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-ext as dec format "->>,>>>,>>9.99".
def var fcust as ch init " ".
def var tcust like fcust init "ZZZZZZZZZ".
def var fcustpo as ch init " ".
def var tcustpo like fcustpo init "ZZZZZZZZZ".
def var fsman like cust.sman init " ".
def var tsman like cust.sman init "ZZZ".
def var zbal as log format "Y/N".
def var v-sub-ext like v-ext.
def var v-cust-ext like v-ext.
def var tot-string as char format "x(5)" init "Total".
def var v-cost like itemfg.total-std-cost.
def var v-qty-onh as int format "->>>,>>>,>>9".
def var v-totcost as dec format "->>>,>>9.99".
DEF VAR jobcust   AS CHAR FORMAT "x(9)" INIT "".
def var v-frst as log.
def var v-sel-one as char format "x(30)".
def var v-sel-two as char format "x(33)".
def var v-sel-three as char format "x(20)".
def var v-custown as log format "Y/N" init "N".
def var v-cust-qty as int format "->>>,>>>,>>9".

def var sort-opt as char no-undo init "C" format "!".
    
format
  itemfg.i-no label "ITEM #"
  itemfg.i-name format "x(20)" label "DESCRIPTION"
  itemfg.cust-no label "CUST #"
  itemfg.cust-po-no column-label "CUST PO#"
  jobcust  column-label "CUST JOB#"
  itemfg.q-prod-ptd column-label "QTY PRODUCED" format "->>,>>>,>>9"
  itemfg.q-ship-ptd column-label "QTY SHIPPED" format "->>,>>>,>>9"
  v-qty-onh column-label "QTY ON HAND" format "->>>,>>>,>>9"
  v-totcost column-label "TOTAL COST" format "->>>,>>>,>>9.99"
  v-ext column-label "TOTAL VALUE"  format "->>>,>>>,>>>9.99"
  with frame itemx no-box down STREAM-IO width 300.

find first fg-ctrl where fg-ctrl.company eq cocode.

assign
 str-tit2 = "Finished Goods Inventory By Sales Rep"
 {sys/inc/ctrtext.i str-tit2 112}

 fcust        = begin_cust
 tcust        = end_cust
 fcustpo      = begin_cust-po
 tcustpo      = END_cust-po
 fsman        = begin_slm
 tsman        = end_slm
 zbal         = tb_inc-zer
 v-custown    = tb_inc-cust
 sort-opt     = SUBSTR(rd_sort,1,1).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

 PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=1mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><cpi13.3><p9>" FORM "x(400)". 
 
 /*DISPLAY WITH FRAME  r-top no-box page-top STREAM-IO width  200.*/


/*if td-show-parm then run show-param.*/

DISPLAY "" WITH FRAME r-top.
     
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN DO:
    MESSAGE "test2" v-excel-file.
   OUTPUT STREAM excel TO VALUE(v-excel-file).
   EXPORT STREAM excel DELIMITER ","
      "Item"
      "Description"
      "Cust #"
      "Customer PO#"
      "Customer Job#"
      "Quantity Produced"
      "Quantity Shipped"
      "Quantity On Hand"
      "Total Cost"
      "Total Value"
       SKIP.
END. 



  for each itemfg
        no-lock
        use-index customer
        where itemfg.company eq cocode 
      AND itemfg.cust-no ge fcust 
      and itemfg.cust-no le tcust 
      AND itemfg.cust-po-no ge fcustpo AND LOOKUP(itemfg.cust-no, custcount) NE 0
      and itemfg.cust-po-no le tcustpo,
        each cust
        no-lock
        where cust.company eq itemfg.company
          and cust.sman ge fsman  and cust.sman le tsman and
        cust.cust-no 
        eq itemfg.cust-no
        break
        by cust.sman
        by (if sort-opt eq "C" then itemfg.cust-no else itemfg.i-no)
        by (if sort-opt eq "C" then itemfg.i-no    else itemfg.cust-no):

      if first-of(cust.sman) then
      assign v-frst = yes.    

      assign v-totcost = 0
        v-ext = 0
        v-cost = 0
        v-qty-onh = 0
        v-cust-qty = 0.

    ASSIGN jobcust = itemfg.cust-job-no .
    IF jobcust = "-0" THEN ASSIGN jobcust =  "        0" .

      for each fg-bin where fg-bin.company eq cocode and
          fg-bin.i-no eq itemfg.i-no no-lock use-index i-no
          break by fg-bin.i-no:
        if fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "" then
        assign v-qty-onh = v-qty-onh + fg-bin.qty.
        else
        assign v-cust-qty = v-cust-qty + fg-bin.qty.

        if v-custown or
          (not v-custown and fg-bin.loc ne "CUST" and trim(fg-bin.cust-no) eq "") then
        do:

          if caps(fg-ctrl.inv-meth) eq "L" then
          v-cost = itemfg.last-cost.
          else
          assign v-cost = fg-bin.std-tot-cost.

          /* Calculate Cost */
          if itemfg.pur-uom eq "CS" and itemfg.case-count ne 0 then
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost)
            / itemfg.case-count.
          else
          find first uom where uom.uom eq itemfg.sell-uom and
            uom.mult ne 0 no-lock no-error.
          if avail uom then
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost / uom.mult).
          else
          assign v-totcost = v-totcost + (fg-bin.qty * v-cost) / 1000.

          if itemfg.pur-uom eq "L" then
          assign v-totcost = v-cost.

          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.job-no  eq fg-bin.job-no
                and oe-ordl.job-no2 eq fg-bin.job-no2
                and oe-ordl.i-no    eq fg-bin.i-no
              use-index job no-lock no-error.

          /* Calculate Selling Price */
          IF AVAIL oe-ordl THEN DO:
            IF (oe-ordl.t-price / oe-ordl.qty * fg-bin.qty) NE ? THEN
              v-ext = v-ext + (oe-ordl.t-price / oe-ordl.qty * fg-bin.qty).
          END.

          ELSE DO:
            if itemfg.sell-uom eq "CS" and itemfg.case-count ne 0 then
              v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / itemfg.case-count).
            else 
            if itemfg.sell-uom eq "L" then v-ext = v-ext + itemfg.sell-price.
            else do:
              find first uom
                  where uom.uom eq itemfg.sell-uom
                    and uom.mult ne 0
                  no-lock no-error.
              if avail uom then
                v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / uom.mult).
              else
                v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / 1000).
            end.
          END.
        end.

        if last-of(fg-bin.i-no) then
        do:
          /** include customer owned inventory in qty on hand
          if v-custown = true. **/
          if v-custown then
          assign v-qty-onh = v-qty-onh + v-cust-qty.
          /**
          if v-ext < 0 then assign v-ext = 0.
          if v-totcost < 0 then assign v-totcost = 0.
          **/
          if (itemfg.q-onh ne 0 ) or (itemfg.q-onh eq 0 and zbal)  then
          do:
            if line-counter ge 56 then
            page.
            if v-frst then
            do:
              find first sman where sman.company eq cust.company and
                sman.sman eq cust.sman no-lock no-error.
              if avail sman then
              put "SALES REP: " cust.sman SPACE(2) sman.sname skip(1).
              else
              put "SALES REP: " cust.sman SPACE(2) "UNKNOWN" skip(1).
              assign v-frst = NO.
            end.
            display
              itemfg.i-no itemfg.i-name itemfg.cust-po-no jobcust
              itemfg.q-prod-ptd itemfg.q-ship-ptd v-qty-onh itemfg.cust-no
              v-totcost v-ext
              with frame itemx.
            down with frame itemx.                              

IF tb_excel THEN     
        IF cust.sman NE salrep THEN
            EXPORT STREAM excel 
                       "Sales Rep: "
                       cust.sman
                       SKIP.

     ASSIGN
            salrep = cust.sman.  
                      

 IF tb_excel THEN                           
   EXPORT STREAM excel DELIMITER ","
          itemfg.i-no 
          itemfg.i-name
          itemfg.cust-no 
          itemfg.cust-po-no 
          jobcust
          itemfg.q-prod-ptd 
          itemfg.q-ship-ptd 
          v-qty-onh
          v-totcost v-ext
          SKIP.

       assign v-sub-ext = v-sub-ext + v-ext
         v-cust-ext = v-cust-ext + v-ext /* 9508 CAH */
         v-totcost = 0
         v-ext = 0
         v-qty-onh = 0.
     end.
   end. /* last-of fg-bin.i-no */
 end. /* each bin */
 if last-of
   (if sort-opt eq "C" then itemfg.cust-no else itemfg.i-no)
   and sort-opt eq "C"  and v-cust-ext ne 0
   then
 do: /* 9508 CAH */
   put "--------------" TO 135 skip
     "CUSTOMER TOTAL" TO 119 v-cust-ext TO 135 skip(1).
   assign v-cust-ext = 0.
 end.

/* MESSAGE "custsman" last-of(cust.sman).
 MESSAGE "subext" v-sub-ext.
 */

 if last-of(cust.sman) and v-sub-ext ne 0 then
 do:
   put "--------------" TO 135 skip
     "SALES REP TOTAL" TO 119 v-sub-ext TO 135 skip(1).
   assign v-sub-ext = 0.
 end.
 end. /* for each itemfg and each cust */


/*RUN custom/usrprint.p (v-prgmname, "").*/

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   /*IF tb_runExcel THEN
       OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-excel-file)).*/
END.
 


end procedure.










/*------------------------------------------------------------------------
    File        : AlphaList.p
    Purpose     :  Alphabetic Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttAlphaList NO-UNDO
        FIELD vFile AS CHAR.
    DEFINE DATASET dsAlphaList FOR ttAlphaList .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCust    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndCust      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vBeginCat     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndCat       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vBeginItem    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vCustWhse     AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vSort         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vZero         AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAlphaList.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF vBeginCust   = ?   THEN ASSIGN vBeginCust = "".
    IF vEndCust = ? THEN ASSIGN vEndCust = "".
    IF vBeginCat  = ?  THEN ASSIGN vBeginCat = "".
    IF vEndCat  = ?  THEN ASSIGN vEndCat = "".
    IF vBeginItem  = ?  THEN ASSIGN vBeginItem = "".
    IF vEndItem  = ?  THEN ASSIGN vEndItem = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmOut    = ?  THEN ASSIGN prmOut = "No".
    

   


def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

{custom/xprint.i}
{sys/inc/var.i new shared}



DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR v-VERSION AS CHAR NO-UNDO. 

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD qty LIKE fg-bin.qty.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF STREAM excel.


DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 44.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO.
DEFINE VARIABLE rd_ext_cst AS CHARACTER INITIAL "Value". 
DEFINE VARIABLE rd_pg_brk AS CHARACTER INITIAL "None".
DEFINE VARIABLE tb_cost AS LOGICAL INITIAL no .
 DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.

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
    AND (usercust.cust-no = vEndCust  OR vEndCust = "zzzzzzzz" ) NO-ERROR.
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
 locode = usercomp.loc
 tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE 
 v-today    = TODAY   . 

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

IF prmAction = "Alpha" THEN DO:
  IF prmOut = "No" THEN DO:
   ASSIGN  
    init-dir    = v-webrootpath
    lv-pdf-file = init-dir + 'ALPHA' 
    lv-pdf-file = lv-pdf-file + vBeginCust + STRING(TIME)
    vPdfFile   = 'ALPHA' + vBeginCust + STRING(TIME) + '.pdf'.
    run run-report.

     IF v-VERSION = "Server 2008" THEN do:
        OS-COPY VALUE(list-name) VALUE (tmp-path).
        PAUSE 1.
    END.
    ELSE
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    
    CREATE ttAlphaList.
    ASSIGN ttAlphaList.vFile = vPdfFile.
    END.

    IF prmOut = "Yes" THEN DO:

        ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "ALPHA" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "ALPHA" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
               
               run run-report.
               
               CREATE ttAlphaList.
               ASSIGN ttAlphaList.vFile = vPdfFile.
    END.

END.


/******************************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-i-no     like itemfg.i-no     extent 2  init ["","zzzzzzzzzzzzzzz"].
def var v-cust     like itemfg.cust-no  extent 2  init ["","zzzzzzzz"].
def var v-procat   like itemfg.procat   extent 2  init ["","zzzzz"].
def var v-break    as   char format "!" init "N".
def var v-prt-cost as   log format "Cost/Value" init no.
def var v-custown  as   log format "Y/N" init no.
def var v-sort-by  as   log format "Y/N" init no.
def var v-zero     as   log format "Y/N" init no.
def var v-sho-cost as   log format "Y/N" init no.

def var v-first       as log extent 2 init yes.
def var v-page-break  as char.
def var v-label1      as char format "x(14)" extent 3.
def var v-label2      as char format "x(14)".
def var v-price       as dec.
def var v-cost        as dec.
def var v-tq-onh      as dec extent 2.
def var v-tq-ono      like v-tq-onh.
def var v-tq-alloc    like v-tq-onh.
def var v-tq-avail    like v-tq-onh.
def var v-tprice      like v-tq-onh.
def var v-qty-onh     like itemfg.q-onh.

DEF VAR excelheader AS CHAR NO-UNDO.

form header skip(1)
            v-page-break format "x(200)"
            
    with frame r-top-2 STREAM-IO width 300 no-labels no-box no-underline page-top.

form header skip(1)
            "               "
            "             "
            " "
            "  "
            v-label1[1]
            "     "
            "           "
            "           "
            "   "
            "           "
            "         "
            skip
           
            "ITEM           "
            "DESCRIPTION  "
            "CAT "
            " UOM"
            v-label1[2]
            "SELL PRICE  "
            " ON HAND"
            "   ON ORDER"
            "   CUST ORDER"
            "   QTY AVAIL "
            v-label2
            skip
            
            "---------------"
            "-------------"
            "-----"
            "--"
            v-label1[3]
            "------------"
            "-----------"
            "-----------"
            "-----------"
            "--------------"
            "-------------"
            skip
           
    with frame r-top-3 STREAM-IO width 300 no-labels no-box no-underline page-top.
    
form
    itemfg.i-no                                 
    itemfg.i-name           format "x(13)"          
    itemfg.procat                               
    itemfg.sell-uom
    itemfg.total-std-cost   format "->>,>>>,>>9.99" 
    /*itemfg.sell-price       format ">,>>>,>>9.99"*/
    itemfg.sell-price       format " >>>,>>9.99"

    v-qty-onh               format "->>,>>>,>>9"    
    itemfg.q-ono            format "->>,>>>,>>9"    
    itemfg.q-alloc          format "->>,>>>,>>9"    
    itemfg.q-avail          format " ->>>,>>>,>>9"    
    v-price                 format "->>,>>>,>>9.99" 
    SKIP  

    
   with frame itemx no-labels no-box down STREAM-IO WIDTH 300.

ASSIGN
    str-tit2 = "Alphabetic Item Report"
 /* str-tit2 = c-win:title */
 {sys/inc/ctrtext.i str-tit2 112}

 v-i-no[1]    = vBeginItem
 v-i-no[2]    = vEndItem
 v-cust[1]    = vBeginCust
 v-cust[2]    = vEndCust
 v-procat[1]  = vBeginCat
 v-procat[2]  = vEndCat
 v-break      = SUBSTR(rd_pg_brk,1,1)
 v-sho-cost   = tb_cost
 v-prt-cost   = rd_ext_cst EQ "Cost"
 v-custown    = IF vCustWhse = "yes" THEN TRUE ELSE FALSE
 v-sort-by    = IF vSort = "yes" THEN TRUE ELSE FALSE
 v-zero       = IF vZero = "yes" THEN TRUE ELSE FALSE.

 
list-name = init-dir + "tmp" + string(time).
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
 PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI13.3><p9>" FORM "x(320)". 

find first fg-ctrl where fg-ctrl.company eq cocode no-lock.
  
  assign
   v-label1[1] = if fg-ctrl.inv-meth eq "A" then
                   "       AVERAGE" else "          LAST"
   v-label1[2] = "          COST"
   v-label1[3] = "--------------"
   v-label2    = if v-prt-cost then
                   "          COST" else "         VALUE".

 IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(v-excel-file).
  excelheader = "FG ITEM#,DESCRIPTION,PROD CAT,UOM," + TRIM(v-label1[1]) +
                " " + TRIM(v-label1[2]) + ",SELLING PRICE,ON HAND,ON ORDER," +
                "CUSTOMER ORDERS,QTY AVAIL,TOTAL " + TRIM(v-label2).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

                 
  if not v-sho-cost then v-label1 = "".
  
  for each itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    ge v-i-no[1]
        and itemfg.i-no    le v-i-no[2]
        and itemfg.cust-no ge v-cust[1]
        and itemfg.cust-no le v-cust[2]
        and itemfg.procat  ge v-procat[1]
        and itemfg.procat  le v-procat[2]
      no-lock
      
      break by (if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1")
            by (if v-sort-by then itemfg.i-no else itemfg.i-name)
            
      with frame itemx.

	IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
      
    if first-of(if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1") then
      v-first[2] = yes.
       
    v-qty-onh  = 0.
     
    for each fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq itemfg.i-no
        use-index i-no no-lock:
        
      if v-custown or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq "") then
        v-qty-onh = v-qty-onh + fg-bin.qty.
    end. /* each bin */

    if v-qty-onh ne 0 or not v-zero then do:
      if v-first[2] then do:
        assign
         v-first[2]   = no
         v-page-break = if v-break eq "C" then
                          ("Customer: " + trim(itemfg.cust-no))
                        else
                        if v-break eq "P" then
                          ("Product Category: " + trim(itemfg.procat))
                        else "".  
        
        IF v-first[1] THEN DO:
          v-first[1] = NO.
          DISPLAY WITH frame r-top.
          IF v-break NE "N" THEN DISPLAY WITH FRAME r-top-2.
          DISPLAY WITH frame r-top-3.
        END.

        ELSE PAGE.
      end.
    
      if v-prt-cost then do:
        v-cost = itemfg.total-std-cost.
      
        if itemfg.prod-uom ne "EA" then
          run sys/ref/convcuom.p (itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                  v-cost, output v-cost).
                                
        v-price = v-cost * v-qty-onh.  
      end.
    
      else do:
        find first uom
            where uom.uom  eq itemfg.sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-price = v-qty-onh * itemfg.sell-price /
                  (if avail uom then uom.mult else 1000).
                 
        if itemfg.sell-uom eq "CS" then
          v-price = v-qty-onh / itemfg.case-count * itemfg.sell-price.
        
        else
        if itemfg.sell-uom eq "L" then v-price = itemfg.sell-price.
      end.
      
      if v-price eq ? then v-price = 0.
              
      display itemfg.i-no
              itemfg.i-name
              itemfg.procat
              itemfg.sell-uom
              itemfg.prod-uom when v-prt-cost @ itemfg.sell-uom
              itemfg.total-std-cost when v-sho-cost
              itemfg.sell-price
              v-qty-onh
              itemfg.q-ono
              itemfg.q-alloc
              (v-qty-onh + itemfg.q-ono - itemfg.q-alloc) @ itemfg.q-avail
              v-price

          with frame itemx.
      down with frame itemx.

      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' itemfg.i-no                                               '",'
            '"' itemfg.i-name                                             '",'
            '"' itemfg.procat                                             '",'
            '"' (IF v-prt-cost THEN itemfg.prod-uom ELSE itemfg.sell-uom) '",'
            '"' (IF v-sho-cost THEN itemfg.total-std-cost ELSE 0)         '",'
            '"' itemfg.sell-price                                         '",'
            '"' v-qty-onh                                                 '",'
            '"' itemfg.q-ono                                              '",'
            '"' itemfg.q-alloc                                            '",'
            '"' (v-qty-onh + itemfg.q-ono - itemfg.q-alloc)               '",'
            '"' v-price                                                   '",'
            SKIP.

      
      assign
       v-tq-onh[1]   = v-tq-onh[1]   + v-qty-onh
       v-tq-ono[1]   = v-tq-ono[1]   + itemfg.q-ono
       v-tq-alloc[1] = v-tq-alloc[1] + itemfg.q-alloc
       v-tq-avail[1] = v-tq-avail[1] + 
                       (v-qty-onh + itemfg.q-ono - itemfg.q-alloc)
       v-tprice[1]   = v-tprice[1]   + v-price.
    end.
     
    if not v-first[2]                                          and
       last-of(if v-break eq "C" then itemfg.cust-no else
               if v-break eq "P" then itemfg.procat  else "1") then do:
               
      if v-break ne "N" then do:
        put skip(1).
        
        display "Cust Total"                          @ itemfg.i-name
                  "ProdCat Total" when v-break eq "P" @ itemfg.i-name
                v-tq-onh[1]                           @ v-qty-onh
                v-tq-ono[1]                           @ itemfg.q-ono
                v-tq-alloc[1]                         @ itemfg.q-alloc
                v-tq-avail[1]                         @ itemfg.q-avail
                v-tprice[1]                           @ v-price
            with frame itemx.
        down with frame itemx.
        
        put skip(1).
      end.
     
      assign
       v-tq-onh[2]   = v-tq-onh[2]   + v-tq-onh[1]
       v-tq-ono[2]   = v-tq-ono[2]   + v-tq-ono[1]
       v-tq-alloc[2] = v-tq-alloc[2] + v-tq-alloc[1]
       v-tq-avail[2] = v-tq-avail[2] + v-tq-avail[1]
       v-tprice[2]   = v-tprice[2]   + v-tprice[1]
       
       v-tq-onh[1]   = 0
       v-tq-ono[1]   = 0
       v-tq-alloc[1] = 0
       v-tq-avail[1] = 0
       v-tprice[1]   = 0.
    end.
    
    if not v-first[1]                                       and
       last(if v-break eq "C" then itemfg.cust-no else
            if v-break eq "P" then itemfg.procat  else "1") then do:
               
      hide frame r-top-2 no-pause.
  
      if v-break ne "N" then page.
      else put skip(2).

      display "Grand Total"                         @ itemfg.i-name
              v-tq-onh[2]                           @ v-qty-onh
              v-tq-ono[2]                           @ itemfg.q-ono
              v-tq-alloc[2]                         @ itemfg.q-alloc
              v-tq-avail[2]                         @ itemfg.q-avail
              v-tprice[2]                           @ v-price
          with frame itemx.
      down with frame itemx.
    end.      
  end. /* each itemfg */
        IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
END.

/* RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */

end procedure.








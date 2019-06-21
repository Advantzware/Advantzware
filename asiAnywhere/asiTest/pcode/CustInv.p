


/*------------------------------------------------------------------------
    File        : Custinv.p
    Purpose     :  Customer Inventory

    Syntax      :

    Description : 

    Author(s)   : Sewa
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustinvoice NO-UNDO
        FIELD abc AS CHAR
        FIELD vFile AS CHAR.
    DEFINE DATASET dsCustinvoice FOR ttCustinvoice .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCust    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndCust      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vFromlistclass    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vTolistclass AS CHARACTER NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustinvoice.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF vBeginCust   = ?   THEN ASSIGN vBeginCust = "".
    IF vEndCust = ? THEN ASSIGN vEndCust = "".
    IF vFromlistclass  = ?  THEN ASSIGN vFromlistclass = "".
    IF vTolistclass    = ?  THEN ASSIGN vTolistclass   = "".
    IF prmAction       = ?  THEN ASSIGN prmAction = "".
    IF prmOut = ?     THEN ASSIGN prmOut = "No".
  
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
{custom/xprint.i}
{sys/inc/var.i new shared}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF TEMP-TABLE tt-report NO-UNDO LIKE report
FIELD qty LIKE fg-bin.qty.
DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.
DEF STREAM excel.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 48.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Item#" .
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO.
DEFINE VARIABLE rd_ext_cst AS CHARACTER INITIAL "Value". 
DEFINE VARIABLE rd_pg_brk AS CHARACTER INITIAL "None".
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO. 
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U      NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
 DEF VAR  tmp-path AS CHAR NO-UNDO. 
 DEF VAR v-VERSION AS CHAR NO-UNDO. 
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = vBeginCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = vEndCust OR vEndCust = "zzzzzzzz") NO-ERROR.
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
 v-today   = TODAY .

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

IF prmAction = "Cust" THEN DO:
      IF prmOut = "No" THEN DO:
          assign
              init-dir    =  v-webrootpath
              lv-pdf-file = init-dir + 'Custinvoice'.
          lv-pdf-file = lv-pdf-file + vBeginCust + STRING(TIME).
          vPdfFile   = 'Custinvoice' + vBeginCust + STRING(TIME) + '.pdf'.
          run run-report.
        IF v-VERSION = "Server 2008" THEN do:
            OS-COPY VALUE(list-name) VALUE (tmp-path).
            PAUSE 1.
        END.
        ELSE
            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

        CREATE ttCustinvoice.
        ASSIGN ttCustinvoice.vFile = vPdfFile.
        END.

        IF prmOut = "Yes" THEN DO:
                ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "Custinvoice" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "Custinvoice" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
               
               run run-report.
               
               CREATE ttCustinvoice.
               ASSIGN ttCustinvoice.vFile = vPdfFile.
        END.
 END.

PROCEDURE run-report :
/* ---------------------------------------------- fg/rep/fg-royal.p 07/01 JLF */
/* FG Inventory Per Customer by Item Class                                    */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def var v-page-brk      as   char               format "x(132)".
def var v-pal-cnt       like fg-bin.unit-count.
def var v-stat          as   CHAR NO-UNDO.
def var v-class         as   CHAR NO-UNDO.
def var li              as   INT NO-UNDO.
DEF VAR lv-rowid        AS   ROWID NO-UNDO.
DEF VAR excelheader     AS   CHAR NO-UNDO.

form header skip(1)
            v-page-brk
            skip(1)

    with frame r-top2 no-box page-top STREAM-IO width  200.

form header v-page-brk
            skip(1)

    with frame r-top3 no-box page-top STREAM-IO width 200.


ASSIGN
 str-tit2 = "FG Inventory Per Customer by Item Class"
 {sys/inc/ctrtext.i str-tit2 112}

 v-class    = vFromlistclass
 vFromlistclass = "".
DO li = 1 TO NUM-ENTRIES(v-class):
  vFromlistclass = vFromlistclass + " " + (IF ENTRY(li,v-class) EQ "" THEN "Spaces"
                                   ELSE ENTRY(li,v-class)).
END.
vFromlistclass = TRIM(vFromlistclass).
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
                                                                           /*4*/
PUT "<PDF=DIRECT><OLANDSCAPE><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=1mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI13.3><p9>" FORM "x(350)". 
DISPLAY WITH FRAME  r-top no-box page-top STREAM-IO width  200.

DEF VAR first-page AS LOG INIT YES NO-UNDO.
DEF VAR v-line-count AS INT NO-UNDO.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(v-excel-file).
  excelheader = "Item#,Customer Part#,Description,Re-Order Point,"
              + "Release PO#,Total On-Hand,Pallet/Count,Release Quantity,"
              + "Date Required".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

for each itemfg
    where itemfg.company eq cocode
      and itemfg.cust-no ge vBeginCust
      and itemfg.cust-no le vEndCust
      AND itemfg.cust-no NE ""
      AND  /* LOOKUP(itemfg.class,v-class) GT 0 */   itemfg.CLASS ge v-class
          and itemfg.CLASS le vTolistclass
    NO-LOCK
    BREAK BY itemfg.cust-no
          BY itemfg.class
          BY (IF rd_sort EQ "Item#" THEN itemfg.i-no ELSE "")
          BY itemfg.part-no:

	IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
    
  IF FIRST-OF(itemfg.class) THEN DO:
    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ itemfg.cust-no
        NO-LOCK NO-ERROR.

    v-page-brk = "Customer: " +
                 TRIM(IF AVAIL cust THEN cust.name ELSE itemfg.cust-no) +
                 "   " +
                 "Inventory Class: " +
                 itemfg.class.

    if FIRST(itemfg.class) then /*view frame r-top2.*/
    DO:
       IF NOT first-page THEN
          DISPLAY WITH FRAME r-top2.
       ELSE
          DISPLAY WITH FRAME r-top3.
    END.
    IF NOT first-page THEN
       page.
    ELSE
       first-page = NO.
  END.

  ASSIGN
   v-pal-cnt = 0
   lv-rowid  = ?.
    
  for each fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq itemfg.i-no
      use-index co-ino no-lock
      break by fg-bin.qty desc:
    
    ASSIGN
     v-pal-cnt = (IF fg-bin.case-count   EQ 0 THEN 1 ELSE fg-bin.case-count) *
                 (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit) *
                 (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet)
     lv-rowid  = ROWID(fg-bin).
    
    LEAVE.
  END.

  FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.
  
  RELEASE oe-ordl.
  RELEASE oe-rel.

  FOR EACH oe-ordl
      WHERE oe-ordl.company  EQ cocode
        AND oe-ordl.i-no     EQ itemfg.i-no
      NO-LOCK,
      FIRST oe-ord OF oe-ordl
      WHERE (oe-ord.cust-no EQ itemfg.cust-no OR itemfg.cust-no EQ "")
      NO-LOCK
      BY oe-ordl.req-date DESC
      BY oe-ordl.ord-no   DESC:
    LEAVE.
  END.

  IF AVAIL oe-ordl THEN
  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      BY oe-rel.rel-date:
    {oe/rel-stat.i v-stat}
    IF INDEX("CZ",v-stat) EQ 0 THEN LEAVE.
  END.

  DISPLAY itemfg.i-no                           COLUMN-LABEL "Item#" 
          itemfg.part-no                        COLUMN-LABEL "Customer Part#"
                                              /*FORMAT "x(14)" SPACE(8)*/
          itemfg.i-name                         COLUMN-LABEL "Description"
          itemfg.ord-level                      COLUMN-LABEL "Re-Order"
                                                /*FORMAT "->,>>>,>>>" SPACE(8)*/
          oe-rel.po-no WHEN AVAIL oe-rel        COLUMN-LABEL "Release PO#"   /*SPACE(10)*/
          itemfg.q-onh                          COLUMN-LABEL "QTY On-Hand"
                                                /*FORMAT "->,>>>,>>>" SPACE(20)*/
          v-pal-cnt                             COLUMN-LABEL "Pal/Cnt"
                                                /*FORMAT "->>,>>>" SPACE(20) */
          oe-ordl.req-date WHEN AVAIL oe-ordl   COLUMN-LABEL "Req Date"
                                                FORMAT "99/99/99"
          "__________"                          COLUMN-LABEL "Release Qty"  /*SPACE(35) */
                    skip(1)

      with frame detail{1} no-box no-attr-space stream-io width 232 down.
  down with frame detail{1}.

    IF tb_excel THEN 
    PUT STREAM excel UNFORMATTED
        '"' itemfg.i-no                                            '",'
        '"' itemfg.part-no                                         '",'
        '"' itemfg.i-name                                          '",'
        '"' STRING(itemfg.ord-level,"->>>,>>>,>>>")                '",'
        '"' (IF AVAIL oe-rel THEN oe-ordl.po-no
             ELSE "")                                              '",'
        '"' STRING(itemfg.q-onh,"->>>,>>>,>>>")                    '",'
        '"' STRING(v-pal-cnt,"->>,>>>")                            '",'
        '"' "__________"                                           '",'
        '"' (IF AVAIL oe-ordl AND
               oe-ordl.req-date NE ? THEN STRING(oe-ordl.req-date)
               ELSE "")                                            '",'
        SKIP.


end.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  
END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.









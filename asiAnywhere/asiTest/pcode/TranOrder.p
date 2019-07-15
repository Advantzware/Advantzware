/*------------------------------------------------------------------------
    File        : TranOrder.p
    Purpose     :  Print Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTranOrder NO-UNDO
FIELD vFile AS CHAR
FIELD a AS CHAR.
DEFINE DATASET dsTranOrder FOR ttTranOrder .

    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCust    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndCust      AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER vBeginPo      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndPo        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vBeginItem    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vEndItem      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vBeginJob     AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vEndJob       AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER vBeginJob2    AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vEndJob2      AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER vBeginOrd     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vEndOrd       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vDetailed     AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER vCloseOrd     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER vNewOrd       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTranOrder.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?   THEN ASSIGN prmUser = "".
    IF vBeginCust   = ?   THEN ASSIGN vBeginCust = "".
    IF vEndCust = ? THEN ASSIGN vEndCust = "".
    IF vBeginPo  = ?  THEN ASSIGN vBeginPo = "".
    IF vEndPo  = ?  THEN ASSIGN vEndPo = "".
    IF vBeginItem  = ?  THEN ASSIGN vBeginItem = "".
    IF vEndItem  = ?  THEN ASSIGN vEndItem = "".
    IF vBeginJob  = ?  THEN ASSIGN vBeginJob = "".
    IF vEndJob  = ?  THEN ASSIGN vEndJob = "".
    IF vBeginJob2  = ?  THEN ASSIGN vBeginJob2 = "".
    IF vEndJob2  = ?  THEN ASSIGN vEndJob2 = "".
    IF vBeginOrd  = ?  THEN ASSIGN vBeginOrd = 0.
    IF vEndOrd  = ?  THEN ASSIGN vEndOrd = 0.

    

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR v-VERSION AS CHAR NO-UNDO. 
{custom/xprint.i}
{custom/vxprint.i}
{sys/inc/var.i new shared}


    
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD qty LIKE fg-bin.qty.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

DEF STREAM excel.


DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-trnord.csv". 
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 99.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)" INITIAL "Courier New Size=9 (13CPI)" .
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)" INITIAL "11". 
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L".
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 5. 
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO. 
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no .
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO. 
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
 DEF VAR  tmp-path AS CHAR NO-UNDO. 

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
 tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE
 v-today = TODAY   . 

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
   ASSIGN  
    init-dir    = v-webrootpath .
IF prmAction = "Trans" THEN DO:
    IF prmOut = "No" THEN DO:
        assign
        init-dir    = v-webrootpath
        lv-pdf-file = init-dir + 'ORDER'.
        lv-pdf-file = lv-pdf-file + vBeginCust + STRING(TIME).
        vPdfFile   = 'ORDER' + vBeginCust + STRING(TIME) + '.pdf'.
        run run-report.
        /*
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
        RUN vprintFile(list-name). 
        OS-COMMAND SILENT VALUE ("createpdf " + list-name). 
        */
        IF v-VERSION = "Server 2008" THEN do:
            OS-COPY VALUE(list-name) VALUE (tmp-path). 
            PAUSE 1.
        END.
        ELSE 
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").


        CREATE ttTranOrder.
        ASSIGN ttTranOrder.vFile = vPdfFile.
        
    END. /* IF prmOut = "No" THEN DO: */
    IF prmOut = "Yes"  THEN DO:
        assign
        init-dir    = v-webrootpath
        v-excel-file = init-dir + 'ORDER' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        
        vPdfFile   = 'ORDER' + 
                        STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
             
        run run-report.
        
        CREATE ttTranOrder.
        ASSIGN ttTranOrder.vFile = vPdfFile.
    END. /* IF prmOut = "Yes"  THEN DO: */
END. /* IF prmAction = "Trans" THEN DO: */

    
    
PROCEDURE run-report :
    /* ---------------------------------------------- fg/rep/fg-trans.p 07/96 JLF */
    /* finished goods transactions by order                                       */
    /* -------------------------------------------------------------------------- */
    
    {sys/form/r-topw.f}
    
    def var fcus        like itemfg.cust-no.
    def var tcus        like fcus               init "zzzzzzzz".
    def var fitm        like itemfg.i-no.
    def var titm        like fitm               init "zzzzzzzzzzzzzzz".
    def var fp-o        like oe-ordl.po-no.
    def var tp-o        like fp-o               init "zzzzzzzzzzzzzzz".
    def var ford        like oe-ord.ord-no      format ">>>>>>".
    def var tord        like ford               init 999999.
    def var fjob        like oe-ordl.job-no.
    def var tjob        like fjob               init "zzzzzz".
    def var fjob2       like oe-ordl.job-no2    format "99".
    def var tjob2       like fjob2              init 99.
    def var vdet        as   log                init yes    format "Detail/Summary".
    def var vinc        as   log                init yes    format "Yes/No".
    def var vinc1       as   log                init yes    format "Yes/No".
    def var v-qty       as   int.
    def var v-qop       as   int.
    def var v-qoh       as   int.
    def var v-bal       as   int.
    def var v-val       as   dec.
    def var v-job       as   char               format "x(9)".
    def var v-cus       like itemfg.cust-no.
    def var v-itm       like itemfg.i-no.
    def var v-price     like oe-ordl.price.
    def var v-printed   as   log.
    
    DEF VAR li-tqty AS INT NO-UNDO.
    DEF VAR excelheader AS CHAR NO-UNDO.
    
    form header
          /*  "        " */
            "               "
            "               "
            "         "
            "         "
            "        "
            " "
            "          "
            "           "
            "          "
            "         "
            "              "                    skip
    
          /*  "Cust #  " */
            "Item #         "
            "Cust PO #      "
            "    Job #"
            "Qty Order"
            "  Date  "
            "C"
            "       Qty"
            "Qty OnHand"
            "Qty Balanc"
            "Sell Price"
            "Tot Value"                    skip
    
         /*   "--------" */
            "---------------"
            "---------------"
            "---------"
            "---------"
            "--------"
            "-"
            "----------"
            "-----------"
            "---------"
            "---------"
            "----------"                    SKIP
            skip(1)
            with no-box page-top STREAM-IO width 132 frame top.
    
    
    assign
         str-tit2 = "FG Transactions by Order --  " + "Cust#:" + vBeginCust
        /* str-tit2 = c-win:title */
     {sys/inc/ctrtext.i str-tit2 112}
    
     fcus   = vBeginCust
     tcus   = vEndCust
     fitm   = vBeginItem
     titm   = vEndItem
     fp-o   = vBeginPo
     tp-o   = vEndPo
     fjob   = fill(" ",6 - length(trim(vBeginJob))) +
              trim(vBeginJob) + STRING(int(vBeginJob2),"99")
     tjob   = fill(" ",6 - length(trim(vEndJob)))   +
              trim(vEndJob)   + STRING(int(vEndJob2),"99")
     ford   = vBeginOrd
     tord   = vEndOrd
     vdet   = IF vDetailed = "yes" THEN TRUE ELSE FALSE
     vinc   = IF vCloseOrd = "yes" THEN TRUE ELSE FALSE
     vinc1  = IF vNewOrd = "yes" THEN TRUE ELSE FALSE.
         
    /* {sys/inc/print1.i} */
     list-name = init-dir + "\tmp" + string(time).
    
    {sys/inc/outprint.i value(lines-per-page)}
     /*PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI10.5><P11>" FORM "x(350)". */
    
     PUT UNFORMATTED
                   "<PRINT=NO><SILENT=TRUE><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" 
                                lv-pdf-file ".pdf><CPI10.8><P11>".
    
    IF tb_excel THEN DO:
       OUTPUT STREAM excel TO VALUE(v-excel-file).
       excelheader = "Cust #,Item #,Cust PO #,Job #,Qty Ordered,"
                   + "Trans Date,C,Qty,Qty On Hand,Balance Remaining,Selling Price,Total Value".
       PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.
    
    DISPLAY "" WITH FRAME r-top.
    
    DISPLAY WITH FRAME TOP.
    
    FOR EACH cust NO-LOCK
        WHERE cust.company          EQ cocode
    
          AND cust.cust-no          GE fcus
          AND cust.cust-no          LE tcus,
    
        EACH itemfg NO-LOCK
        WHERE itemfg.company        EQ cust.company
          AND itemfg.cust-no        EQ cust.cust-no
    
          AND itemfg.i-no           GE fitm
          AND itemfg.i-no           LE titm
    
        USE-INDEX customer,
    
        EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company       EQ itemfg.company
          AND oe-ordl.i-no          EQ itemfg.i-no
          AND oe-ordl.cust-no       EQ cust.cust-no
          AND (oe-ordl.opened       EQ YES OR vinc)
    
          AND oe-ordl.po-no         GE fp-o
          AND oe-ordl.po-no         LE tp-o
    
          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                     TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99"))
                                    GE fjob
          AND STRING(FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                     TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99"))
                                    LE tjob
    
          AND oe-ordl.ord-no        GE ford
          AND oe-ordl.ord-no        LE tord
    
        USE-INDEX item,
    
        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company        EQ oe-ordl.company
          AND oe-ord.ord-no         EQ oe-ordl.ord-no
    
        BREAK BY itemfg.cust-no
              BY itemfg.i-no
              BY oe-ordl.ord-no
    
        WITH FRAME main NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 132 DOWN:
    
    	IF LOOKUP(cust.cust-no, custcount) = 0 THEN NEXT.
        
      IF FIRST-OF(itemfg.cust-no) THEN v-cus = itemfg.cust-no.
           
      IF FIRST-OF(itemfg.i-no) THEN v-itm = itemfg.i-no.
    
      IF FIRST-OF(oe-ordl.ord-no) THEN EMPTY TEMP-TABLE tt-report.
    
      ASSIGN
       v-job     = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                   TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")
       v-price   = oe-ordl.t-price / oe-ordl.qty * 1000
       v-printed = NO.
    
      IF TRIM(oe-ordl.job-no) NE "" THEN
      FOR EACH fg-rcpth NO-LOCK
          WHERE fg-rcpth.company EQ oe-ordl.company
            AND fg-rcpth.job-no  EQ oe-ordl.job-no
            AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
            AND fg-rcpth.i-no    EQ oe-ordl.i-no
          USE-INDEX job,
    
          EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          USE-INDEX rm-rdtl
    
          BREAK BY fg-rdtlh.loc
                BY fg-rdtlh.loc-bin
                BY fg-rdtlh.tag
                BY fg-rcpth.trans-date
                BY fg-rcpth.r-no:
    
        CREATE tt-report.
        ASSIGN
         tt-report.term-id = ""
         tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                             STRING(MONTH(fg-rcpth.trans-date),"99")  +
                             STRING(DAY(fg-rcpth.trans-date),"99")
         tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
         tt-report.rec-id  = RECID(fg-rdtlh).
      END.
    
      FOR EACH oe-boll NO-LOCK
          WHERE oe-boll.company  EQ cocode
            AND oe-boll.ord-no   EQ oe-ordl.ord-no
            AND oe-boll.i-no     EQ oe-ordl.i-no
            AND oe-boll.line     EQ oe-ordl.line
            AND (oe-ordl.job-no  EQ ""             OR
                 oe-boll.job-no  NE oe-ordl.job-no OR
                 oe-boll.job-no2 NE oe-ordl.job-no2)
            AND CAN-FIND(FIRST oe-bolh
                         WHERE oe-bolh.company EQ oe-boll.company
                           AND oe-bolh.b-no    EQ oe-boll.b-no
                           AND oe-bolh.posted  EQ YES)
          USE-INDEX ord-no
          BREAK BY oe-boll.job-no
                BY oe-boll.job-no2
                BY oe-boll.loc
                BY oe-boll.loc-bin
                BY oe-boll.tag
                BY oe-boll.cust-no:
    
        IF LAST-OF(oe-boll.cust-no) THEN
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company EQ oe-boll.company
              AND fg-rcpth.b-no    EQ oe-boll.b-no
              AND fg-rcpth.job-no  EQ oe-boll.job-no
              AND fg-rcpth.job-no2 EQ oe-boll.job-no2
              AND fg-rcpth.i-no    EQ oe-boll.i-no
            USE-INDEX b-no,
    
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              AND fg-rdtlh.tag       EQ oe-boll.tag
              AND fg-rdtlh.loc       EQ oe-boll.loc
              AND fg-rdtlh.loc-bin   EQ oe-boll.loc-bin
              AND fg-rdtlh.cust-no   EQ oe-boll.cust-no
            USE-INDEX rm-rdtl
    
            BY fg-rcpth.trans-date
            BY fg-rcpth.r-no:
    
          CREATE tt-report.
          ASSIGN
           tt-report.term-id = ""
           tt-report.key-01  = STRING(YEAR(fg-rcpth.trans-date),"9999") +
                               STRING(MONTH(fg-rcpth.trans-date),"99")  +
                               STRING(DAY(fg-rcpth.trans-date),"99")
           tt-report.key-02  = STRING(fg-rcpth.r-no,"9999999999999")
           tt-report.rec-id  = RECID(fg-rdtlh).
        END.
      END.
    
      IF LAST-OF(oe-ordl.ord-no) THEN DO:
        EMPTY TEMP-TABLE tt-fg-bin.
    
        for each tt-report where tt-report.term-id eq "",
            first fg-rdtlh where recid(fg-rdtlh) eq tt-report.rec-id no-lock,
            first fg-rcpth where fg-rcpth.r-no eq fg-rdtlh.r-no NO-LOCK
            break by tt-report.key-01
                  by tt-report.key-02:
    
          FIND FIRST tt-fg-bin
              WHERE tt-fg-bin.company EQ fg-rcpth.company
                AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no
              NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-fg-bin THEN DO:
            CREATE tt-fg-bin.
            ASSIGN
             tt-fg-bin.company = fg-rcpth.company
             tt-fg-bin.job-no  = fg-rcpth.job-no
             tt-fg-bin.job-no2 = fg-rcpth.job-no2
             tt-fg-bin.loc     = fg-rdtlh.loc
             tt-fg-bin.loc-bin = fg-rdtlh.loc-bin
             tt-fg-bin.tag     = fg-rdtlh.tag
             tt-fg-bin.cust-no = fg-rdtlh.cust-no.
          END.
    
          ASSIGN
           li-tqty       = (fg-rdtlh.qty *
                            IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 1)
           tt-fg-bin.qty = (IF fg-rcpth.rita-code EQ "C" THEN 0
                                                         ELSE tt-fg-bin.qty) +
                           li-tqty.
    
          RELEASE tt-fg-bin.
          tt-report.qty = 0.
          FOR EACH tt-fg-bin:
            tt-report.qty = tt-report.qty + tt-fg-bin.qty.
          END.
        END.
    
        for each tt-report where tt-report.term-id eq "",
            first fg-rdtlh where recid(fg-rdtlh) eq tt-report.rec-id no-lock,
            first fg-rcpth
            where fg-rcpth.r-no      eq fg-rdtlh.r-no
              and fg-rcpth.rita-code ne "T"
            NO-LOCK
            break by tt-report.key-01
                  by tt-report.key-02:
    
          IF FIRST(tt-report.key-01) THEN
            ASSIGN
             v-qty = oe-ordl.qty
             v-bal = oe-ordl.qty.
     
          v-bal = v-bal + (fg-rdtlh.qty *
                           IF fg-rcpth.rita-code EQ "S" THEN -1 ELSE 0).
    
          if last(tt-report.key-01) or vdet then do:
            ASSIGN
             v-qoh = tt-report.qty
             v-val = round(v-qoh * (oe-ordl.t-price / oe-ordl.qty),2).
    
            display /*v-cus*/
                    v-itm
                    oe-ordl.po-no
                    fill(" ",6 - length(trim(fg-rcpth.job-no))) +
                     trim(fg-rcpth.job-no) + "-" + STRING(fg-rcpth.job-no2,"99")
                                                          FORMAT "x(9)"         
                    v-qty when v-qty ne 0                 FORMAT ">,>>>,>>9"    
                    fg-rcpth.trans-date when vdet         FORMAT "99/99/99"     
                    fg-rcpth.rita-code when vdet                                
                    fg-rdtlh.qty when vdet                format "->>>,>>9"     
                    v-qoh                                 format "->,>>>,>>9"   
                    v-bal when v-bal ge 0                 format "->,>>>,>>9"   
                      0 when v-bal lt 0 @ v-bal                                 
                    v-price                               format "->>,>>9.99"   
                    v-val                                 format "->>>,>>9.99"  
    
                with frame main.
            down with frame main.
        IF tb_excel THEN 
                   PUT STREAM excel UNFORMATTED
                      '"' v-cus                                         '",'
                      '"' v-itm                                         '",'
                      '"' oe-ordl.po-no                                 '",'
                      '"' FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                          TRIM(fg-rcpth.job-no) + "-" + STRING(fg-rcpth.job-no2,"99") '",'
                      '"' (IF v-qty NE 0 THEN v-qty ELSE 0)             '",'
                      '"' (IF vdet AND fg-rcpth.trans-date <> ? THEN
                            STRING(fg-rcpth.trans-date,"99/99/99")
                          ELSE "")    '",'
                      '"' (IF vdet THEN fg-rcpth.rita-code ELSE "")     '",'
                      '"' (IF vdet THEN STRING(fg-rdtlh.qty) ELSE "")           '",'
                      '"' v-qoh                                         '",'
                      '"' (IF v-bal GE 0 THEN v-bal ELSE 0)             '",'
                      '"' v-price                                       '",'
                      '"' v-val                                         '",'
                      SKIP.
            
            assign
             v-printed = yes
             v-cus     = ""
             v-itm     = "".
          end.
    
          if last(tt-report.key-01) and vdet then put skip(1).
        end.
    
        if not v-printed                                and
           ((vinc1 and index("CZ",oe-ord.stat) eq 0) or
            (vinc and index("CZ",oe-ord.stat) gt 0))    then do:
          display v-cus
                  v-itm
                  oe-ordl.po-no
                  v-job
                  oe-ordl.qty       @ v-qty
                  oe-ord.ord-date   @ fg-rcpth.trans-date
                  v-price
    
              with frame main.
    
          put skip(1).
        
          IF tb_excel THEN 
                PUT STREAM excel UNFORMATTED
                   '"' v-cus                                         '",'
                   '"' v-itm                                         '",'
                   '"' oe-ordl.po-no                                 '",'
                   '"' v-job                                         '",'
                   '"' oe-ordl.qty                                   '",'
                   '"' oe-ord.ord-date                               '",'
                   '"' ""                                            '",'
                   '"' ""                                            '",'
                   '"' ""                                            '",'
                   '"' ""                                            '",'
                   '"' v-price                                       '",'
                   SKIP.
    
          assign
           v-cus = ""
           v-itm = ""
           v-qoh = 0.
        END.
      END.
    END. /* each cust */
    
    IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
      
    END.
    OUTPUT CLOSE.
    
    /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
  
END PROCEDURE. /* run-report */






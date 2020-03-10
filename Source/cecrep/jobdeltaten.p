/* ----------------------------------------------  */
/*  cecrep/jobdeltaten.p  Corrugated factory ticket  for Xprint landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

DEFINE INPUT PARAMETER v-format AS CHARACTER.

DEFINE SHARED VARIABLE s-prt-mstandard  AS LOG       NO-UNDO.
DEFINE        VARIABLE k_frac           AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE        VARIABLE v-ink-1          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-ink-2          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-ink-3          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-ink-4          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-ink-5          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-ink-6          AS cha       FORM "X(30)" NO-UNDO.
DEFINE        VARIABLE v-dept-note      AS cha       FORM "x(124)" EXTENT 31 NO-UNDO.
DEFINE        VARIABLE v-spec-note      AS cha       FORM "x(124)" EXTENT 10 NO-UNDO.
DEFINE        VARIABLE v-deptnote       AS cha       NO-UNDO.
DEFINE        VARIABLE v-dept-length    AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE lv-under-run     AS cha       NO-UNDO.
DEFINE        VARIABLE lv-over-run      AS cha       NO-UNDO.
DEFINE        VARIABLE iunder-run       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE iover-run        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE lv-part-name     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-fg-name       AS cha       NO-UNDO.
DEFINE        VARIABLE tb_app-unprinted AS LOG       NO-UNDO.
DEFINE        VARIABLE iset-qty         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE lPrintScores     AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE cCustName        AS CHARACTER NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobtickL.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

DEFINE NEW SHARED VARIABLE v-out1-id      AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id      AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */
 
DEFINE            VARIABLE v-vend-no      LIKE oe-ordl.vend-no NO-UNDO.
DEFINE            VARIABLE v-po-no        LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE            VARIABLE v-qty-or-sup   AS CHARACTER FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE v-i-line       AS CHARACTER EXTENT 4 FORMAT "x(38)" NO-UNDO.
DEFINE            VARIABLE v-flag         AS LOG       INIT NO NO-UNDO.
DEFINE            VARIABLE v-local-copies AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-local-loop   AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-print-score  AS LOG       INIT YES NO-UNDO.
DEFINE            VARIABLE v-pqty         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-part-no     AS cha       FORM "x(15)" NO-UNDO.
DEFINE            VARIABLE v-loop-cnt     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-note-cnt     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-note-length  AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-die-loc      AS cha       FORM "x(15)" NO-UNDO.
DEFINE            VARIABLE v-plate-loc    AS CHARACTER FORM "X(8)" NO-UNDO.
DEFINE            VARIABLE cImagePath     AS CHARACTER FORMAT "x(100)" NO-UNDO.

{custom/notesdef.i}
{cecrep/jc-prem.i}
DEFINE BUFFER b-ef FOR ef.
DEFINE WORKFILE tt-wm LIKE w-m.
DEFINE VARIABLE v-xg-flag    AS LOG NO-UNDO.
DEFINE VARIABLE v-tmp-stype  AS cha NO-UNDO.
DEFINE VARIABLE v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEFINE VARIABLE v-tmp-score  AS cha NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.
DEFINE        VARIABLE lv-spec-qty      LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-set-header AS LOG NO-UNDO.
DEFINE        VARIABLE v-dept-inst      AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE        VARIABLE v-inst2          AS cha EXTENT 6 NO-UNDO.    
DEFINE BUFFER b-eb FOR eb.
DEFINE        VARIABLE v-job-cust    AS LOG       NO-UNDO.
DEFINE        VARIABLE ls-fgitem-img AS CHARACTER FORM "x(150)" NO-UNDO.
DEFINE SHARED VARIABLE s-prt-fgimage AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE v-dept-codes  AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE v-dept-log    AS LOG       NO-UNDO.
DEFINE        VARIABLE cBarCodeVal   AS CHARACTER FORM "x(20)" NO-UNDO .
DEFINE        VARIABLE v-shipto      AS cha       NO-UNDO.
DEFINE        VARIABLE dJobQty       AS DECIMAL   NO-UNDO .   
DEFINE        VARIABLE lv-text       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE li            AS INTEGER NO-UNDO.

DEFINE BUFFER bf-itemfg         FOR itemfg .

DEFINE BUFFER b-rt              FOR reftable.
DEFINE BUFFER bf-box-design-hdr FOR box-design-hdr.

DEFINE NEW SHARED WORKFILE wrk-ink
    FIELD i-code AS CHARACTER FORMAT "x(10)"
    FIELD form-no LIKE eb.form-no
    FIELD blank-no LIKE eb.blank-no
    FIELD i-dscr AS CHARACTER FORMAT "x(30)"
    FIELD i-qty AS DECIMAL FORMAT ">,>>9.9<"
    FIELD i-pass AS DECIMAL
    FIELD i-unit AS INTEGER
    FIELD side AS CHARACTER .


DO TRANSACTION:
    {sys/inc/tspostfg.i}
    {custom/formtext.i NEW}
END.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "JOBQTYCUST"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.NAME    = "JOBQTYCUST"
        sys-ctrl.module  = "JC"
        sys-ctrl.descrip = "Create Job Quantity with overrun % from customer if no order?"
        sys-ctrl.log-fld = NO .
END.

v-job-cust = sys-ctrl.log-fld.
s-prt-set-header = NO .

ASSIGN
    v-line[1]      = CHR(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95) + "  " +
             chr(95) + fill(CHR(95),40) + chr(95)  
    v-line[2]      = v-line[1]
    v-line[3]      = CHR(95) + fill(CHR(95),128) + chr(95)
    v-line[4]      = v-line[3]
    v-line[5]      = CHR(95) + fill(CHR(95),84) + chr(95) + "  " +
                chr(95) + fill(CHR(95),40) + chr(95)
    v-line[6]      = v-line[5]
    v-line[7]      = CHR(95) + fill(CHR(95),25) + chr(95) + "  " +
             chr(95) + fill(CHR(95),99) + chr(95)
    v-line[8]      = v-line[7]
    v-qty-or-sup   = IF LOOKUP(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") GT 0
                THEN ("Supplier:"     + fill("_",28))
                ELSE ("Qty Received: " + fill("_",24))
    v-local-copies = 1.

DO v-local-loop = 1 TO v-local-copies:
    {cecrep/jobprem.i}
      break by job.job-no BY job.job-no2 BY job-hdr.frm:

v-break = FIRST-OF(job.job-no2).

RELEASE xest.
RELEASE xef.
RELEASE xeb.
RELEASE xoe-ord.
RELEASE xoe-ordl.
RELEASE xoe-rel.

RUN cecrep/jobtick1.p (RECID(job-hdr), v-format,
    v-local-loop, v-local-copies).

FOR EACH w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
    EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
    AND b-eb.est-no = job-hdr.est-no 
    AND b-eb.form-no = w-ef.frm
    AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
    BREAK BY w-ef.frm BY b-eb.blank-no:
    RELEASE xef.
    RELEASE xeb.
    RELEASE xstyle.
    RELEASE xxprep.
        
    /* RUN cecrep/jobtick2csc.p (RECID(w-ef), RECID(job-hdr), ROWID(b-eb)).*/
    RUN cecrep/jobtickdelta.p (RECID(w-ef), RECID(job-hdr),RECID(b-eb)).

    ASSIGN
        v-pqty = 1
        v-cp   = "".
    IF AVAILABLE xeb THEN 
    DO:
        IF xeb.stock-no NE "" THEN v-fg = xeb.stock-no.
        ASSIGN
            v-cp       = xeb.part-no
            lv-fg-name = itemfg.i-name.

        {cec/rollfac.i}
        v-pqty = IF v-rollfac OR xeb.est-type EQ 8 THEN 1 ELSE
            IF xeb.quantityPerSet LT 0 THEN (-1 / xeb.quantityPerSet)
            ELSE xeb.quantityPerSet.
    END.
        
    ASSIGN
        v-loc     = ""
        v-loc-bin = "".
         
    IF v-format EQ "Brick" OR v-format EQ "Corrugat" THEN 
    DO: 
        v-iso = "ISO# CS-05-1-F".

        RELEASE fg-rdtlh.
          
        FIND FIRST fg-bin
            WHERE fg-bin.company   EQ cocode
            AND fg-bin.i-no      EQ job-hdr.i-no
            AND fg-bin.job-no    EQ job-hdr.job-no
            AND fg-bin.job-no2   EQ job-hdr.job-no2
            AND fg-bin.loc       EQ job-hdr.loc
            AND fg-bin.qty       NE 0
            NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin THEN 
        DO:
            ASSIGN
                v-loc     = "Whs: " + fg-bin.loc
                v-loc-bin = "Bin: " + fg-bin.loc-bin.
        END.
        ELSE
            IF AVAILABLE itemfg THEN 
            DO:                             
                ASSIGN
                    v-loc     = "Whs: " + itemfg.def-loc
                    v-loc-bin = "Bin: " + itemfg.def-loc-bin.
            END.

    END. /*brick format*/

    ASSIGN 
        lv-over-run  = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) ELSE
                             IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%"))  ELSE ""
        lv-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) ELSE
                              IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%"))  ELSE ""
        lv-part-name = xeb.part-dscr1    
        iunder-run   = IF AVAILABLE xoe-ordl THEN DECIMAL(xoe-ordl.under-pct) ELSE
                             IF AVAILABLE xoe-ord  THEN DECIMAL(xoe-ord.under-pct)  ELSE 0
        iover-run    = IF AVAILABLE xoe-ordl THEN DECIMAL(xoe-ordl.over-pct) ELSE
                             IF AVAILABLE xoe-ord  THEN DECIMAL(xoe-ord.over-pct)  ELSE 0                                 .
    IF v-job-cust AND NOT AVAILABLE xoe-ord AND AVAILABLE cust THEN
        ASSIGN
            lv-over-run  = TRIM(STRING(cust.over-pct,">>9.99%"))
            lv-under-run = TRIM(STRING(cust.under-pct,">>9.99%")) 
            iunder-run   = DECIMAL(cust.under-pct)
            iover-run    = DECIMAL(cust.over-pct)
            .
    ASSIGN 
        cBarCodeVal = trim(job-hdr.job-no) /* + "-" + STRING(job-hdr.job-no2,"99") */ . 
    ASSIGN  
        cCustName = cust.NAME 
        {sys/inc/ctrtext.i cCustName 30}.
      
    PUT "<C1><R1.2><#Start>"
        "<=Start><FROM><C108><R50><RECT><|1>"
        "<=Start><#JobStart>"
        "<=JobStart><C+20><#JobTR>"
        "<=JobStart><R+3><#JobBL> "
        "<=JobStart><C+20><R+3><#JobEnd>"
        "<=JobStart><FROM><RECT#JobEnd><|1>"
        "<=JobTR><#HeaderStart>"
        "<=HeaderStart><C+58><#HeaderTR>"
        "<=HeaderStart><R+3><#HeaderBL>"
        "<=HeaderStart><C+58><R+3><#HeaderEnd>"
        "<=HeaderStart><FROM><RECT#HeaderEnd><|1>"
        "<=HeaderTR><#BarCodeStart>"
        "<=BarCodeStart><C108><R+3><#BarCodeEnd>"
        "<=BarCodeStart><FROM><RECT#BarCodeEnd><|1>"
        "<=JobStart><C+.5><R+0.6><#JobLabel>"
        "<=JobLabel><C+7><#JobNum>"
        "<=HeaderStart><C+1><R+0.6><#FormLabel>"
        "<=HeaderStart><C+1><R+1.6><#BlankLabel>"
        "<=FormLabel><C+4><#Form>"
        "<=BlankLabel><C+4><#Blank>"
        "<=HeaderStart><C+10><R+0.6><#PartLabel>"
        "<=PartLabel><R+1><#Part>"
        "<=HeaderStart><C+22><R+0.6><#CustomerName>"           
        "<=BarCodeStart><C+2><R+.3><FROM><C108><R3.9><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" + cBarCodeVal + ">" FORMAT "x(150)"
        "<P14>                  "
        "<=JobLabel>Job #:"
        "<FGColor=Blue><B>   "
        "<=JobNum>" job-hdr.job-no + "-" + string(job-hdr.job-no2,"99") FORMAT "x(10)"
        "</B><FGColor=Black>"
        "<P8> "
        "<=BlankLabel>Blank:"
        "<=FormLabel>Form: "
        "<=PartLabel>Customer Part #"
        "<P14>"
        "<FGColor=Blue>"
        "<=CustomerName><B>" cCustName FORMAT "x(30)" "</B>"
        "<FGColor=Black>"
        "<P8> "
        "<=Form><B>" IF AVAILABLE xeb THEN STRING(xeb.form-no,"99") ELSE "" FORMAT "x(2)" "</B>  "
        "<=Blank><B>" IF AVAILABLE xeb THEN STRING(xeb.BLANK-no,"99") ELSE ""  FORMAT "x(2)" "</B>"
        "<=Part><B><P10>" v-cp FORMAT "x(15)" "<P8></B>" "<R+0.5><C+10>Cust#: <B>" xeb.cust-no FORMAT "x(10)" "</B>" 
        .

    PUT 
        "<=Start><R+3><#PageStart>"
        "<=PageStart><#QuantityStart>"
        "<=QuantityStart><C+25><#QuantityTR>"
        "<=QuantityStart><R+8><#QuantityBL>"
        "<=QuantityStart><C+25><R+8><#QuantityEnd>"
        "<=QuantityTR><FROM><LINE#QuantityEnd>"
        "<=QuantityBL><#BoardStart>"
        "<=BoardStart><C+32><#BoardTR>"
        "<=BoardStart><R+11><#BoardBL>"
        "<=BoardStart><C+32><R+11><#BoardEnd>"
        "<=BoardTR><FROM><LINE#BoardEnd><|1>"
        "<=BoardBL><#BoxImageStart>"
        "<=BoardTR><#DieStart>"
        "<=DieStart><C+29><#DieTR>"
        "<=DieStart><R+9><#DieBL>"
        "<=DieStart><C+29><R+9><#DieEnd>"
            
        "<=RoutingStart><C+61><#RoutingTR>"
        "<=RoutingStart><R+9><#RoutingBL>"
        "<=RoutingStart><C+61><R+9><#RoutingEnd>"
        "<=BoxImageStart><C+61><#BoxImageTR>"
        "<=BoxImageStart><R+29><#BoxImageBL>"
        "<=BoxImageStart><C+61><R+28><#BoxImageEnd>"
        .
         
                
    PUT "<=BoxImageStart><FROM><LINE#BoxImageTR><|1>"
        "<=DieTR><FROM><LINE#BoxImageEnd>"
        "<=QuantityTR><#ItemStart>"
        "<=ItemStart><C+44><#ItemTR>"
        "<=ItemStart><R+8><#ItemBL>"
        "<=ItemStart><C+44><R+8><#ItemEnd>"
        "<=ItemTR><FROM><LINE#ItemEnd><|1>"
        "<=ItemStart><R+1><RIGHT=C+5>FG ID: <#FGItemID><RIGHT=C+27>Estimate: <#Estimate>"
        "<=ItemStart><R+2><RIGHT=C+5>Name: <#FGItemName>"
        "<=ItemStart><R+3><RIGHT=C+5>Desc1: <#FGItemDesc1>"
        "<=ItemStart><R+4><RIGHT=C+5>Desc2: <#FGItemDesc2>"
        "<=ItemStart><R+5><RIGHT=C+5>Style: <#Style><RIGHT=C+18>Tab: <#TabInOut>"
        "<=ItemStart><R+6><RIGHT=C+5>Size: <#Size><RIGHT=C+18>CAD#: <#CAD>"
        "<=ItemTR><#OrderStart>"
        "<=OrderStart><C108><#OrderTR>"
        "<=OrderStart><R+8><#OrderBL>"
        "<=OrderStart><C108><R+8><#OrderEnd>"
        "<=OrderStart><R+0.5><RIGHT=C+14>Job Card printed: <#Printed>"
        "<=OrderStart><R+2><RIGHT=C+14>Our Order #: <#OrderNum><C+12>CSR:<#OrderCsr>"
        "<=OrderStart><R+3><RIGHT=C+14>Customer PO: <#CustomerPO>"
        "<=OrderStart><R+4><RIGHT=C+14>Order Quantity: <#OrderQuantity>"
        "<=OrderStart><R+5><RIGHT=C+14>Order Date: <#OrderDate>"
        "<=OrderStart><R+6><RIGHT=C+14>Due Date: <#OrderDueDate>"
        "<=QuantityStart><FROM><RECT#OrderEnd><|1>"
        "<=QuantityStart><R+1><RIGHT=C+12>Job Quantity: <#JobQuantity>"
        "<=QuantityStart><R+2><RIGHT=C+12>Overrun: <#Overrun> "
        "<=QuantityStart><R+2><C+19><#OverrunPct>"
        "<=QuantityStart><R+3><RIGHT=C+12>Underrun: <#Underrun>"
        "<=QuantityStart><R+3><C+19><#UnderrunPct>"
        "<=QuantityStart><R+5><RIGHT=C+12>Set Quantity: <#SetQuantity> "
        "<=QuantityStart><R+6><RIGHT=C+12>Parts Per Set: <#QtyPerSet>"
        "<=BoardStart><R+1><RIGHT=C+6>Board: <#Board>"
        "<=BoardStart><R+2><RIGHT=C+6>Sheets: <#SheetsRequired>"
        /*              "<=BoardStart><R+2><RIGHT=C+20>Received: <#SheetsReceived>"*/
        "<=BoardStart><R+3><RIGHT=C+6>Size: <#SheetsSize>"
        "<=BoardStart><R+3><RIGHT=C+24>Tot Sq Ft: <#SheetsMSF>"
        "<=BoardStart><R+4><RIGHT=C+9>Wid Scores: <#Scores>"
        "<=BoardStart><R+5><RIGHT=C+9> <#Scores2>"
        "<=BoardStart><R+6><RIGHT=C+9>Len Scores: <#ScoresLen>"
        "<=BoardStart><R+7><RIGHT=C+9> <#ScoresLen2>"
        "<=BoardStart><R+8><RIGHT=C+6>Adders:"
        "<=BoardStart><R+8><C7.5><#Adders1>"
        "<=BoardStart><R+9><C7.5><#Adders2>"
        "<=BoardStart><R+10><RIGHT=C+5>PO: <#VendorPO>"
        "<=BoardStart><R+10><RIGHT=C+21>Vendor: <#VendorCode>"
        "<=DieStart><R+1><RIGHT=C+8>Die#: <#Die>"
        "<=DieStart><R+2><RIGHT=C+8>Die Loc.: <#DieLocation>"
        "<=DieStart><R+3><RIGHT=C+8>Joint: <#JointImpressions>"
        "<=DieStart><R+4><RIGHT=C+6>Gross: "
        "<=DieStart><R+4><RIGHT=C+7>W:<#GrossWidth> "
        "<=DieStart><R+4><RIGHT=C+13> L:<#GrossLength>"
        "<=DieStart><R+4><RIGHT=C+21>Out: "
        "<=DieStart><R+4><RIGHT=C+23>W:<#OutW>"
        "<=DieStart><R+4><RIGHT=C+26>L:<#OutL>"
        "<=DieStart><R+5><RIGHT=C+6>Net: "
        "<=DieStart><R+5><RIGHT=C+7>W:<#NetWidth> "
        "<=DieStart><R+5><RIGHT=C+13> L:<#NetLength>"
        "<=DieStart><R+6><RIGHT=C+6>Die: "
        "<=DieStart><R+6><RIGHT=C+7>W:<#DieWidth> "
        "<=DieStart><R+6><RIGHT=C+13> L:<#DieLength>"
        "<=DieStart><R+6><RIGHT=C+21>Up: "
        "<=DieStart><R+6><RIGHT=C+23>W:<#UpW>"
        "<=DieStart><R+6><RIGHT=C+26>L:<#UpL>"
        "<=DieStart><R+7><RIGHT=C+6>Blank: "
        "<=DieStart><R+7><RIGHT=C+7>W:<#BlankWidth> "
        "<=DieStart><R+7><RIGHT=C+13> L:<#BlankLength>"
        "<=DieStart><R+7><RIGHT=C+21> Sq Ft:<#SqFeet>"
        "<=DieTR><#PrintStart>"
        "<=PrintStart><R+1><RIGHT=C+14>Plate#: <#Plate>"
        "<=PrintStart><R+2><RIGHT=C+14>Plate Loc.: <#PlateLocation>"
        "<=PrintStart><R+3><RIGHT=C+14>Color Desc/Cert.: <#InkDescription>"
        . 

    IF v-format EQ "RFC" OR v-format EQ "Boxtech" THEN
        ASSIGN
            v-i-line[1] = itemfg.i-name
            v-i-line[2] = itemfg.part-dscr1
            v-i-line[3] = itemfg.part-dscr2
            v-i-line[4] = itemfg.part-dscr3.
    ELSE
        ASSIGN
            v-i-line[1] = "ITEM DESCRIPTION"
            v-i-line[2] = "Style: " + IF AVAILABLE xstyle THEN xstyle.dscr ELSE ""
            v-i-line[3] = "Size: "  + IF AVAILABLE xeb    THEN
                     TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")) ELSE ""
            v-i-line[4] = "Joint: " + IF AVAILABLE xeb THEN v-joint-dscr ELSE "".
       
    lv-part-no = IF AVAILABLE xoe-ordl THEN xoe-ordl.part-no 
    ELSE itemfg.part-no.
        
    RUN sys/ref/getpo#.p (IF AVAILABLE xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
        w-ef.frm, OUTPUT v-po-no).

    RELEASE po-ord.
          
    IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
            AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
          
    ASSIGN
        v-vend-no    = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""
        v-qty-or-sup = "Supplier: ".

    IF v-vend-no NE "" THEN 
    DO:
        v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
        IF v-po-no NE 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                trim(STRING(v-po-no,">>>>>>>>>>")).
    END.
        
        
    v-form-sqft = ROUND(IF v-corr THEN (v-form-len * v-form-wid * .007)
    ELSE (v-form-len * v-form-wid / 144),3).
    FIND FIRST xxprep WHERE xxprep.company EQ cocode
        AND xxprep.code EQ xeb.die-no
        NO-LOCK NO-ERROR.
    v-die-loc = IF AVAILABLE xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

    FIND FIRST xxprep WHERE xxprep.company EQ cocode
        AND xxprep.code EQ xeb.plate-no
        NO-LOCK NO-ERROR.

    v-plate-loc = IF AVAILABLE xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "" .

    IF AVAILABLE xeb THEN
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ xeb.company
            AND bf-itemfg.i-no EQ xeb.stock-no NO-ERROR .

    iset-qty = IF AVAILABLE xeb AND xeb.est-type EQ 6 THEN
        IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty
        ELSE 0 .
    dJobQty  = job-hdr.qty * (IF xeb.est-type EQ 6 AND xeb.quantityPerSet GT 0 THEN xeb.quantityPerSet ELSE 1) .
    IF AVAILABLE xef THEN
        ASSIGN v-form-wid = xef.nsh-wid 
            v-form-len = xef.nsh-len .
          
    PUT "<FGColor=Blue><B>"
        "<=JobQuantity><P10>" dJobQty FORMAT "->>,>>>,>>9"
        "<P8></B><FGColor=Black>"
        "<=Overrun>" STRING( (job-hdr.qty * iover-run) / 100,"->>>>>>9") /*FORMAT "->>,>>>,>>9"*/
        "<=Underrun>" STRING( (job-hdr.qty * iunder-run) / 100,"->>>>>>9")  /*FORMAT "->>,>>>,>>9"*/
        "<=OverrunPct>" lv-over-run /*FORMAT "99.9%"*/
        "<=UnderrunPct>" lv-under-run /*FORMAT "99.9%"*/
        "<=SetQuantity>" iset-qty 
        "<=QtyPerSet>" IF AVAILABLE xeb THEN xeb.quantityPerSet ELSE  0
        "<=FGItemID>" IF AVAILABLE xeb THEN xeb.stock ELSE job-hdr.i-no FORMAT "x(15)" 
        "<FGColor=Blue><B><P10>"
        "<=FGItemName>" IF AVAILABLE xeb THEN  xeb.part-dscr1 ELSE "" FORMAT "x(30)" 
        "<P8></B><FGColor=Black>"
        "<=FGItemDesc1>" IF AVAILABLE xeb THEN  xeb.part-dscr2 ELSE "" FORMAT "x(30)" 
        "<=FGItemDesc2>" IF AVAILABLE xeb AND AVAILABLE bf-itemfg THEN  bf-itemfg.part-dscr2 ELSE "" FORMAT "x(30)" 
        "<B>"
        "<=Style><P10>" IF AVAILABLE xstyle THEN xstyle.style ELSE "" FORMAT "x(15)"
        "<P8><=TabInOut>" IF AVAILABLE xeb AND xeb.tab-in EQ YES THEN "In" ELSE IF AVAILABLE xeb AND xeb.tab-in EQ NO THEN "Out" ELSE "" FORMAT "x(10)"
        "<=Estimate>" IF AVAILABLE xeb THEN xeb.est-no ELSE "" FORMAT "x(10)"
        "</B>"
        "<=Size>" IF AVAILABLE xeb THEN (TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
        trim(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
        trim(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99"))) ELSE ""  FORM "x(30)" 
        "<=CAD><P10><B>" IF AVAILABLE xeb THEN xeb.cad-no ELSE "" FORMAT "x(15)"
        "</B><P8><=Printed><B>" TODAY  "</B>"
        "<=OrderNum>" IF AVAILABLE xoe-ord THEN STRING(xoe-ord.ord-no) ELSE "" 
        "<=OrderCsr>" SPACE(1) IF AVAILABLE xoe-ord THEN STRING(xoe-ord.csrUser_id) ELSE job.csrUser_id FORMAT "x(10)"
        "<=CustomerPO>" IF AVAILABLE xoe-ordl AND xoe-ord.po-no NE "" THEN xoe-ordl.po-no ELSE IF AVAILABLE xoe-ord THEN xoe-ord.po-no ELSE "" FORMAT "x(15)"
        "<FGColor=Blue><B>"
        /* "<=CustomerName>" cust.NAME FORMAT "x(30)"*/
        "</B><FGColor=Black>"
        "<=OrderQuantity>"  IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE 0 
        "<=OrderDate>" IF AVAILABLE xoe-ord THEN STRING(xoe-ord.ord-date) ELSE "" 
        "<FGColor=Blue><B>"
        "<=OrderDueDate>"   IF AVAILABLE xoe-ord THEN STRING(xoe-ord.due-code) + "  " + STRING(xoe-ord.due-date) ELSE "" FORMAT "x(15)"
        "</B><FGColor=Black>"
        "<B>"
        "<=Board><P10>" v-board-dscr FORMAT "x(20)" 
        "<P8></B>"
        "<=SheetsRequired>" TRIM(STRING(v-sht-qty))   FORMAT "x(9)"
        /*              "<=SheetsReceived>"*/
        "<=SheetsSize>" "W:" + trim(STRING({sys/inc/k16v.i v-form-wid},">>,>>9.99")) + "  " +
        "L:" + trim(STRING({sys/inc/k16v.i v-form-len},">>,>>9.99"))  FORMAT "x(30)"

        "<=SheetsMSF>" TRIM(STRING(v-sht-qty * v-form-sqft ,">>,>>>,>>9.9<")) FORMAT "x(12)"
        "<=Scores><P9>" SUBSTRING(v-wid-score,1,34) FORMAT "x(35)" 
        "<=Scores2><P9>" SPACE (19) SUBSTRING(v-wid-score,35,34) FORMAT "x(35)"
        "<=ScoresLen>" SUBSTRING(v-len-score,1,34) FORMAT "x(35)" 
        "<=ScoresLen2>" SPACE (19) SUBSTRING(v-len-score,35,34) FORMAT "x(35)"
        "<P8><=Adders1>" xef.adder[7]  FORMAT "x(20)"
        "<=Adders2>"  xef.adder[8]  FORMAT "x(20)"
        "<=VendorPO>" STRING(v-po-no)  FORMAT "x(10)" 
        "<=VendorCode>" STRING(v-vend-no ) FORMAT "x(15)"
        "<B>"
        "<=Die>" IF AVAILABLE xeb THEN xeb.die-no ELSE "" FORMAT "X(15)"
        "</B>"
        "<=DieLocation>" v-die-loc FORMAT "x(10)"
        "<=JointImpressions><P10>" (if avail xeb then v-joint-dscr else "")    FORMAT "x(25)"
        "<P8><=GrossWidth>" TRIM(STRING({sys/inc/k16v.i xef.gsh-wid},">>>>9.99")) FORMAT "x(8)"
        "<=GrossLength>" TRIM(STRING({sys/inc/k16v.i xef.gsh-len},">>>>9.99")) FORMAT "x(8)"
        "<=OutL>" STRING(xef.n-out-l) FORMAT "x(3)"
        "<=OutW>" STRING(xef.n-out)   FORMAT "x(3)"
        "<=NetWidth>" TRIM(STRING({sys/inc/k16v.i xef.nsh-wid},">>>>9.99")) FORMAT "x(8)"
        "<=NetLength>" TRIM(STRING({sys/inc/k16v.i xef.nsh-len},">>>>9.99")) FORMAT "x(8)"
        "<=DieWidth>" TRIM(STRING({sys/inc/k16v.i xef.trim-w},">>>>9.99")) FORMAT "x(8)"
        "<=DieLength>" TRIM(STRING({sys/inc/k16v.i xef.trim-l},">>>>9.99")) FORMAT "x(8)"
        "<=UpL>" STRING(v-upw) FORMAT "x(8)"
        "<=UpW>" STRING(v-upl) FORMAT "x(8)"
        "<=BlankWidth>" TRIM(STRING({sys/inc/k16v.i xeb.t-wid},">>>>9.99")) FORMAT "x(8)"
        "<=BlankLength>" TRIM(STRING({sys/inc/k16v.i xeb.t-len},">>>>9.99")) FORMAT "x(8)"
        "<=SqFeet>" TRIM( IF v-corr THEN STRING(xeb.t-sqin * .007,">>>>>>9.9999")
        ELSE STRING(xeb.t-sqin / 144,">>>>>>9.9999")) FORMAT "x(12)"
                                  
        "<B>"
        "<=Plate>" IF AVAILABLE xeb THEN xeb.plate-no ELSE "" FORMAT "x(15)" 
        "</B>"
        "<=PlateLocation>" v-plate-loc FORMAT "x(20)" 
        "<=InkDescription>" IF AVAILABLE xeb THEN xeb.i-coldscr ELSE "" FORMAT "x(30)"
        .

           
    FOR EACH wrk-ink:
        DELETE wrk-ink.
    END.

    /** BUILD INK WORK FILE **/
    FOR EACH job-mat WHERE job-mat.company EQ cocode
        AND job-mat.job     EQ job-hdr.job
        AND job-mat.frm     EQ xeb.form-no
        NO-LOCK,
        FIRST ITEM NO-LOCK 
        {sys/look/itemivW.i}
                       and item.i-no eq job-mat.i-no:
                       
    DO i = 1 TO 10:
                        
        IF xeb.i-code[i] EQ job-mat.i-no THEN 
        DO:
                             
            FIND FIRST wrk-ink
                WHERE wrk-ink.i-code   EQ xeb.i-code[i]
                AND wrk-ink.form-no  EQ xeb.form-no
                AND wrk-ink.blank-no EQ xeb.blank-no
                AND wrk-ink.i-pass   EQ xeb.i-ps[i]
                                   
                NO-ERROR.
            IF NOT AVAILABLE wrk-ink THEN 
            DO:
                CREATE wrk-ink.
                ASSIGN 
                    wrk-ink.i-code   = xeb.i-code[i]
                    wrk-ink.form-no  = xeb.form-no
                    wrk-ink.blank-no = xeb.blank-no
                    wrk-ink.i-dscr   = xeb.i-dscr[i]
                    wrk-ink.i-pass   = xeb.i-ps[i]
                    .
            END.
        END.
    END. /* loop i */
                  
    FIND FIRST wrk-ink
        WHERE wrk-ink.i-code    EQ job-mat.i-no
        AND wrk-ink.form-no   EQ job-mat.frm
        AND (wrk-ink.blank-no EQ job-mat.blank-no OR
        est.est-type     EQ 4)
        NO-ERROR.

    IF NOT AVAILABLE wrk-ink AND
        (job-mat.blank-no  EQ xeb.blank-no OR
        (job-mat.blank-no EQ 0 AND xeb.blank-no EQ 1)) THEN 
    DO:
        CREATE wrk-ink.
        ASSIGN
            wrk-ink.i-code   = job-mat.i-no
            wrk-ink.form-no  = xeb.form-no
            wrk-ink.blank-no = xeb.blank-no
            wrk-ink.i-dscr   = item.est-dscr
            wrk-ink.i-unit   = 0 
            wrk-ink.i-pass   = 1.
    END.

    IF AVAILABLE wrk-ink THEN 
        wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
END. /* JOB-MAT */

            
j = 0 . 
FOR EACH wrk-ink NO-LOCK:
    j = j + 1   .
    v-ink-1 =  STRING(wrk-ink.i-dscr,"x(28)") .
    v-ink-2 = (IF wrk-ink.i-qty <> 0 THEN STRING(wrk-ink.i-qty,">>>,>>9.99") ELSE "" ) +
        (IF wrk-ink.i-dscr <> "" THEN "  LBS" ELSE "") .
    v-ink-3 = "Pass: " +  STRING(wrk-ink.i-pass) + " " + STRING("F") .
   
    PUT "<=PrintStart><R+" + string(3 + j) + "><RIGHT=C+10>Ink" + STRING( j) + ":<#Ink" + string(1 + j) +  "Name>" FORMAT "x(200)" 
        "<=PrintStart><R+" + string(3 + j) + "><C+28><#lbs" + string(1 + j) +  "Name>" FORMAT "x(200)"
        "<=PrintStart><R+" + string(3 + j) + "><C+37><#Pass" + string(1 + j) +  "Name>" FORMAT "x(200)".

    PUT "<=Ink" + string(1 + j) + "Name>" + v-ink-1 FORMAT "x(150)" 
        "<=lbs" + string(1 + j) + "Name>" + v-ink-2 FORMAT "x(150)"
        "<=Pass" + string(1 + j) + "Name>" + v-ink-3 FORMAT "x(150)" .
                   
END.

PUT "<=PrintStart><R+" + STRING( 4 + j) + "><#RoutingStart>" FORMAT "x(100)" .

        
i = 0.
FOR EACH w-m:
    i = i + 1.
END.
IF i LT 3 THEN 
DO i = i + 1 TO 3:
    CREATE w-m.
    w-m.dseq = 999999999.
END.
/* box for route */
PUT 
    "<=RoutingStart><FROM><C108><LINE><|1>"
    "<B><=RoutingStart><R+1><C+1>Routing"
    "<=RoutingStart><R+1><C+30>MR"
    "<=RoutingStart><R+1><C+35>Run"
    "<=RoutingStart><R+1><C+40>Speed"
    "</B>" SKIP .
       
i = 0.

FOR EACH w-m BREAK BY w-m.dseq:
    IF w-m.dscr NE "" THEN 
    DO:
        ASSIGN
            i        = i + 1
            v-letter = substr("UTE",i,1).
          
        PUT 
            "<=RoutingStart><R+" + STRING( 1 + i) + "><C+1><#Mach" + STRING(1 + i) + ">"  FORMAT "x(90)"
            "<=RoutingStart><R+" + STRING(1 + i) + "><C+30><#MRHours" + STRING(1 + i ) + ">" FORMAT "x(90)"
            "<=RoutingStart><R+" + STRING(1 + i) + "><C+35><#RunHours" + STRING(1 + i) + ">"  FORMAT "x(90)"
            "<=RoutingStart><R+" + STRING(1 + i) + "><C+40><#Speed" +  STRING(1 + i) + ">"    FORMAT "x(90)"
    
            "<=Mach" + STRING(1 + i) + ">" +  w-m.dscr FORMAT "x(60)" .
        IF s-prt-mstandard THEN
            PUT
                "<=MRHours" + STRING(1 + i) + ">" +  string(w-m.s-hr) FORMAT "x(200)"
                "<=RunHours" + STRING(1 + i) + ">" +  string(w-m.r-hr) FORMAT "x(200)"
                "<=Speed" + STRING(1 + i) + ">" + string(w-m.r-sp) FORMAT "x(200)" .
           
    END.
    PUT  "<=RoutingStart><R+" + string(2 + i) + "><#NotesStart>"  FORMAT "x(100)" .
          
                  
    v-lines = v-lines + 1.
END.
      
FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
    AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
IF AVAILABLE b-ef AND b-ef.form-no = w-ef.frm THEN 
    FOR EACH w-m:
        CREATE tt-wm.
        BUFFER-COPY w-m TO tt-wm.
    END.
      
/* dept notes */
ASSIGN
    v-note-length   = 82
    v-tmp-lines     = 0
    j               = 0
    K               = 0
    lv-got-return   = 0
    v-dept-note     = "" 
    v-prev-note-rec = ?.

  lv-text = "".   
  FOR EACH notes WHERE notes.rec_key = job.rec_key and
    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0) AND
    ((v-dept-log AND lookup(notes.note_code,v-dept-codes) NE 0) OR NOT v-dept-log)  ,
    FIRST dept WHERE dept.code = notes.note_code NO-LOCK 
    BY notes.note_code:

    lv-text = lv-text + "<B>" + caps(dept.dscr) + ":</B>" + CHR(10) + 
        "  " + notes.note_text + CHR(10).          
  END.
  DO li = 1 TO 30:
      CREATE tt-formtext.
      ASSIGN tt-line-no = li
          tt-length  = 78. 
  END.
  RUN custom/formtext.p (lv-text).
  ASSIGN
      i = 0
      v-inst2 = ""
      v-dept-note = "".
  
  FOR EACH tt-formtext:
      i = i + 1.
      IF  i <= 30 THEN v-dept-note[i] = tt-formtext.tt-text.      
  END.
   
        
ASSIGN
    v-inst        = ""
    v-spec-note   = ""
    v-tmp-lines   = 0
    j             = 0
    K             = 0
    lv-got-return = 0.

FOR EACH notes WHERE notes.rec_key = bf-itemfg.rec_key 
    AND lookup(notes.note_code,spec-list) NE 0 NO-LOCK.
         
    DO i = 1 TO LENGTH(notes.note_text) :        
        IF i - j >= v-note-length THEN ASSIGN j             = i
                lv-got-return = lv-got-return + 1.
                   
        v-tmp-lines = ( i - j ) / v-note-length.
        {SYS/INC/ROUNDUP.I v-tmp-lines}

        k = v-tmp-lines + lv-got-return.
        IF k < 9 THEN v-spec-note[k] = v-spec-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
            ELSE "" .              
           
        IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
            THEN 
        DO:
            lv-got-return = lv-got-return + 1.
            j = i.
        END.         
    END.
END.
    

PUT  "<=NotesStart><FROM><C108><LINE><|1>"
    "<=NotesStart><C+16><R+1><B><P11>Production Notes:<P8></B><#Notes>"
    "<=NotesStart><C+1><R+2><#Notes1>"
    "<=NotesStart><C+1><R+3><#Notes2>"
    "<=NotesStart><C+1><R+4><#Notes3>"
    "<=NotesStart><C+1><R+5><#Notes4>"
    "<=NotesStart><C+1><R+6><#Notes5>"
    "<=NotesStart><C+1><R+7><#Notes6>"
    "<=NotesStart><C+1><R+8><#Notes7>"
    "<=NotesStart><C+1><R+9><#Notes8>"
    "<=NotesStart><C+1><R+10><#Notes9>"
    "<=NotesStart><C+1><R+11><#Notes10>"
    "<=NotesStart><C+1><R+12><#Notes11>"
    "<=NotesStart><C+1><R+13><#Notes12>"
    "<=NotesStart><C+1><R+14><#Notes13>"
    "<=NotesStart><C+1><R+15><#Notes14>"
    "<=NotesStart><C+1><R+16><#Notes15>"
    "<=NotesStart><C+1><R+17><#Notes16>"
    "<=NotesStart><C+1><R+18><#Notes17>"
    "<=NotesStart><C+1><R+19><#Notes18>"
    "<=NotesStart><C+1><R+20><#Notes19>"
             
             
    /*"<=NotesStart><C+1><R+21><#SpecNotes1>"
    "<=NotesStart><C+1><R+22><#SpecNotes2>"
    "<=NotesStart><C+1><R+23><#SpecNotes3>"
    "<=NotesStart><C+1><R+24><#SpecNotes4>"
    "<=NotesStart><C+1><R+25><#SpecNotes5>"
    "<=NotesStart><C+1><R+26><#SpecNotes6>"*/
             
    "<P8><=Notes1>" v-dept-note[1] FORMAT "x(100)" SKIP
    "<=Notes2>" v-dept-note[2] FORMAT "x(100)" SKIP
    "<=Notes3>" v-dept-note[3] FORMAT "x(100)"  SKIP 
    "<=Notes4>" v-dept-note[4] FORMAT "x(100)" SKIP
    "<=Notes5>" v-dept-note[5] FORMAT "x(100)" SKIP
    "<=Notes6>" v-dept-note[6] FORMAT "x(100)"  SKIP

    "<=Notes7>" v-dept-note[7] FORMAT "x(100)"  SKIP
    "<=Notes8>" v-dept-note[8] FORMAT "x(100)"  SKIP
    "<=Notes9>" v-dept-note[9] FORMAT "x(100)"  SKIP
    "<=Notes10>" v-dept-note[10] FORMAT "x(100)"  SKIP
    "<=Notes11>" v-dept-note[11] FORMAT "x(100)"  SKIP
    "<=Notes12>" v-dept-note[12] FORMAT "x(100)"  SKIP
    "<=Notes13>" v-dept-note[13] FORMAT "x(100)"  SKIP
    "<=Notes14>" v-dept-note[14] FORMAT "x(100)"  SKIP
    "<=Notes15>" v-dept-note[15] FORMAT "x(100)"  SKIP
    "<=Notes16>" v-dept-note[16] FORMAT "x(100)"  SKIP
    "<=Notes17>" v-dept-note[17] FORMAT "x(100)"  SKIP
    "<=Notes18>" v-dept-note[18] FORMAT "x(100)"  SKIP
    "<=Notes19>" v-dept-note[19] FORMAT "x(100)"  SKIP
              
    /*"<=SpecNotes1>" v-spec-note[1] FORMAT "x(100)" SKIP
    "<=SpecNotes2>" v-spec-note[2] FORMAT "x(100)" SKIP
    "<=SpecNotes3>" v-spec-note[3] FORMAT "x(100)"  SKIP
    "<=SpecNotes1>" v-spec-note[4] FORMAT "x(100)" SKIP
    "<=SpecNotes2>" v-spec-note[5] FORMAT "x(100)" SKIP
    "<=SpecNotes3>" v-spec-note[6] FORMAT "x(100)"  SKIP*/
    .
v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id 
ELSE IF AVAILABLE xeb THEN xeb.ship-id
ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id 
ELSE "".

FIND FIRST shipto NO-LOCK
    WHERE shipto.company EQ cocode
    AND shipto.cust-no EQ job-hdr.cust-no
    AND shipto.ship-id EQ v-shipto NO-ERROR .

IF AVAILABLE xeb  THEN
    FIND FIRST stackPattern NO-LOCK 
        WHERE stackPattern.stackcode EQ xeb.stack-code NO-ERROR .


PUT      "<=Start><R23.5><#PageStart2>"
    "<=PageStart2><#PackingStart>"
    "<=PackingStart><C+25><#PackingTR>"
    "<=PackingStart><R+10><#PackingBL>"
    "<=PackingStart><C+25><R+10><#PackingEnd>"
    "<=PackingTR><FROM><LINE#PackingEnd>"
    "<=PackingStart><R+1><RIGHT=C+8>Pallet: <#Pallet>"
    "<=PackingStart><R+2><RIGHT=C+8># Per Bndl: <#PerBundle>"
    /*"<=PackingStart><R+2><C10>L:<#PalletLength>"
    "<=PackingStart><R+2><C17>W:<#PalletWidth> "*/
    /*"<=PackingStart><R+3><C11>Per"*/
    /*"<=PackingStart><R+3><C17>Job Total"*/
    /*"<=PackingStart><R+4><RIGHT=C+8>Per Case:"*/
    /*"<=PackingStart><R+4><C11><#CaseCount>"*/
    /*"<=PackingStart><R+4><C17><#JobCases>"*/
    /*"<=PackingStart><R+5><RIGHT=C+8>Per Pallet:"*/
    /*"<=PackingStart><R+5><C11><#PalletCount>"*/
    /*"<=PackingStart><R+5><C17><#JobPallets>"*/
    /*"<=PackingStart><FROM><RECT#ShippingEnd><|1>"*/
    "<=PackingStart><R+3><RIGHT=C+8># Per Unit: <#PerUnits>"
    "<=PackingStart><R+4><RIGHT=C+8>Layers: <#Layers>"
    "<=PackingStart><R+5><RIGHT=C+8>Stacks: <#Stacks>"
    "<=PackingStart><R+6><RIGHT=C+8>Pattern: <#PatternCode>"
    "<=PackingStart><R+7><RIGHT=C+8>Desc: <#Pattern>"
    "<=PackingStart><R+8><RIGHT=C+8>Pallet: <#Palletwl>"
    "<=PackingTR><#PatternImageStart>"
    "<=PatternImageStart><C+22><#PatternImageTR>"
    "<=PatternImageStart><R+10><#PatternImageBL>"
    "<=PatternImageStart><C+22><R+10><#PatternImageEnd>"
    "<=PatternImageStart><R+.3><C+.3><#PatternImage><=PatternImageEnd><IMAGE#PatternImage=" + (IF AVAILABLE stackPattern THEN stackPattern.stackImage ELSE "") + "><=PatternImage>" FORMAT "x(300)" 
    "<=PatternImageTR><FROM><#PatternImageEnd><|1>"
    "<=PatternImageTR>"
    "<=Start><R33><#ShippingStart>"
    "<=ShippingStart><C1><#ShippingTR>"
    "<=ShippingStart><R+10><#ShippingBL>"
    "<=ShippingStart><C1><R+10><#ShippingEnd>"
    "<=ShippingStart><R+1><RIGHT=C+7>Ship To: <#ShipTo>"
    "<=ShippingStart><R+1><C35>Contact: "
    "<=ShippingStart><R+2><C+2><#ShipName>"
    "<=ShippingStart><R+2><C35><#ShipContact>"
    "<=ShippingStart><R+3><C+2><#ShipAdd1>"
    "<=ShippingStart><R+3><C35><#ShipPhone>"
    "<=ShippingStart><R+4><C+2><#ShipAdd2>"
    "<=ShippingStart><R+4><C35>Dock Hours: <#DockHours>"
    "<=ShippingStart><R+5><C+2><#ShipCityStateZip>"
    "<=ShippingStart><R+5><C35>Dock#: <#Dock>"
    "<=ShippingStart><R+6><C+2><#ShipNotes1>"
    "<=ShippingStart><R+7><C+2><#ShipNotes2>"
    "<=ShippingStart><R+8><C+2><#ShipNotes3>"
    "<=ShippingStart><R+9><C+2><#ShipNotes4>"
    "<=PackingBL><#ItemImageStart>"
    "<=ItemImageStart><C1><#ItemImageTR>"
    "<=ItemImageStart><R50><#ItemImageBL>"
    "<=ItemImageStart><C1><R50><#ItemImageEnd>"
    "<=PackingBL><FROM><R50><C1><RECT><|1> "
             
    "<=Pallet>" IF AVAILABLE xeb THEN xeb.tr-no ELSE "" FORMAT "x(10)" 
    /*"<=PalletLength>" IF AVAILABLE xeb THEN STRING(xeb.tr-len,">>9.99") ELSE "" FORMAT "x(6)"*/
    /*"<=PalletWidth>" IF AVAILABLE xeb THEN STRING(xeb.tr-wid,">>9.99") ELSE "" FORMAT "x(6)"*/
    "<=PerBundle>" IF AVAILABLE xeb THEN trim(STRING(xeb.cas-cnt,">>>>>>9")) ELSE "" FORMAT "x(7)"
    "<=PerUnits>" IF AVAILABLE xeb THEN trim(STRING(xeb.tr-cnt,">>>>>>9")) ELSE "" FORMAT "x(7)"
    /*"<B>"*/
    /*"<=CaseCount>" IF AVAILABLE xeb THEN STRING(xeb.tr-cnt) ELSE "" FORMAT "x(5)" */
    /*"<=PalletCount>" IF AVAILABLE xeb THEN STRING(xeb.cas-pal) ELSE "" FORMAT "x(6)" */
    /*"</B>"*/
    /*"<=JobCases>" IF AVAILABLE xeb THEN STRING(xeb.tr-cnt) ELSE "" FORMAT "x(5)"*/
    /*"<=JobPallets>" IF AVAILABLE xeb THEN STRING(xeb.cas-pal) ELSE "" FORMAT "x(6)"*/
    "<=Layers>" IF AVAILABLE xeb THEN STRING(xeb.tr-cas) ELSE "" FORMAT "x(4)"
    "<=Stacks>" IF AVAILABLE xeb THEN STRING(xeb.stacks) ELSE "" FORMAT "x(6)"
    "<=PatternCode>" IF AVAILABLE xeb THEN STRING(xeb.stack-code) ELSE "" FORMAT "x(3)"
    "<=Pattern>" IF AVAILABLE xeb AND AVAILABLE stackPattern THEN stackPattern.stackDescription ELSE "" FORMAT "x(30)"
    "<=Palletwl>" (IF AVAILABLE xeb THEN trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9")) ELSE "") format "x(15)"
    "<=ShipTo>" v-shipto  FORMAT "x(10)"
    "<=ShipName>" IF AVAILABLE shipto THEN shipto.ship-name ELSE "" FORMAT "x(30)"
    "<=ShipAdd1>" IF AVAILABLE shipto THEN shipto.ship-addr[1] ELSE "" FORMAT "x(30)"
    "<=ShipAdd2>" IF AVAILABLE shipto THEN shipto.ship-addr[2] ELSE "" FORMAT "x(30)"
    "<=ShipCityStateZip>" IF AVAILABLE shipto THEN TRIM(shipto.ship-city) + ", " +
    shipto.ship-state + "  " + shipto.ship-zip ELSE "" FORMAT "x(30)"
    "<=ShipContact>" IF AVAILABLE shipto THEN shipto.contact ELSE "" FORMAT "x(20)"
    "<=DockHours>" IF AVAILABLE shipto THEN shipto.dock-hour ELSE "" FORMAT "x(20)"
    "<=Dock>" IF AVAILABLE shipto THEN shipto.dock-loc ELSE "" FORMAT "x(20)"
    "<=ShipPhone>" IF AVAILABLE shipto THEN "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx") ELSE "" FORMAT "x(20)"
    "<=ShipNotes1>" IF AVAILABLE shipto THEN shipto.notes[1] ELSE "" FORMAT "x(90)"
    "<=ShipNotes2>" IF AVAILABLE shipto THEN shipto.notes[2] ELSE "" FORMAT "x(90)"
    "<=ShipNotes3>" IF AVAILABLE shipto THEN shipto.notes[3] ELSE "" FORMAT "x(90)"
    "<=ShipNotes4>" IF AVAILABLE shipto THEN shipto.notes[4] ELSE "" FORMAT "x(90)" .
  
ls-fgitem-img = bf-itemfg.box-image.

PUT UNFORMATTED 
    "<=ItemImageStart><R+.3><C+.3><#ItemImage><=ItemImageEnd><IMAGE#ItemImage=" ls-fgitem-img "><=ItemImage>".
             
PUT "<R33.7><C1><FROM><R33.7><C61><LINE> " .

PUT "<||><R23.5><C44><#8><FROM><R31><C61><RECT>" SKIP.
PUT "<R26><C44><FROM><R26><C61><LINE> " .
PUT "<R28.5><C44><FROM><R28.5><C61><LINE> " .
PUT "<R23.5><C49.5><FROM><R31><C49.5><LINE> " .
PUT "<R23.5><C55.5><FROM><R31><C55.5><LINE> " .
PUT "<||><R31><C44><#9><FROM><R33.5><C61><RECT>" SKIP.
PUT "<R31.1><C49.5>GRAND TOTAL" SKIP.

PAGE.

PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11><C30><FROM><R+4><C30><LINE><|3>"
            "<=11><C60><FROM><R+4><C60><LINE><|3>"
            "<=11><R+4><C1><FROM><C105><LINE><|3>"
            "<=11>Job # <C30> Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C70>" lv-part-no SKIP(4).

        if print-box and avail xest then do:
           FOR EACH tt-formtext:
               DELETE tt-formtext.
           END.
    
           lv-text = "".
           FOR EACH notes WHERE notes.rec_key = job.rec_key
                        AND (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                        AND notes.note_code = "BN" NO-LOCK:

                 lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
           END.
           DO li = 1 TO 2:
              CREATE tt-formtext.
              ASSIGN tt-line-no = li
                     tt-length  = 80.
           END.
           RUN custom/formtext.p (lv-text).
           ASSIGN
              i = 0
              v-dept-note = "".

           FOR EACH tt-formtext:
              i = i + 1.
              IF  i <= 2 THEN v-dept-note[i] = tt-formtext.tt-text.      
           END.
           PUT UNFORMATTED 
               "<UNITS=INCHES> <C3><R44.5><FROM><C25><R46.8><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE=" +
               cBarCodeVal + ">" FORMAT "x(150)" "<R46.9><C7>" 
                   cBarCodeVal  FORMAT "x(20)"
               "<R44><C1><FROM><C105><LINE><|3>" skip
               "<R44><C26><FROM><R48><C26><LINE><|3>"
               "<R44><C26.5><B>BN Notes:</B><C36>" v-dept-note[1] SKIP
               "<C36>" v-dept-note[2] "<=11><R+4>" SKIP.
           
           v-out1-id = RECID(xeb).
           run cec/desprnL2.p (recid(xef),
                              input-output v-lines,
                              recid(xest)).   
           PAGE.
        end.
        ELSE PAGE.

END.  /* for each w-ef */
IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN 
DO: /* print set header */
    i = 0.
    FOR EACH bf-eb WHERE bf-eb.company = est.company
        AND bf-eb.est-no = est.est-no
        AND bf-eb.form-no > 0 NO-LOCK:
        i = i + 1.
    END.   
    IF i > 1 THEN 
    DO:
        DEFINE VARIABLE v-set-qty   AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-ord-qty   AS INTEGER NO-UNDO.
        DEFINE VARIABLE v-over-run  AS cha     NO-UNDO.
        DEFINE VARIABLE v-under-run AS cha     NO-UNDO.
        DEFINE VARIABLE v-fg-set    AS cha     FORM "x(15)" NO-UNDO.
        ASSIGN
            v-fg-set    = job-hdr.i-no
            v-set-qty   = IF AVAILABLE xeb AND xeb.est-type EQ 6 THEN
                           IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty
                         ELSE 0
            v-ord-qty   = (IF AVAILABLE xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty) *
                         IF est.form-qty LE 1 THEN 1 ELSE v-pqty
            v-over-run  = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.over-pct,">>9.99%"))  ELSE ""
            v-under-run = IF AVAILABLE xoe-ordl THEN TRIM(STRING(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAILABLE xoe-ord  THEN TRIM(STRING(xoe-ord.under-pct,">>9.99%"))  ELSE "".
        IF v-job-cust AND NOT AVAILABLE xoe-ord AND AVAILABLE cust THEN
            ASSIGN
                v-over-run  = TRIM(STRING(cust.over-pct,">>9.99%"))
                v-under-run = TRIM(STRING(cust.under-pct,">>9.99%")) .

        PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
            "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
            "<C60>Our Date: " v-ord-date SKIP
            "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
            "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
            "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION" SKIP
            v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
            v-i-line[2] AT 90
            SKIP
            v-cus[2] AT 3 " Job Qty:" TRIM(STRING(job-hdr.qty * v-pqty,">>>,>>9"))    FORMAT "x(7)"
            " Order Qty:" STRING(v-ord-qty) FORMAT "x(7)"
            v-i-line[3] AT 90 SKIP
            v-cus[3] AT 3  " Cust Part #:" lv-part-no 
            v-i-line[4] AT 90 SKIP
            v-cus[4]  AT 3 " Overrun:"  FORMAT "x(7)"  
            " Underrun:" FORMAT "x(7)"  
            "Adders:" v-adders FORM "x(33)" AT 90 SKIP
            "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
            "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
        /* each components */
        DEFINE VARIABLE v-tmp-line AS INTEGER NO-UNDO.
        
        v-tmp-line = 0.
        FOR EACH xeb WHERE xeb.company = est.company
            AND xeb.est-no = est.est-no
            AND xeb.form-no > 0 NO-LOCK:
            PUT xeb.stock-no AT 3 SPACE(14) xeb.part-dscr1 SPACE(5) xeb.quantityPerSet FORMAT "->>>>>>>9" SKIP.
            v-tmp-line = v-tmp-line + 1.
        END.
        v-tmp-line = v-tmp-line + 1.
        /* print raw materials from misc/fram of Est */ 
        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
            AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        DO i = 1 TO 8:
            IF b-ef.spec-no[i] <> "" THEN 
            DO:
                RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                    OUTPUT lv-spec-qty[i]).
                PUT b-ef.spec-dscr[i] AT 32 SPACE(16) lv-spec-qty[i] SKIP.
                v-tmp-line = v-tmp-line + 1.
            END.
        END.
        PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
        v-tmp-line = v-tmp-line + 12 .
        
        i = 0.
        FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0:
            i = i + 1.
        END.
        i = i + 2.
        PUT /*"<C2>Machine Routing:  <C15> SU:    Start    Stop     Total    Run:   Start   Stop    total   qty   in   out  waste  date" SKIP*/
            "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
            "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
            "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
        .
        
        i = 0.
        FOR EACH tt-wm WHERE LOOKUP(tt-wm.m-code,tspostfg-char) > 0  BY tt-wm.dseq:
            i = i + 1.
            DISPLAY tt-wm.dscr AT 3
                tt-wm.s-hr 
                WHEN tt-wm.s-hr NE 0
                FILL("_",7)  FORMAT "x(7)"    TO 38   
                WHEN tt-wm.dscr NE ""
                FILL("_",7)  FORMAT "x(7)"    TO 46   
                WHEN tt-wm.dscr NE ""
                FILL("_",7)  FORMAT "x(7)"    TO 54   
                WHEN tt-wm.dscr NE ""
                SPACE(2)
                tt-wm.r-sp 
                WHEN tt-wm.r-sp NE 0
                FILL("_",7)  FORMAT "x(7)"    TO 69   
                WHEN tt-wm.dscr NE ""
                FILL("_",7)  FORMAT "x(7)"    TO 77   
                WHEN tt-wm.dscr NE ""
                FILL("_",7)  FORMAT "x(7)"    TO 85   
                WHEN tt-wm.dscr NE ""
                FILL("_",8)  FORMAT "x(8)"    TO 99   
                WHEN tt-wm.dscr NE ""
                FILL("_",8)  FORMAT "x(8)"    TO 108  
                WHEN tt-wm.dscr NE ""
                FILL("_",8)  FORMAT "x(8)"    TO 117  
                WHEN tt-wm.dscr NE ""
                FILL("_",8)  FORMAT "x(8)"    TO 129  
                WHEN tt-wm.dscr NE ""
                /*chr(124) format "x"           at 131   */                  
                WITH NO-BOX NO-LABELS FRAME o21 WIDTH 132 NO-ATTR-SPACE DOWN STREAM-IO.
        
        END.
        FOR EACH tt-wm:
            DELETE tt-wm.
        END.
        v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
        
        v-shipto = IF AVAILABLE xoe-rel THEN xoe-rel.ship-id 
        ELSE IF AVAILABLE xeb THEN xeb.ship-id
        ELSE IF AVAILABLE xoe-ord THEN xoe-ord.sold-id 
        ELSE "".
        FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
            AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt-prem THEN CREATE tt-prem.
        
        ASSIGN 
            v-tmp-lines   = 0
            j             = 0
            K             = 0
            lv-got-return = 0
            v-dept-inst   = "".
        
        {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                              notes.note_form_no = 0" }
        DO i = 1 TO 6:
            v-dept-inst[i] = v-inst2[i].
        END.
        IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */
        PUT "<=1><R+" + string(v-tmp-line) + ">" FORM "X(20)".
        v-tmp-line = v-tmp-line + 1.
        PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
            "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
            "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" SKIP
            "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" SKIP
            "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " SKIP
            "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
            "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP
        
            "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
            v-dept-inst[1] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[1] SKIP
            v-dept-inst[2] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[2] SKIP
            v-dept-inst[3] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[3] SKIP
            v-dept-inst[4] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" v-shp[4] SKIP
            v-dept-inst[5] AT 3 FORM "x(82)" CHR(124) FORMAT "xx" "Item PO #:" v-po-no SKIP
            v-dept-inst[6] 
            .
        
        PAGE.
    END. /* set header printing */
END. /* est.est-type = 6 */
/* end of set header printing */

END.  /* each job */
END.  /* end v-local-loop  */
 
HIDE ALL NO-PAUSE.


PROCEDURE stackImage:
    DEFINE BUFFER pattern      FOR reftable.
    DEFINE BUFFER stackPattern FOR stackPattern.
    IF v-stackcode EQ '' THEN RETURN.
    FIND FIRST stackPattern NO-LOCK
        WHERE stackPattern.stackCode EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
    IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN
        PUT UNFORMATTED
            "<#71><C27><R+1><FROM><C2><R+12>"
            "<IMAGE#71=" stackPattern.stackImage ">"
            "<R-13>".
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */

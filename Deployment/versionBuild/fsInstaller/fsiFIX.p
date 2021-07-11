&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fsiFIX.p
    Purpose     : Run data fix routines against a DECADE system

    Syntax      :

    Description :

    Author(s)   : MYT
    Created     : 14 SEP 2005
    Notes       : The first and main thing to remember when adding to this 
                  routine is that we don't necessarily know where the user 
                  is in the fix/patch process, so we ALWAYS need to test to 
                  see if a particular procedure should be run, using
                  known and unknown values.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR wpos AS CHAR NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR isempty AS LOG INITIAL TRUE NO-UNDO.
DEF VAR menuname AS CHAR NO-UNDO.
DEF VAR vCtr AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR vTopList AS CHAR NO-UNDO.
DEF VAR lDataChanged AS LOG NO-UNDO.
DEF VAR vProcName AS CHAR NO-UNDO.
DEF VAR vMemo-files AS CHAR NO-UNDO.

DEF BUFFER buf-menu FOR z_menu.
DEF BUFFER bzmenu FOR z_menu.
DEF BUFFER bzf FOR z_funct.
   

DEF TEMP-TABLE ttMenu
    FIELD menuname AS CHAR
    FIELD idx AS INT
    FIELD parentname AS CHAR
    FIELD parentidx AS INT
    FIELD pgmtype AS INT
    FIELD menudesc AS CHAR
    FIELD menupos AS INT
    FIELD sortorder AS INT
    FIELD runpgm AS CHAR
    FIELD level1 AS INT
    FIELD level2 AS INT
    FIELD level3 AS INT
    FIELD level4 AS INT
    FIELD level5 AS INT
    FIELD level6 AS INT
    FIELD level7 AS INT
    INDEX iidx idx
    INDEX ilevel
        level1
        level2
        level3
        level4
        level5
        level6
        level7
    INDEX isortorder parentname sortorder menudesc
    INDEX imenuname menuname
    INDEX iparentname parentname
    INDEX iparentidx parentidx.

DEF BUFFER bttMenu FOR ttMenu.

DEF VAR error-line AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CalcQuant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcQuant Procedure 
FUNCTION CalcQuant RETURNS CHARACTER
 ( INPUT qty AS INT, 
    INPUT entity AS char, 
    INPUT it-code AS CHAR, 
    INPUT in-uom AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvQuant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvQuant Procedure 
FUNCTION ConvQuant RETURNS INTEGER
  ( INPUT cQty AS CHAR,
    INPUT entity AS char, 
    INPUT it-code AS CHAR, 
    INPUT in-uom AS CHAR  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FORM 
    SKIP(1)
    "Running: " AT 2 vProcName FORMAT "x(60)"    AT 13 NO-LABEL
    SKIP(1)
    "(c) 2005, Foresight Software, Inc." AT 2
    "All rights reserved." TO 68
    WITH FRAME fixdisp TITLE "fsInstaller - Running Fixes"
    THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 77.

IF SEARCH("delta.df") <> ? THEN DO:
    ASSIGN
        vProcName = "ipLoadDelta"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipLoadDelta.
END.
IF CAN-FIND(FIRST order WHERE order.order-status = "") THEN DO:
    ASSIGN 
        vProcName = "ipInitOrderStatus".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipInitOrderStatus.
END.
IF CAN-FIND(FIRST z_gmenu WHERE z_gmenu.menu-name BEGINS "tpm") THEN DO:
    ASSIGN 
        vProcName = "ipCleanGMenu"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipCleanGMenu.
END.
IF NOT CAN-FIND(FIRST z_funct WHERE z_funct.funct-name = "xref.utility") THEN DO:
    ASSIGN 
        vProcName = "ipRenameCompiler"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipRenameCompiler.
END.
IF CAN-FIND(FIRST z_menu WHERE z_menu.menu-name BEGINS "tpm") THEN DO:
    ASSIGN 
        vProcName = "ipRemoveTpmTqm"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipRemoveTpmTqm.
END.
IF CAN-FIND(FIRST z_menu WHERE z_menu.menu-name BEGINS "tqm") THEN DO:
    ASSIGN 
        vProcName = "ipRemoveTpmTqm"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipRemoveTpmTqm.
END.
IF CAN-FIND(FIRST z_print WHERE z_print.PRINTER-NAME = "screen") THEN DO:
    ASSIGN
        vProcName = "ipFixPrintToScreen".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixPrintToScreen.
END.
IF CAN-FIND(FIRST z_gmenu WHERE z_gmenu.menu-desc BEGINS ":") THEN DO:
    ASSIGN
        vProcName = "ipFixColonMenus"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixColonMenus.
END.
/* Ted Added */
IF CAN-FIND(FIRST ar-hist-item where ar-hist-item.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixArHistItems"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixArHistItems.
END.
IF CAN-FIND(FIRST inv-jrnl where inv-jrnl.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvJnl"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvJnl.
END.
IF CAN-FIND(FIRST inv-tfr-s where inv-tfr-s.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvTfrS"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvTfrS.
END.
IF CAN-FIND(FIRST inv-tfr where inv-tfr.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvTfr"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvTfr.
END.
IF CAN-FIND(FIRST inv-tfrh where inv-tfrh.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvTfrH"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvTfrH.
END.
IF CAN-FIND(FIRST ar-open-item where ar-open-item.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixAROpenItem"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixAROpenItem.
END.
IF CAN-FIND(FIRST cash-jrnl-tr where cash-jrnl-tr.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixCashJrnlTr"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCashJrnlTr.
END.
IF CAN-FIND(FIRST appl-jrnl where appl-jrnl.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixApplJrnl"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixApplJrnl.
END.
IF CAN-FIND(FIRST cont-bil-lin where cont-bil-lin.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixContBilLin"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixContBilLin.
END.
IF CAN-FIND(FIRST sales-jrnl where sales-jrnl.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixSalesJrnlDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixSalesJrnlDoc.
END.
IF CAN-FIND(FIRST sales-jrnl where sales-jrnl.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixSalesJrnlRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixSalesJrnlRef.
END.
IF CAN-FIND(FIRST ar-hist-tr where ar-hist-tr.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixARHistTrDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixARHistTrDoc.
END.
IF CAN-FIND(FIRST ar-hist-tr where ar-hist-tr.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixARHistTrRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixARHistTrRef.
END.
IF CAN-FIND(FIRST ar-open-tr where ar-open-tr.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixAROpenTrDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixAROpenTrDoc.
END.
IF CAN-FIND(FIRST ar-open-tr where ar-open-tr.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixAROpenTrRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixAROpenTrRef.
END.
IF CAN-FIND(FIRST cash-jrnl where cash-jrnl.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixCashJrnlDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCashJrnlDoc.
END.
IF CAN-FIND(FIRST invoice where invoice.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceDoc.
END.
IF CAN-FIND(FIRST invoice where invoice.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceRef.
END.
IF CAN-FIND(FIRST commiss-jrnl where commiss-jrnl.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixCommisJnlDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCommisJnlDoc.
END.
IF CAN-FIND(FIRST commiss-jrnl where commiss-jrnl.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixCommisJnlRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCommisJnlRef.
END.
IF CAN-FIND(FIRST invoice-line where invoice-line.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceLine"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceLine.
END.
IF CAN-FIND(FIRST ar-gl-distr where ar-gl-distr.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixARGLDistrDoc"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixARGLDistrDoc.
END.
IF CAN-FIND(FIRST ar-gl-distr where ar-gl-distr.reference = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixARGLDistrRef"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixARGLDistrRef.
END.
IF CAN-FIND(FIRST sales-jrnl1 where sales-jrnl1.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixSalesJrnll"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixSalesJrnll.
END.
IF CAN-FIND(FIRST sales-hist where sales-hist.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixSalesHist"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixSalesHist.
END.
IF CAN-FIND(FIRST invoice-add where invoice-add.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceAdd"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceAdd.
END.

IF CAN-FIND(FIRST invoice-ln-s where invoice-ln-s.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceLnS"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceLnS.
END.
IF CAN-FIND(FIRST invoice-ln-s where invoice-ln-s.document = ?) THEN DO:
    ASSIGN
        vProcName = "ipFixInvoiceLnS"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixInvoiceLnS.
END.
IF CAN-FIND(FIRST z_memo where z_memo.file-name =  "PO":U and 
   length(entry(3,z_memo.key-list)) <= 2) THEN DO:
    ASSIGN
        vProcName = "ipFixPOZmemo"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixPOZmemo.
END.
IF CAN-FIND(FIRST z_memo where z_memo.file-name = "PO-LINE":U and
   length(entry(3,z_memo.key-list)) <= 2) THEN DO:
    ASSIGN
        vProcName = "ipFixPOLineZmemo"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixPOLineZmemo.
END.

IF CAN-FIND(FIRST z_param where z_param.param-file = "edi/acknlst.y") OR
    CAN-FIND(FIRST z_param where z_param.param-file = "edi/acknprg.y") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZParam"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZParam.
END.
IF CAN-FIND(FIRST z_menu where z_menu.menu-name = "edi.overview"
                  and z_menu.pos       = 3) 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZMenu"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZMenu.
END.
IF CAN-FIND(FIRST z_field where z_field.file-name = "edi-func-key" 
    AND z_field.field-name = "Return-value") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZField"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZField.
END.
IF CAN-FIND(FIRST z_funct where z_funct.funct-name = "rem.dir.lock")
                    or CAN-FIND(FIRST z_funct WHERE z_funct.funct-name = "rest.edi.data") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZFunct"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZFunct.
END.
IF CAN-FIND(FIRST ff-control WHERE 
            run_liability = ? OR ff-control.dt-start = "" OR ff-control.dt-end = "") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixFFCtrl"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixFFCtrl.
END.
IF CAN-FIND(FIRST z_param where param-file = "ca/camrrr.y" AND
            Field-name = "to-terr" or field-name = "sum-det") OR
    CAN-FIND(FIRST z_param where param-file = "pt/pxref.y" AND
             field-name = "do-pxref")
    THEN DO:
    ASSIGN
        vProcName = "ipFixZparam2"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZparam2.
END.
IF CAN-FIND(FIRST call-contact WHERE call-contact.line-no <> integer(substring(call-contact.contact,39,3))) 
    THEN DO:
    ASSIGN
        vProcName = "ipFixCallCont"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCallCont.
END.
IF CAN-FIND(FIRST call-solution WHERE call-solution.item-no = "") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixCallSol"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCallSol.
END.
IF CAN-FIND(FIRST call-solution where call-solution.knowledge ne "") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixCallKnow"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCallKnow.
END.
IF NOT CAN-FIND(FIRST call-activity where call-activity.next-date > 01/01/1900) 
    THEN DO:
    ASSIGN
        vProcName = "ipFixCallActiv"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixCallActiv.
END.
IF CAN-FIND(FIRST z_user WHERE z_user.see-login = ?) 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZUser"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZUser.
END.
IF CAN-FIND(FIRST z_param WHERE param-file = "pt/helplst.y":U AND Param-order = 10
            AND z_param.Field-name <> "from-user") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixZparam3"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixZparam3.
END.
IF CAN-FIND(FIRST chq-direct where chq-direct.week-no <= 0009999) 
    THEN DO:
    ASSIGN
        vProcName = "ipFixChqHdr"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixChqHdr.
END.
    ASSIGN
        vProcName = "ipCreateBinD"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipCreateBinD.

IF CAN-FIND(FIRST contract where contract.uom-code = "") 
    THEN DO:
    ASSIGN
        vProcName = "ipFixContractUOM"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixContractUOM.
END.
IF CAN-FIND(FIRST z_funct where funct-name = "chart.delete") 
OR CAN-FIND(FIRST z_funct where funct-name = "so-legal.maint") 
OR CAN-FIND(FIRST z_funct where funct-name = "itemvds.maint") 
OR CAN-FIND(FIRST z_funct where funct-name = "itemvdt.maint") 
    THEN DO:
    ASSIGN
        vProcName = "ipDelFunct"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipDelFunct.
END.
IF CAN-FIND(FIRST z_mess where mess-id >= "cp0090"
    AND mess-id <= "cp0099" AND mess-type <> 4) 
    THEN DO:
    ASSIGN
        vProcName = "ipMessType"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipMessType.
END.
IF CAN-FIND(FIRST z_nextnum WHERE FILE-NAME = "in-control-d" AND Entry-no = 2) 
    THEN DO:
    ASSIGN
        vProcName = "ipAddZnextnum"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipAddZnextnum.
END.
IF CAN-FIND(FIRST customer WHERE NOT CAN-FIND(customer-d OF customer)) 
    THEN DO:
    ASSIGN
        vProcName = "ipCustD"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipCustD.
END.
IF CAN-FIND(FIRST contract WHERE contract.start-time[1] = "") OR
   CAN-FIND(FIRST service WHERE
            service.Arrive-time  = "" OR
            service.Close-time   = "" OR
            service.Complete-tm  = "" OR
            service.Dispatch-tm  = "" OR
            service.Entry-time   = "" OR
            service.Escalate-tm  = "" OR
            service.ETA-time     = "" OR
            service.Notify-time  = "" OR
            service.Request-time = "" OR
            service.return-time  = "" OR
            service.Tele-time    = "" )
    THEN DO:
    ASSIGN
        vProcName = "ipMultiStartEnd"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipMultiStartEnd.
END.
IF CAN-FIND(FIRST service WHERE service.prd = 00 OR service.prd = ? OR
        service.yr = 0 OR service.yr = ?) 
    THEN DO:
    ASSIGN
        vProcName = "ipServicePrd"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipServicePrd.
END.
DO:
    ASSIGN 
        vMemo-files = "contract,contract-lin,serial,service,service-line,codes" +
        ",cust-add,ff-codes,ord-lin-shpmt,pm-header,pm-line,rv,tech,tech-cal" +
        ",tech-ind,tech-item,tech-terr,tech-vend,terr-zip,meter-card"
        vProcName = "ipZfilememo"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipZfilememo.
END.
IF CAN-FIND(FIRST z_funct WHERE funct-name = "temp.so.group") OR 
    CAN-FIND(FIRST z_menu WHERE funct-name = "temp.so.group") 
    THEN DO:
    ASSIGN
        vProcName = "ipClearfunct"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipClearfunct.
END.
IF NOT CAN-FIND(FIRST tax WHERE tax.tax-code = "NTX") OR 
   NOT CAN-FIND(FIRST in-tax WHERE in-tax.tax-code = "NTX") 
    THEN DO:
    ASSIGN
        vProcName = "ipCreateDefTax"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipCreateDefTax.
END.
IF CAN-FIND(FIRST z_menu WHERE z_menu.Funct-name = "gen-gmenu") OR
    CAN-FIND(FIRST z_funct WHERE z_funct.Funct-name = "gen-gmenu") OR
    CAN-FIND(FIRST z_menu WHERE z_menu.menu-name = "progress" AND 
             z_menu.funct-name = "dataadmin")
    THEN DO:
    ASSIGN
        vProcName = "ipDelGenMenu"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipDelGenMenu.
END.
IF CAN-FIND(FIRST z_menu WHERE z_menu.Funct-name = "item.vend.inf") OR
    CAN-FIND(FIRST z_menu WHERE z_menu.funct-name = "item.vend.spec")
    THEN DO:
    ASSIGN
        vProcName = "ipFixItemVendInq"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipFixItemVendInq.
END.
IF CAN-FIND(FIRST op-control WHERE op-control.orders-to-view = "") 
    THEN DO:
    ASSIGN
        vProcName = "ipOPCtrlToView"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipOPCtrlToView.
END.
IF CAN-FIND(FIRST z_zoom-ln WHERE 
    z_zoom-ln.Funct-name = "wo.mat.inq" AND z_zoom-ln.Entry-no = 7) 
    THEN DO:
    ASSIGN
        vProcName = "ipDelZoom"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipDelZoom.
END.
IF CAN-FIND(FIRST po-control WHERE 
    po-control.POs-to-view = "") 
    THEN DO:
    ASSIGN
        vProcName = "ipPostoview"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipPostoview.
END.
IF CAN-FIND(FIRST op-control) OR
    CAN-FIND(FIRST po-control) THEN DO:
    ASSIGN
        vProcName = "ipHomeCurr"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipHomeCurr.
END.
IF CAN-FIND(FIRST order WHERE order.ship-telephone = "") THEN DO:
    ASSIGN
        vProcName = "ipOrdTelno"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipOrdTelno.
END.
IF CAN-FIND(FIRST invoice WHERE invoice.country = "") THEN DO:
    ASSIGN
        vProcName = "ipInvCountry"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipInvCountry.
END.
RUN Update-next-files IN THIS-PROCEDURE. /* - Procedure at max size */

/* End Ted's Addition */


IF CAN-FIND (FIRST order-line WHERE order-line.c-ordered = "") THEN DO:
   ASSIGN vProcName = "ipOrderLine".
    DISPLAY vProcName WITH FRAME fixdisp. 
    RUN ipOrderLine.
END.

IF CAN-FIND (FIRST serial WHERE serial.cust-no <> "" AND serial.sort-name = "") THEN DO:
    ASSIGN vProcName = "ipSerialSortName"
           lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipSerialSortName.
END.
if can-find (first call-header where call-header.icall-no = 0) then do:
   assign vProcName = "ipCallHeader".
   display vprocname with frame fixdisp.
   run ipcallheader.
end.
IF CAN-FIND (FIRST codes WHERE codes.code-type = "METY":U AND
             codes.chr-1 = "") THEN DO:
    ASSIGN vProcName = "ipInitMemoTypeDef".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipInitMemoTypeDef.
END.
/* force data changed for testing */
ASSIGN
    lDataChanged = TRUE.
    
/* Note: only set lDataChanged if you want to rebuild ALL the internals (menus, procs, xrefs, etc.) */
IF lDataChanged THEN DO:
    ASSIGN 
        vProcName = "ipCleanInfrastructure".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipCleanInfrastructure.
    
    ASSIGN 
        vProcName = "ipRegenGUIMenus".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipRegenGUIMenus.
    
    ASSIGN 
        vProcName = "ipRegenSchema".
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN ipRegenSchema.
END.

HIDE FRAME fixdisp NO-PAUSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ipAddZnextnum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipAddZnextnum Procedure 
PROCEDURE ipAddZnextnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bzn FOR z_nextnum.

    FOR EACH z_nextnum WHERE FILE-NAME = "in-control-d"
        BREAK BY z_nextnum.entity-code:
        IF FIRST-OF(z_nextnum.entity-code) 
        AND NOT CAN-FIND(FIRST bzn WHERE
            bzn.entity-code = z_nextnum.entity-code AND
            bzn.file-name = z_nextnum.file-name AND
            bzn.last-control = z_nextnum.last-control)
        THEN DO:
            CREATE bzn.
            ASSIGN bzn.entity-code = z_nextnum.Entity-code 
                bzn.entry-no = 2
                bzn.FILE-NAME =  z_nextnum.File-name
                bzn.last-control =  z_nextnum.Last-control.
            FIND z_numgen WHERE z_numgen.FILE-NAME = "in-control-d"
                AND z_numgen.entity-code = z_nextnum.entity-code
                EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN z_numgen.last-control[2] = z_nextnum.last-control.
        END.
    END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCallHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCallHeader Procedure 
PROCEDURE ipCallHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
For each call-header:
Assign icall-no = integer(call-header.call-no).
End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCleanGMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCleanGMenu Procedure 
PROCEDURE ipCleanGMenu :
/*------------------------------------------------------------------------------
  Purpose: Delete unused.broken z_gmenu items    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH z_gmenu WHERE 
        INDEX(z_gmenu.menu-name,".technical") <> 0 OR
        INDEX(z_gmenu.menu-parent,".technical") <> 0 OR
        INDEX(z_gmenu.menu-name,".demo") <> 0 OR
        INDEX(z_gmenu.menu-parent,".demo") <> 0 OR
        INDEX(z_gmenu.menu-name,"tpm.") <> 0 OR
        INDEX(z_gmenu.menu-parent,"tpm.") <> 0 OR
        INDEX(z_gmenu.menu-name,"tqm.") <> 0 OR
        INDEX(z_gmenu.menu-parent,"tqm.") <> 0 OR
        (INDEX(z_gmenu.menu-parent,"pb.dev.inquiry") <> 0 AND INDEX(z_gmenu.menu-name,"source.inq") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"dump.maker") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"icon.driver") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"pb.relations") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"pb.runpgm") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"pb.viewstack") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.development") <> 0 AND INDEX(z_gmenu.menu-name,"xcomp") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"calculator") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"demo.driver") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"documentor") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"help.maker") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"menu") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"on-line.doc") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"popup.calendar") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"probase") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"trace") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.help") <> 0 AND INDEX(z_gmenu.menu-name,"zoom.driver") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.inquiry") <> 0 AND INDEX(z_gmenu.menu-name,"site.inq") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.macros") <> 0 AND INDEX(z_gmenu.menu-name,"pb.files") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.macros") <> 0 AND INDEX(z_gmenu.menu-name,"pb.source") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"data.entry") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"design") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"entry.form") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"file.sheet") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"menu.sheet") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"mxp.maint") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"pb.features") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"security") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.overview") <> 0 AND INDEX(z_gmenu.menu-name,"tap.guides") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.support") <> 0 AND INDEX(z_gmenu.menu-name,"dv.report1") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"pb.support") <> 0 AND INDEX(z_gmenu.menu-name,"no-wait") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"in.maintenance") <> 0 AND INDEX(z_gmenu.menu-name,"get.item") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"hd") <> 0 AND INDEX(z_gmenu.menu-name,"patch.create") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"hd") <> 0 AND INDEX(z_gmenu.menu-name,"profit.demo") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"mxp") <> 0 AND INDEX(z_gmenu.menu-name,"tpm") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"mxp") <> 0 AND INDEX(z_gmenu.menu-name,"tqm") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"no-wait") <> 0 AND INDEX(z_gmenu.menu-name,"stats.browse") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"no-wait") <> 0 AND INDEX(z_gmenu.menu-name,"stats.create") <> 0) OR
        (INDEX(z_gmenu.menu-parent,"no-wait") <> 0 AND INDEX(z_gmenu.menu-name,"stats.update") <> 0) :
        
        FIND z_menu WHERE 
            z_menu.funct-name = z_gmenu.menu-name AND
            z_menu.menu-name = z_gmenu.menu-parent NO-ERROR.
        IF AVAIL z_menu THEN DO:
            ASSIGN vCtr = vCtr + 1.
            DELETE z_gmenu.
            IF z_menu.pos <> 0 THEN DELETE z_menu.
            ELSE ASSIGN vTopList = vTopList + z_menu.menu-name + ",".
        END.
    END.

    RUN ipRegenMenus.
    
    DO vCtr = 1 TO NUM-ENTRIES(vTopList):
        FIND z_menu WHERE 
            z_menu.menu-name = ENTRY(vCtr,vTopList) AND 
            z_menu.pos = 0 NO-ERROR.
        IF AVAIL z_menu THEN DELETE z_menu.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCleanInfrastructure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCleanInfrastructure Procedure 
PROCEDURE ipCleanInfrastructure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH z_module EXCLUSIVE-LOCK:
        FIND z_funct WHERE z_funct.funct-name = z_module.funct-name NO-LOCK NO-ERROR.
        IF NOT AVAIL z_funct THEN DELETE z_module.
    END.
      
    FOR EACH z_shortcut EXCLUSIVE-LOCK:
        FIND z_funct WHERE z_funct.funct-name = z_shortcut.funct-name NO-LOCK NO-ERROR.
        IF NOT AVAIL z_funct THEN DELETE z_shortcut.
    END.
    
    FOR EACH z_fxref EXCLUSIVE-LOCK:
        FIND FIRST z_module WHERE 
            z_module.module-name = z_fxref.module-name
            NO-LOCK NO-ERROR.
        IF NOT AVAIL z_module THEN DELETE z_fxref.
    END.
        
    FOR EACH z_proc EXCLUSIVE-LOCK:
        FIND FIRST z_module WHERE 
            z_module.module-name = z_proc.module-name
            NO-LOCK NO-ERROR.
        IF NOT AVAIL z_module THEN DELETE z_proc.
    END.
    
    FOR EACH z_pxref EXCLUSIVE-LOCK:
        FIND FIRST z_module WHERE 
            z_module.module-name = z_pxref.module-name
            NO-LOCK NO-ERROR.
        IF NOT AVAIL z_module THEN DELETE z_pxref.
    END.
        
    FOR EACH z_zoom EXCLUSIVE-LOCK:
        FIND z_funct WHERE 
            z_funct.funct-name = z_zoom.funct-name
            NO-LOCK NO-ERROR.
        IF NOT AVAIL z_funct THEN DELETE z_zoom.
    END.
    
    FOR EACH z_zoom-ln:
        FIND z_funct WHERE 
            z_funct.funct-name = z_zoom-ln.Funct-name 
            NO-LOCK NO-ERROR.
        
        FIND bzf WHERE 
            bzf.funct-name = z_zoom-ln.Zoom-funct 
            NO-LOCK NO-ERROR.
    
        IF NOT AVAIL z_funct OR NOT AVAIL bzf THEN DELETE z_zoom-ln.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipClearfunct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipClearfunct Procedure 
PROCEDURE ipClearfunct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH z_funct WHERE 
       z_funct.funct-name = "temp.so.group" EXCLUSIVE-LOCK:
       DELETE z_funct.
   END.

    FOR EACH z_menu WHERE 
       z_menu.funct-name = "temp.so.group" EXCLUSIVE-LOCK:
       DELETE z_menu.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCreateBinD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateBinD Procedure 
PROCEDURE ipCreateBinD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH bin:
        FIND bin-d OF bin NO-LOCK NO-ERROR.
        IF NOT AVAIL bin-d THEN do:
            CREATE bin-d.
            ASSIGN bin-d.Bin-location = bin.bin-location
                bin-d.In-entity = bin.in-entity
                bin-d.Whs-code = bin.whs-code.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCreateDefTax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreateDefTax Procedure 
PROCEDURE ipCreateDefTax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF NOT CAN-FIND(tax WHERE tax.tax-code = "NTX") THEN DO:
       CREATE tax.
       ASSIGN 
           tax.tax-code = "NTX"
           tax.tax-desc = "No Tax".
   END.

   IF NOT CAN-FIND(in-tax WHERE in-tax.tax-code = "NTX") THEN DO:
       CREATE in-tax.
       ASSIGN 
           in-tax.tax-code = "NTX"
           in-tax.tax-desc = "No Tax".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCustCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCustCountry Procedure 
PROCEDURE ipCustCountry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   For each customer where customer.country = "" exclusive-lock:
       FIND country WHERE 
           country.country-code = customer.country-code NO-LOCK NO-ERROR.
       IF AVAIL country AND
           country.DESCRIPTION <> "" THEN
           ASSIGN customer.country = country.DESCRIPTION.
   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipCustD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCustD Procedure 
PROCEDURE ipCustD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH customer NO-LOCK:
        FIND customer-d OF customer NO-LOCK NO-ERROR.
        IF NOT AVAIL customer-d THEN
            DO:
            CREATE customer-d.
            ASSIGN customer-d.cust-no = customer.cust-no.
        END.
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipDelFunct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelFunct Procedure 
PROCEDURE ipDelFunct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vList AS CHAR NO-UNDO.
DEF VAR c-Funct AS CHAR NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.

ASSIGN vList = "chart.delete,so-legal.maint,itemvds.maint,itemvdt.maint".


DO iCtr = 1 to NUM-ENTRIES(vList):

    ASSIGN c-funct = ENTRY(iCtr,vList).

    For each z_funct where z_funct.funct-name = c-funct exclusive-lock:
        For each z_menu where z_menu.funct-name = c-funct exclusive-lock :
            Delete z_menu.                                                    
        END.
        For each z_shortcut where z_shortcut.funct-name = c-funct exclusive-lock:
            Delete z_shortcut.
        END.
        Delete z_funct.
    END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipDelGenMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelGenMenu Procedure 
PROCEDURE ipDelGenMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH z_menu WHERE 
    z_menu.Funct-name = "gen-gmenu":
    FOR EACH z_image WHERE 
        z_image.menu-name = z_menu.menu-name:
        DO j = 1 TO 32 :
            IF substr(z_image.entry-desc[j],1,2) = string(z_menu.pos) THEN DO:
               ASSIGN z_image.entry-desc[j] = "".
            END.
        END.
        DO j = 1 TO 32:
            IF z_image.funct-list[j] = "gen-gmenu" THEN DO:
                ASSIGN z_image.funct-list[j] = "".
            END.
        END.
    END.
    DELETE z_menu.
END.

FOR each z_funct WHERE
    z_funct.Funct-name = "gen-gmenu" EXCLUSIVE-LOCK:
    DELETE z_funct.
END.

FOR EACH z_menu WHERE 
    z_menu.menu-name = "progress" AND z_menu.funct-name = "dataadmin":
    DELETE z_menu.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipDelZoom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDelZoom Procedure 
PROCEDURE ipDelZoom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH z_zoom-ln WHERE 
    z_zoom-ln.Funct-name = "wo.mat.inq" AND z_zoom-ln.Entry-no = 7:
    DELETE z_zoom-ln.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixApplJrnl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixApplJrnl Procedure 
PROCEDURE ipFixApplJrnl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each appl-jrnl where appl-jrnl.document = ? exclusive-lock:
  assign appl-jrnl.document = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixARGLDistrDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixARGLDistrDoc Procedure 
PROCEDURE ipFixARGLDistrDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ar-gl-distr where ar-gl-distr.document = ? exclusive-lock:
 assign ar-gl-distr.document = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixARGLDistrRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixARGLDistrRef Procedure 
PROCEDURE ipFixARGLDistrRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ar-gl-distr where ar-gl-distr.reference = ? exclusive-lock:
 assign ar-gl-distr.reference = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixArHistItems) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixArHistItems Procedure 
PROCEDURE ipFixArHistItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-ar-hist-item FOR ar-hist-item.

    for each ar-hist-item where ar-hist-item.reference = ? exclusive-lock:
        IF CAN-FIND(b-ar-hist-item WHERE
                    b-ar-hist-item.Ar-entity    = ar-hist-item.Ar-entity    AND
                    b-ar-hist-item.Cust-no      = ar-hist-item.Cust-no      AND
                    b-ar-hist-item.Reference    = 0                         AND
                    b-ar-hist-item.Ar-account   = ar-hist-item.Ar-account   AND
                    b-ar-hist-item.Currency-cod = ar-hist-item.Currency-cod)
            THEN DO:
            ASSIGN error-line = "ar-hist-item already exists with: " +
                   " ar-hist-item.Ar-entity    = " + ar-hist-item.Ar-entity    +
                   " ar-hist-item.Cust-no      = " + ar-hist-item.Cust-no      +
                   " ar-hist-item.Reference    = " + "0"                       +
                   " ar-hist-item.Ar-account   = " + ar-hist-item.Ar-account   +
                   " ar-hist-item.Currency-cod = " + ar-hist-item.Currency-cod.
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign ar-hist-item.reference = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixARHistTrDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixARHistTrDoc Procedure 
PROCEDURE ipFixARHistTrDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ar-hist-tr where ar-hist-tr.document = ? exclusive-lock:
 assign ar-hist-tr.document = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixARHistTrRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixARHistTrRef Procedure 
PROCEDURE ipFixARHistTrRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-ar-hist-tr FOR ar-hist-tr.

    for each ar-hist-tr where ar-hist-tr.reference = ? exclusive-lock:
        IF CAN-FIND(b-ar-hist-tr WHERE
                    b-ar-hist-tr.Ar-entity    = ar-hist-tr.Ar-entity    AND
                    b-ar-hist-tr.Cust-no      = ar-hist-tr.Cust-no      AND
                    b-ar-hist-tr.Reference    = 0                       AND
                    b-ar-hist-tr.Ar-account   = ar-hist-tr.Ar-account   AND
                    b-ar-hist-tr.seq-no       = ar-hist-tr.seq-no       AND
                    b-ar-hist-tr.trans-code   = ar-hist-tr.trans-code   AND
                    b-ar-hist-tr.jrnl-no      = ar-hist-tr.jrnl-no)
            THEN DO:
            ASSIGN error-line = "ar-hist-tr already exists with: " +
                   " ar-hist-tr.Ar-entity    = " + ar-hist-tr.Ar-entity    +
                   " ar-hist-tr.Cust-no      = " + ar-hist-tr.Cust-no      +
                   " ar-hist-tr.Reference    = " + "0"                     +
                   " ar-hist-tr.Ar-account   = " + ar-hist-tr.Ar-account   +
                   " ar-hist-tr.seq-no       = " + STRING(ar-hist-tr.seq-no) +
                   " ar-hist-tr.trans-code   = " + ar-hist-tr.trans-code    +
                   " ar-hist-tr.jrnl-no      = " + STRING(ar-hist-tr.jrnl-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign ar-hist-tr.reference = 0.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixAROpenItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixAROpenItem Procedure 
PROCEDURE ipFixAROpenItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-ar-open-item FOR ar-open-item.

    for each ar-open-item where ar-open-item.reference = ? exclusive-lock:
        IF CAN-FIND(b-ar-open-item WHERE
                    b-ar-open-item.Ar-entity    = ar-open-item.Ar-entity    AND
                    b-ar-open-item.Cust-no      = ar-open-item.Cust-no      AND
                    b-ar-open-item.Reference    = 0                         AND
                    b-ar-open-item.Ar-account   = ar-open-item.Ar-account   AND
                    b-ar-open-item.Currency-cod = ar-open-item.Currency-cod)
            THEN DO:
            ASSIGN error-line = "ar-open-item already exists with: " +
                   " ar-open-item.Ar-entity    = " + ar-open-item.Ar-entity    +
                   " ar-open-item.Cust-no      = " + ar-open-item.Cust-no      +
                   " ar-open-item.Reference    = " + "0"                       +
                   " ar-open-item.Ar-account   = " + ar-open-item.Ar-account   +
                   " ar-open-item.Currency-cod = " + ar-open-item.Currency-cod.
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign ar-open-item.reference = 0.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixAROpenTrDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixAROpenTrDoc Procedure 
PROCEDURE ipFixAROpenTrDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ar-open-tr where ar-open-tr.document = ? exclusive-lock:
 assign ar-open-tr.document = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixAROpenTrRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixAROpenTrRef Procedure 
PROCEDURE ipFixAROpenTrRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-ar-open-tr FOR ar-open-tr.

    for each ar-open-tr where ar-open-tr.reference = ? exclusive-lock:
        IF CAN-FIND(b-ar-open-tr WHERE
                    b-ar-open-tr.Ar-entity    = ar-open-tr.Ar-entity    AND
                    b-ar-open-tr.Cust-no      = ar-open-tr.Cust-no      AND
                    b-ar-open-tr.Reference    = 0                       AND
                    b-ar-open-tr.Ar-account   = ar-open-tr.Ar-account   AND
                    b-ar-open-tr.seq-no       = ar-open-tr.seq-no       AND
                    b-ar-open-tr.trans-code   = ar-open-tr.trans-code   AND
                    b-ar-open-tr.jrnl-no      = ar-open-tr.jrnl-no)
            THEN DO:
            ASSIGN error-line = "ar-open-tr already exists with: " +
                   " ar-open-tr.Ar-entity    = " + ar-open-tr.Ar-entity    +
                   " ar-open-tr.Cust-no      = " + ar-open-tr.Cust-no      +
                   " ar-open-tr.Reference    = " + "0"                     +
                   " ar-open-tr.Ar-account   = " + ar-open-tr.Ar-account   +
                   " ar-open-tr.seq-no       = " + STRING(ar-open-tr.seq-no) +
                   " ar-open-tr.trans-code   = " + ar-open-tr.trans-code    +
                   " ar-open-tr.jrnl-no      = " + STRING(ar-open-tr.jrnl-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign ar-open-tr.reference = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCallActiv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCallActiv Procedure 
PROCEDURE ipFixCallActiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each call-activity exclusive-lock:
  assign next-date = ?.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCallCont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCallCont Procedure 
PROCEDURE ipFixCallCont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each call-contact WHERE
    call-contact.line-no <> integer(substring(call-contact.contact,39,3))
     exclusive-lock:
    assign call-contact.line-no = integer(substring(call-contact.contact,39,3))
        call-contact.activity-no = 
        integer(substring(call-contact.contact,42,4)).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCallKnow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCallKnow Procedure 
PROCEDURE ipFixCallKnow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each call-solution where call-solution.knowledge ne "":
     find knowledge
          where knowledge.knowledge-no = call-solution.knowledge-no
          exclusive-lock no-error.
     if avail knowledge then
     assign knowledge.serial-no = call-solution.serial-no.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCallSol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCallSol Procedure 
PROCEDURE ipFixCallSol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each call-problem where call-problem.solution-no ne 0:
        find call-solution
            where call-solution.solution-no = call-problem.solution-no
            exclusive-lock no-error.
        if avail call-solution then
        assign call-solution.item-no = call-problem.item-no
               call-solution.serial-no = call-problem.serial-no.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCashJrnlDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCashJrnlDoc Procedure 
PROCEDURE ipFixCashJrnlDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each cash-jrnl where cash-jrnl.document = ? exclusive-lock:
 assign cash-jrnl.document = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCashJrnlTr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCashJrnlTr Procedure 
PROCEDURE ipFixCashJrnlTr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-cash-jrnl-tr FOR cash-jrnl-tr.

    for each cash-jrnl-tr where cash-jrnl-tr.reference = ? exclusive-lock:
        IF CAN-FIND(b-cash-jrnl-tr WHERE
                    b-cash-jrnl-tr.Ar-entity    = cash-jrnl-tr.Ar-entity    AND
                    b-cash-jrnl-tr.trans-code   = cash-jrnl-tr.trans-code   AND
                    b-cash-jrnl-tr.seq-no       = cash-jrnl-tr.seq-no       AND
                    b-cash-jrnl-tr.Reference    = 0                         )
            THEN DO:
            ASSIGN error-line = "cash-jrnl-tr already exists with: " +
                   " cash-jrnl-tr.Ar-entity    = " + cash-jrnl-tr.Ar-entity    +
                   " cash-jrnl-tr.trans-code   = " + cash-jrnl-tr.trans-code   +
                   " cash-jrnl-tr.seq-no   =     " + STRING(cash-jrnl-tr.seq-no)   +
                   " cash-jrnl-tr.Reference    = " + "0"                       .
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign cash-jrnl-tr.reference = 0.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixChqHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixChqHdr Procedure 
PROCEDURE ipFixChqHdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each chq-direct where chq-direct.company = chq-direct.company
                      and chq-direct.week-no <= 0009999 exclusive-lock:
   chq-direct.week-no = (chq-direct.week-no * 10) + 1900000.
end.

for each chq-dtl exclusive-lock:
   chq-dtl.week-no = (chq-dtl.week-no * 10) + 1900000.
end.

for each chq-hdr exclusive-lock:

   assign
   chq-hdr.week-no = (chq-hdr.week-no * 10) + 1900000
   chq-hdr.wk-apply = (chq-hdr.wk-apply * 10) + 1900000.

   if chq-hdr.week-used <> 0 then
      chq-hdr.week-used = (chq-hdr.week-used * 10) + 1900000.

end.

for each empl-ie where empl-ie.company eq empl-ie.company
                   and empl-ie.empl-no eq empl-ie.empl-no
                   and empl-ie.week-no < 0009999
                   exclusive-lock:
   empl-ie.week-no = (empl-ie.week-no * 10) + 1900000.
end.

for each sick-hist exclusive-lock:
   sick-hist.week-no = (sick-hist.week-no * 10) + 1900000.
end.

for each vacn-hist exclusive-lock:
   vacn-hist.week-no = (vacn-hist.week-no * 10) + 1900000.
end.

for each week where week.company = week.company
                and week.week-no >= 0001000
                and week.week-no <= 0009999 exclusive-lock:
   week.week-no = (week.week-no * 10) + 1900000.
end.

for each week where week.company = week.company
                and week.week-no <= 0000999
                exclusive-lock:
   week.week-no = (week.week-no * 10) + 2000000.
end.

for each tran exclusive-lock:

   assign
   tran.week-no = (tran.week-no * 10) + 1900000
   tran.wk-apply = (tran.wk-apply * 10) + 1900000.

   if tran.week-used <> 0 then
     tran.week-used = (tran.week-used * 10) + 1900000.

   if tran.week-auto <> 0 then
     tran.week-auto = (tran.week-auto * 10) + 1900000.

end.

for each cp-param where cp-param.company = cp-param.company
                        and cp-param.param-no = 102 exclusive-lock:
   assign
   cp-param.param-val  = (cp-param.param-val * 10) + 1900000
   cp-param.description[1] = "Current Pay Period (YYYYWWW)"
   cp-param.description[2] = "YYYY = year-no   WWW = pay period" .
end.

for each cp-param-a where cp-param-a.company = cp-param-a.company
                        and cp-param-a.param-no = 102 exclusive-lock:
   assign
   cp-param-a.param-val  = (cp-param-a.param-val * 10) + 1900000
   cp-param-a.description[1] = "Current Pay Period (YYYYWWW)"
   cp-param-a.description[2] = "YYYY = year-no   WWW = pay period" .
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixColonMenus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixColonMenus Procedure 
PROCEDURE ipFixColonMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH z_gmenu WHERE z_gmenu.menu-desc BEGINS ":":
        ASSIGN z_gmenu.menu-desc = TRIM(z_gmenu.menu-desc,":").
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCommisJnlDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCommisJnlDoc Procedure 
PROCEDURE ipFixCommisJnlDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each commiss-jrnl where commiss-jrnl.document = ? exclusive-lock:
 assign commiss-jrnl.document = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixCommisJnlRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixCommisJnlRef Procedure 
PROCEDURE ipFixCommisJnlRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each commiss-jrnl where commiss-jrnl.reference = ? exclusive-lock:
 assign commiss-jrnl.reference = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixContBilLin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixContBilLin Procedure 
PROCEDURE ipFixContBilLin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-cont-bil-lin FOR cont-bil-lin.

    for each cont-bil-lin where cont-bil-lin.document = ? exclusive-lock:
        IF CAN-FIND(b-cont-bil-lin WHERE
                    b-cont-bil-lin.record-type = cont-bil-lin.record-type  AND
                    b-cont-bil-lin.contract-no = cont-bil-lin.contract-no  AND 
                    b-cont-bil-lin.line-no     = cont-bil-lin.line-no      AND
                    b-cont-bil-lin.document    = 0                         )
            THEN DO:
            ASSIGN error-line = "cont-bil-lin already exists with: " +
                   " cont-bil-lin.record-type  = " + cont-bil-lin.record-type     +
                   " cont-bil-lin.contract-no  = " + cont-bil-lin.contract-no     +
                   " cont-bil-lin.line-no      = " + STRING(cont-bil-lin.line-no) +
                   " cont-bil-lin.document     = " + "0"                       .
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign cont-bil-lin.do_liability = yes.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixContractUOM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixContractUOM Procedure 
PROCEDURE ipFixContractUOM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    For each contract where contract.uom-code = "" exclusive-lock:
        Find first uom no-lock no-error.
        Assign contract.uom-code = uom.uom-code.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixFFCtrl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixFFCtrl Procedure 
PROCEDURE ipFixFFCtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each ff-control EXCLUSIVE-LOCK:
    IF run_liability = ? THEN
        assign 
        run_liability = "Y"
        renumber-line = no.
    IF ff-control.dt-start = "" THEN 
        ASSIGN ff-control.dt-start = "0000".
    IF ff-control.dt-end = "" THEN 
        ASSIGN ff-control.dt-end = "0000".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvJnl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvJnl Procedure 
PROCEDURE ipFixInvJnl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each inv-jrnl where inv-jrnl.reference = ? exclusive-lock:
  assign inv-jrnl.reference = " ".
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvoiceAdd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvoiceAdd Procedure 
PROCEDURE ipFixInvoiceAdd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-invoice-add FOR invoice-add.

    for each invoice-add where invoice-add.document = ? exclusive-lock:
        IF CAN-FIND(b-invoice-add WHERE
                    b-invoice-add.Ar-entity    = invoice-add.Ar-entity    AND
                    b-invoice-add.document     = 0                     AND
                    b-invoice-add.charge-code  = invoice-add.charge-code   AND
                    b-invoice-add.seq-no       = invoice-add.seq-no AND
                    b-invoice-add.order-no     = invoice-add.order-no)
            THEN DO:
            ASSIGN error-line = "invoice-add already exists with: " +
                   " invoice-add.Ar-entity    = " + invoice-add.Ar-entity          +
                   " invoice-add.document     = " + "0"                        +
                   " invoice-add.charge-code  = " + STRING(invoice-add.charge-code) +
                   " invoice-add.seq-no       = " + STRING(invoice-add.seq-no) +
                   " invoice-add.order-no     = " + STRING(invoice-add.order-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
 assign invoice-add.document = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvoiceDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvoiceDoc Procedure 
PROCEDURE ipFixInvoiceDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-invoice FOR invoice.

    for each invoice where invoice.document = ? exclusive-lock:
        IF CAN-FIND(b-invoice WHERE
                    b-invoice.Ar-entity    = invoice.Ar-entity    AND
                    b-invoice.document    = 0                     AND
                    b-invoice.inv-credit   = invoice.inv-credit   AND
                    b-invoice.seq-no       = invoice.seq-no)
            THEN DO:
            ASSIGN error-line = "invoice already exists with: " +
                   " invoice.Ar-entity    = " + invoice.Ar-entity          +
                   " invoice.document     = " + "0"                        +
                   " invoice.inv-credit   = " + STRING(invoice.inv-credit) +
                   " invoice.seq-no       = " + STRING(invoice.seq-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign invoice.document = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvoiceLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvoiceLine Procedure 
PROCEDURE ipFixInvoiceLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-invoice-line FOR invoice-line.

    for each invoice-line where invoice-line.document = ? exclusive-lock:
        IF CAN-FIND(b-invoice-line WHERE
                    b-invoice-line.Ar-entity  = invoice-line.Ar-entity AND
                    b-invoice-line.document   = 0                      AND
                    b-invoice-line.doc-line   = invoice-line.doc-line  AND
                    b-invoice-line.seq-no     = invoice-line.seq-no)
            THEN DO:
            ASSIGN error-line = "invoice-line already exists with: " +
                   " invoice-line.Ar-entity    = " + invoice-line.Ar-entity          +
                   " invoice-line.document     = " + "0"                        +
                   " invoice-line.doc-line     = " + STRING(invoice-line.doc-line) +
                   " invoice-line.seq-no       = " + STRING(invoice-line.seq-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign invoice-line.document = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvoiceLnS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvoiceLnS Procedure 
PROCEDURE ipFixInvoiceLnS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-invoice-ln-s FOR invoice-ln-s.

    for each invoice-ln-s where invoice-ln-s.document = ? exclusive-lock:
        IF CAN-FIND(b-invoice-ln-s WHERE
                    b-invoice-ln-s.Ar-entity  = invoice-ln-s.Ar-entity AND
                    b-invoice-ln-s.document   = 0                      AND
                    b-invoice-ln-s.doc-line   = invoice-ln-s.doc-line  AND
                    b-invoice-ln-s.serial-no  = invoice-ln-s.serial-no AND
                    b-invoice-ln-s.seq-no     = invoice-ln-s.seq-no)
            THEN DO:
            ASSIGN error-line = "invoice-ln-s already exists with: " +
                   " invoice-ln-s.Ar-entity    = " + invoice-ln-s.Ar-entity        +
                   " invoice-ln-s.document     = " + "0"                           +
                   " invoice-ln-s.doc-line     = " + STRING(invoice-ln-s.doc-line) +
                   " invoice-ln-s.serial-no    = " + invoice-ln-s.serial-no        +
                   " invoice-ln-s.seq-no       = " + STRING(invoice-ln-s.seq-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign invoice-ln-s.document = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvoiceRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvoiceRef Procedure 
PROCEDURE ipFixInvoiceRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each invoice where invoice.reference = ? exclusive-lock:
 assign invoice.reference = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvTfr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvTfr Procedure 
PROCEDURE ipFixInvTfr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-inv-tfr FOR inv-tfr.

    for each inv-tfr where inv-tfr.reference = ? exclusive-lock:
        IF CAN-FIND(b-inv-tfr WHERE
            b-inv-tfr.Reference = " " AND
            b-inv-tfr.Seq-no    = inv-tfr.Seq-no)
            THEN DO:
            ASSIGN error-line = "inv-tfr already exists with: " +
                " inv-tfr.Reference = " + "0" +
                " inv-tfr.Seq-no    = " + string(inv-tfr.Seq-no).
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
            ELSE
                assign inv-tfr.reference = " ".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvTfrH) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvTfrH Procedure 
PROCEDURE ipFixInvTfrH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-inv-tfrh FOR inv-tfrh.

    for each inv-tfrh where inv-tfrh.reference = ? exclusive-lock:
        IF CAN-FIND(b-inv-tfrh WHERE
            b-inv-tfrh.Reference = " ")
            THEN DO:
            ASSIGN error-line = "inv-tfrh already exists with: " +
                " inv-tfr.Reference = " + " " .
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
            ELSE
                assign inv-tfrh.reference = " ".
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixInvTfrS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixInvTfrS Procedure 
PROCEDURE ipFixInvTfrS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each inv-tfr-s where inv-tfr-s.reference = ? exclusive-lock:
  assign inv-tfr-s.reference = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixItemVendInq) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixItemVendInq Procedure 
PROCEDURE ipFixItemVendInq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FOR EACH z_menu WHERE z_menu.funct-name = "item.vend.inf" EXCLUSIVE-LOCK:
       ASSIGN z_menu.funct-name = "item.vend.inq".
   END.
   FOR EACH z_menu WHERE z_menu.funct-name = "item.vend.spec" EXCLUSIVE-LOCK:
       ASSIGN z_menu.funct-name = "item.ven.s.inq".
   END.

   FOR EACH z_help WHERE z_help.funct-name = "item.vend.inf" EXCLUSIVE-LOCK:
       ASSIGN z_help.funct-name = "item.vend.inq".
   END.
   FOR EACH z_help WHERE z_help.funct-name = "item.vend.spec" EXCLUSIVE-LOCK:
       ASSIGN z_help.funct-name = "item.ven.s.inq".
   END.
   FOR EACH z_funct WHERE z_funct.funct-name = "item.vend.inf" EXCLUSIVE-LOCK:
          ASSIGN z_funct.funct-name = "item.vend.inq".
      END.
  FOR EACH z_funct WHERE z_funct.funct-name = "item.vend.spec" EXCLUSIVE-LOCK:
      ASSIGN z_funct.funct-name = "item.ven.s.inq".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixPOLineZmemo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixPOLineZmemo Procedure 
PROCEDURE ipFixPOLineZmemo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var vKeyList like z_memo.key-list no-undo.
def buffer bz_memo for z_memo.

    for each z_memo where z_memo.file-name = "PO-LINE":U and
        length(entry(3,z_memo.key-list)) <= 2 exclusive-lock:
        assign vKeyList = entry(1,z_memo.key-list) + ",":U +
            entry(2,z_memo.key-list) + ",":U +
            string(integer(entry(3,z_memo.key-list)),"999") + ",":U +
            entry(4,z_memo.key-list) + ",":U.  
        find last bz_memo where bz_memo.file-name = "PO-LINE":U and
            bz_memo.key-list = vkeylist
            no-lock no-error.
        if  avail bz_memo then
            assign z_memo.page-no = bz_memo.page-no + 1. 
        .
        assign z_memo.key-list = vKeyList.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixPOZmemo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixPOZmemo Procedure 
PROCEDURE ipFixPOZmemo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var vKeyList like z_memo.key-list no-undo.
def buffer bz_memo for z_memo.

for each z_memo where z_memo.file-name =  "PO":U and 
    length(entry(3,z_memo.key-list)) <= 2 exclusive-lock:
    assign vKeyList = entry(1,z_memo.key-list) + ",":U +
        entry(2,z_memo.key-list) + ",":U +
        string(integer(entry(3,z_memo.key-list)),"999") + ",":U  .
    find last bz_memo where bz_memo.file-name = "PO":U and
        bz_memo.key-list = vkeylist
        no-lock no-error.
    if  avail bz_memo then
        assign z_memo.page-no = bz_memo.page-no + 1. 
    assign z_memo.key-list = vKeyList.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixPrintToScreen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixPrintToScreen Procedure 
PROCEDURE ipFixPrintToScreen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST z_print WHERE
        z_print.PRINTER-NAME = "screen" NO-ERROR.
        
    IF AVAIL z_print THEN ASSIGN
        z_print.output-spec = "to <userid>.txt page-size 54 <close>"
        z_print.end-proc = "etc/view.r".
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixSalesHist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixSalesHist Procedure 
PROCEDURE ipFixSalesHist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each sales-hist where sales-hist.document = ? exclusive-lock:
 assign sales-hist.document = 0.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixSalesJrnlDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixSalesJrnlDoc Procedure 
PROCEDURE ipFixSalesJrnlDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-sales-jrnl FOR sales-jrnl.

    for each sales-jrnl where sales-jrnl.document = ? exclusive-lock:
        IF CAN-FIND(b-sales-jrnl WHERE
                    b-sales-jrnl.Ar-entity    = sales-jrnl.Ar-entity    AND
                    b-sales-jrnl.trans-code   = sales-jrnl.trans-code   AND
                    b-sales-jrnl.cust-no      = sales-jrnl.cust-no      AND
                    b-sales-jrnl.document     = 0                       AND
                    b-sales-jrnl.seq-no       = sales-jrnl.seq-no       
                                            )
            THEN DO:
            ASSIGN error-line = "sales-jrnl already exists with: " +
                   " sales-jrnl.Ar-entity    = " + sales-jrnl.Ar-entity  +
                   " sales-jrnl.trans-code   = " + sales-jrnl.trans-code +
                   " sales-jrnl.cust-no      = " + sales-jrnl.cust-no    +
                   " sales-jrnl.seq-no   =     " + STRING(sales-jrnl.seq-no)   +
                   " sales-jrnl.document    = " + "0"                       .
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign sales-jrnl.document = 0.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixSalesJrnll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixSalesJrnll Procedure 
PROCEDURE ipFixSalesJrnll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER b-sales-jrnl1 FOR sales-jrnl1.

    for each sales-jrnl1 where sales-jrnl1.document = ? exclusive-lock:
        IF CAN-FIND(b-sales-jrnl1 WHERE
                    b-sales-jrnl1.Ar-entity    = sales-jrnl1.Ar-entity    AND
                    b-sales-jrnl1.trans-code   = sales-jrnl1.trans-code   AND
                    b-sales-jrnl1.cust-no      = sales-jrnl1.cust-no      AND
                    b-sales-jrnl1.document     = 0                       AND
                    b-sales-jrnl1.seq-no       = sales-jrnl1.seq-no       
                                            )
            THEN DO:
            ASSIGN error-line = "sales-jrnl1 already exists with: " +
                   " sales-jrnl1.Ar-entity    = " + sales-jrnl1.Ar-entity  +
                   " sales-jrnl1.trans-code   = " + sales-jrnl1.trans-code +
                   " sales-jrnl1.cust-no      = " + sales-jrnl1.cust-no    +
                   " sales-jrnl1.seq-no   =     " + STRING(sales-jrnl1.seq-no)   +
                   " sales-jrnl1.document    = " + "0"                       .
            OUTPUT TO "FSIFIX-Err.txt" APPEND.
            PUT error-line.
            OUTPUT CLOSE.
        END.
        ELSE
            assign sales-jrnl1.document = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixSalesJrnlRef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixSalesJrnlRef Procedure 
PROCEDURE ipFixSalesJrnlRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each sales-jrnl where sales-jrnl.reference = ? exclusive-lock:
 assign sales-jrnl.reference = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZField Procedure 
PROCEDURE ipFixZField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each z_field where z_field.file-name = "edi-func-key" exclusive-lock:
    if z_field.field-name = "Return-value" then delete z_field.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZFunct) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZFunct Procedure 
PROCEDURE ipFixZFunct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each z_funct where z_funct.funct-name = "rem.dir.lock"
                    or z_funct.funct-name = "rest.edi.data"
                 exclusive-lock:

    /* DELETE CROSS-REFERENCE */
    for each z_module where z_module.funct-name = z_funct.funct-name
                      exclusive-lock:
        delete z_module.
    end.

    /* ADJUST PASSWORDS */
    for each z_pass where z_pass.funct-name = z_funct.funct-name
                    exclusive-lock:
        if z_pass.field-name = "" then do:
           if z_funct.funct-name = "rest.edi.data"
              then z_pass.funct-name = "edi.file.mgr".
              else z_pass.funct-name = "rem.edi.lock".
        end.
        else delete z_pass.
    end.

    /* DELETE STACKERS */
    for each z_stack where z_stack.funct-name = z_funct.funct-name
                     exclusive-lock:
        delete z_stack.
    end.

    /* DELETE ACTION MESSAGES */
    for each z_action where z_action.funct-name = z_funct.funct-name
                      exclusive-lock:
        delete z_action.
    end.

    /* DELETE MAIL */
    for each z_mail where z_mail.funct-name = z_funct.funct-name
                    exclusive-lock:
        delete z_mail.
    end.

    
    /* DELETE HELP */
    for each z_help where z_help.funct-name = z_funct.funct-name
                    exclusive-lock:
        delete z_help.
    end.

    delete z_funct.

end. /* FOR EACH Z_FUNCT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZMenu Procedure 
PROCEDURE ipFixZMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each z_menu where z_menu.menu-name = "edi.overview"
                  and z_menu.pos       = 3
                exclusive-lock:
    delete z_menu.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZParam Procedure 
PROCEDURE ipFixZParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each z_param where z_param.param-file = "edi/acknlst.y"
                 exclusive-lock:
    if z_param.param-order = 10 or
       z_param.param-order = 20 or
       z_param.param-order = 30 or
       z_param.param-order = 40
       then delete z_param.
end.

for each z_param where z_param.param-file = "edi/acknprg.y"
                 exclusive-lock:
    if z_param.param-order = 10 or
       z_param.param-order = 20 or
       z_param.param-order = 30 or
       z_param.param-order = 40
       then delete z_param.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZparam2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZparam2 Procedure 
PROCEDURE ipFixZparam2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each z_param where param-file = "ca/camrrr.y" exclusive-lock:
 if Field-name = "to-terr" or field-name = "sum-det"
  then delete z_param.
 end.
for each z_param where param-file = "pt/pxref.y" exclusive-lock:
 if field-name = "do-pxref"
   then delete z_param.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZparam3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZparam3 Procedure 
PROCEDURE ipFixZparam3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


FIND  z_param WHERE param-file = "pt/helplst.y":U AND Param-order = 10
     EXCLUSIVE-LOCK NO-ERROR.
ASSIGN z_param.Field-name = "from-user":U
     z_param.Phelp  = "Enter user name or leave blank for all users"
    z_param.Pinitial  = ""
    z_param.Plabel   = "From User".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipFixZUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipFixZUser Procedure 
PROCEDURE ipFixZUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each z_user WHERE z_user.see-login = ? exclusive-lock:
   assign z_user.see-login = yes
          z_user.auto-login = yes.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipHomeCurr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipHomeCurr Procedure 
PROCEDURE ipHomeCurr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH po-control:
    FIND gl-control WHERE
        gl-control.gl-entity = po-control.ap-entity NO-LOCK NO-ERROR.
    IF AVAIL gl-control THEN
        ASSIGN po-control.home-curr = gl-control.home-curr.
END.
FOR EACH op-control:
    FIND gl-control WHERE
        gl-control.gl-entity = op-control.ap-entity NO-LOCK NO-ERROR.
    IF AVAIL gl-control THEN
        ASSIGN op-control.home-curr = gl-control.home-curr.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipInitMemoTypeDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInitMemoTypeDef Procedure 
PROCEDURE ipInitMemoTypeDef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH codes WHERE codes.code-type = "METY":U EXCLUSIVE-LOCK:
    IF codes.chr-1 = "" THEN ASSIGN CODEs.chr-1 = "NO":U.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipInitOrderStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInitOrderStatus Procedure 
PROCEDURE ipInitOrderStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR order-line-rowid AS ROWID.
    DEF VAR order-rowid AS ROWID.
    DEF VAR new-status AS CHAR.
    FOR EACH order WHERE 
        order.order-status = "" OR 
        order.order-status = ? EXCLUSIVE-LOCK:
        
     order.order-status = "Open".
        
      /*  IF order.pps-no > 0 THEN order.order-status = "Picked".
        
        IF order.invoice-no > 0 THEN order.order-status = "Invoiced".
        */
        FOR EACH order-line WHERE 
            order-line.ar-entity = order.ar-entity AND 
            order-line.order-no = order.order-no NO-LOCK:
            
               
       FIND FIRST ord-lin-shpmt WHERE 
           ord-lin-shpmt.ar-entity = order-line.ar-entity AND 
           ord-lin-shpmt.order-no = order-line.order-no AND 
           ord-lin-shpmt.line-no = order-line.line-no
           NO-LOCK NO-ERROR.
        FIND FIRST pps-line WHERE 
         pps-line.ar-entity = order-line.ar-entity AND 
         pps-line.order-no = order-line.order-no AND 
         pps-line.line-no = order-line.line-no
         NO-LOCK NO-ERROR.

     IF AVAIL pps-line THEN DO:
         FIND FIRST ship-pkgl WHERE 
             ship-pkgl.ar-entity = pps-line.ar-entity AND 
             ship-pkgl.pps-no = pps-line.pps-no AND 
             ship-pkgl.pps-line-no = pps-line.pps-line-no
             NO-LOCK NO-ERROR.
         IF AVAIL ship-pkgl THEN 
             FIND ship-pkg OF ship-pkgl 
             NO-LOCK NO-ERROR.
     END.


            IF NOT AVAIL ord-lin-shpmt THEN DO:
                 CREATE ord-lin-shpmt.
                ASSIGN
                    ord-lin-shpmt.Ar-entity  = order-line.ar-entity
                    ord-lin-shpmt.Box-of-order = IF AVAIL ship-pkgl THEN ship-pkgl.box-of-order ELSE 0
                    ord-lin-shpmt.Cust-no = order.cust-no 
                    ord-lin-shpmt.Line-no = order-line.line-no
                    ord-lin-shpmt.Order-no = order-line.order-no
                    ord-lin-shpmt.Package-no = IF AVAIL ship-pkgl THEN ship-pkgl.package-no ELSE 0
                    ord-lin-shpmt.Pallet-no = IF AVAIL ship-pkgl THEN "1" ELSE "0"
                    ord-lin-shpmt.Pps-line-no = IF AVAIL pps-line THEN pps-line.pps-line-no ELSE 0
                    ord-lin-shpmt.Pps-no = IF AVAIL pps-line THEN pps-line.pps-no ELSE 0
                    ord-lin-shpmt.Promise-date = order-line.promise-date
                    ord-lin-shpmt.Qty-allocated = IF AVAIL pps-line THEN pps-line.qty-on-pps ELSE order-line.qty-alloc
                    ord-lin-shpmt.Qty-Promised = IF AVAIL pps-line THEN pps-line.qty-on-pps ELSE order-line.qty-open-ord
                    ord-lin-shpmt.Qty-shipped = IF AVAIL ship-pkgl THEN ship-pkgl.qty-picked ELSE order-line.qty-shipped
                    ord-lin-shpmt.Serial-no = ""
                    ord-lin-shpmt.ship-date = IF AVAIL ship-pkg THEN ship-pkg.ship-date ELSE ?
                    ord-lin-shpmt.Ship-TO = order.ship-no
                    ord-lin-shpmt.Tracer-no = ""
                    ord-lin-shpmt.truck-no = ""
                    ord-lin-shpmt.Via-code = order.via-code.
                    ord-lin-shpmt.c-allocated = calcQuant(ord-lin-shpmt.qty-allocated, " ", " ", order-line.selling-uom).
                    ord-lin-shpmt.c-promised = calcQuant(ord-lin-shpmt.qty-promised, " ", " ", order-line.selling-uom).
                    ord-lin-shpmt.c-shipped = calcQuant(ord-lin-shpmt.qty-shipped," ", " ", order-line.selling-uom).
                    ASSIGN   ord-lin-shpmt.filler-1 = order-line.selling-uom
                        ord-lin-shpmt.filler-2 = order-line.in-entity
                        ord-lin-shpmt.filler-3 = order-line.item-no
                        ord-lin-shpmt.filler-4 = calcQuant(order-line.qty-open-ord, " " ," ", order-line.selling-uom).
               
            END.
            FOR EACH ord-lin-shpmt WHERE  ord-lin-shpmt.ar-entity = order-line.ar-entity AND 
                ord-lin-shpmt.order-no = order-line.order-no AND 
                ord-lin-shpmt.line-no = order-line.line-no EXCLUSIVE-LOCK:
                IF LOOKUP(ord-lin-shpmt.order-status,"Allocated,B/O,Invoiced,NEW,Open,Packed,Partial,Picked,PPS,Shipped") = 0 THEN 
                    NEXT.
   
                FIND FIRST pps-line WHERE 
                    pps-line.ar-entity = order-line.ar-entity AND 
                    pps-line.order-no = order-line.order-no AND 
                    pps-line.line-no = order-line.line-no AND 
                    pps-line.pps-no = ord-lin-shpmt.pps-no
                    AND pps-line.pps-line-no = ord-lin-shpmt.pps-line-no
                    NO-LOCK NO-ERROR.

                IF AVAIL pps-line THEN DO:
                    FIND FIRST ship-pkgl WHERE 
                        ship-pkgl.ar-entity = pps-line.ar-entity AND 
                        ship-pkgl.pps-no = pps-line.pps-no AND 
                        ship-pkgl.pps-line-no = pps-line.pps-line-no
                        NO-LOCK NO-ERROR.
                    IF AVAIL ship-pkgl THEN 
                        FIND ship-pkg OF ship-pkgl 
                        NO-LOCK NO-ERROR.
                END.
                FIND FIRST invoice-line WHERE invoice-line.ar-entity = order-line.ar-entity
                    AND invoice-line.order-no = order-line.order-no
                    AND invoice-line.line-no = order-line.line-no NO-LOCK NO-ERROR.

               IF ord-lin-shpmt.pps-no = 0 AND ord-lin-shpmt.qty-alloc = 0
                   AND ord-lin-shpmt.qty-shipped = 0 THEN ord-lin-shpmt.order-status = "OPEN":U.
               IF ord-lin-shpmt.pps-no = 0 AND ord-lin-shpmt.qty-shipped = 0
                   AND ord-lin-shpmt.qty-alloc <> 0 THEN ord-lin-shpmt.order-status = "ALLOCATED":U.
               IF ord-lin-shpmt.pps-no > 0 AND ord-lin-shpmt.qty-alloc = 0 AND
                   ord-lin-shpmt.qty-shipped = 0 THEN ord-lin-shpmt.order-status = "PPS":U.
               IF ord-lin-shpmt.pps-no > 0 AND ord-lin-shpmt.qty-alloc = 0
                   AND ord-lin-shpmt.qty-shipped = 0 AND AVAIL pps-line AND pps-line.qty-picked > 0
                    THEN ord-lin-shpmt.order-status = "PICKED":U.
               IF ord-lin-shpmt.pps-no > 0 AND ord-lin-shpmt.qty-alloc = 0
                   AND ord-lin-shpmt.qty-shipped = 0 AND AVAIL pps-line AND
                   pps-line.packl-gen = YES AND AVAIL ship-pkgl AND AVAIL ship-pkg AND 
                   ship-pkg.packed = YES THEN ord-lin-shpmt.order-status = "PACKED":U.
               IF ord-lin-shpmt.pps-no > 0 AND ord-lin-shpmt.qty-alloc = 0
                    AND ord-lin-shpmt.qty-shipped <>  0 AND AVAIL pps-line AND
                    pps-line.shipped = YES THEN ord-lin-shpmt.order-status = "SHIPPED":U.
               IF AVAIL invoice-line THEN ord-lin-shpmt.order-status = "INVOICED":U.

            END.
        END.  /* for each order-line */
        FOR EACH order-line WHERE 
                  order-line.ar-entity = order.ar-entity AND 
               order-line.order-no = order.order-no EXCLUSIVE-LOCK:
              
                ASSIGN 
                   order-line.slsmn-code = order.slsmn-code
                    order-line.net-price = order-line.gross-price.
                RUN update-line-status IN THIS-PROCEDURE(INPUT ROWID(order-line),
                                                         INPUT order-line.order-status,
                                                         OUTPUT new-status).
        END.
           RUN update-order-status IN THIS-PROCEDURE(INPUT ROWID(order),
                                                     INPUT order.order-status,
                                                     OUTPUT new-status).
    END.  /* for each order */
    
    FOR EACH pps-line EXCLUSIVE-LOCK:
        ASSIGN c-qty-on-pps = calcQuant(pps-line.qty-on-pps, " ", " ", pps-line.uom-code).
        ASSIGN c-picked = calcQuant(pps-line.qty-picked, " ", " ", pps-line.uom-code).
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipInvCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipInvCountry Procedure 
PROCEDURE ipInvCountry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   For each invoice where invoice.country = "" exclusive-lock:
       FIND country WHERE 
           country.country-code = invoice.country-code NO-LOCK NO-ERROR.
       IF AVAIL country AND
           country.DESCRIPTION <> "" THEN
           ASSIGN invoice.country = country.DESCRIPTION.
   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipLoadDelta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadDelta Procedure 
PROCEDURE ipLoadDelta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE " About to make changes to database " LDBNAME(1) "." SKIP(2)
            "!! THESE CHANGES CANNOT BE UNDONE !!" SKIP(2)
            "Continue anyway?"
            VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lChoice AS LOG.
    
    IF NOT lChoice THEN RETURN.
    
    IF SEARCH("prodict/load_df.r") <> ? THEN DO: 
        /* Stupidly, the load_df program writes a message; this prevents the ugly grey screen */
        OUTPUT TO nul.
        RUN prodict/load_df.r (INPUT SEARCH("delta.df") + ",yes,NEW OBJECTS").
        OUTPUT CLOSE.
        OS-RENAME 
            VALUE(SEARCH("delta.df")) 
            VALUE("..\DATABASE\defs\delta" + 
                SUBstring(STRING(TODAY),7,2) +
                SUBSTRING(STRING(TODAY),1,2) +
                SUBSTRING(STRING(TODAY),4,2) + ".df").
    END.
    ELSE DO:
        MESSAGE 
            "Unable to find Progress Load File." SKIP
            "Skipping .df load."
            VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipMessType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMessType Procedure 
PROCEDURE ipMessType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH z_mess WHERE mess-id >= "cp0090"
        AND mess-id <= "cp0099" EXCLUSIVE-LOCK:
       IF mess-id = "cp0098" THEN ASSIGN mess-type = 2.
       ELSE ASSIGN mess-type = 4.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipMultiStartEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipMultiStartEnd Procedure 
PROCEDURE ipMultiStartEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH contract EXCLUSIVE-LOCK:
        DO i = 1 to 8:
            IF contract.start-time[i] = ""  THEN contract.start-time[i] = contract.filler1.
            IF contract.end-time[i] = ""    THEN contract.end-time[i] = contract.filler2.
            IF contract.dt-start[i] = ""    THEN contract.dt-start[i] = contract.filler3.
            IF contract.dt-end[i] = ""      THEN contract.dt-end[i] = contract.filler4.
            IF contract.start-time[i] = ""  THEN contract.start-time[i] = "0000".
            IF contract.end-time[i] = ""    THEN contract.end-time[i] = "0000".
            IF contract.dt-start[i] = ""    THEN contract.dt-start[i] = "0000".
            IF contract.dt-end[i] = ""      THEN contract.dt-end[i] = "0000".
        END.  
    END.
    
    FOR EACH contract-lin EXCLUSIVE-LOCK:
        DO i = 1 to 8:
            IF contract-lin.start-time[i] = ""  THEN contract-lin.start-time[i] = contract-lin.filler1.
            IF contract-lin.end-time[i] = ""    THEN contract-lin.end-time[i] = contract-lin.filler2.
            IF contract-lin.dt-start[i] = ""    THEN contract-lin.dt-start[i] = contract-lin.filler3.
            IF contract-lin.dt-end[i] = ""      THEN contract-lin.dt-end[i] = contract-lin.filler4.  
            IF contract-lin.start-time[i] = ""  THEN contract-lin.start-time[i] = "0000".
            IF contract-lin.end-time[i] = ""    THEN  contract-lin.end-time[i] = "0000".
            IF contract-lin.dt-start[i] = ""    THEN  contract-lin.dt-start[i] = "0000".
            IF contract-lin.dt-end[i] = ""      THEN  contract-lin.dt-end[i] = "0000".
        END.  
    END.
    
    FOR EACH service EXCLUSIVE-LOCK:
        DO i = 1 to 8:
            IF service.start-time[i] = ""   THEN  service.start-time[i] = service.filler1.
            IF service.end-time[i] = ""     THEN  service.end-time[i] = service.filler2.
            IF service.dt-start[i] = ""     THEN  service.dt-start[i] = service.filler3.
            IF service.dt-end[i] = ""       THEN  service.dt-end[i] = service.filler4.
            IF service.start-time[i] = ""   THEN  service.start-time[i] = "0000".
            IF service.end-time[i] = ""     THEN  service.end-time[i] = "0000".
            IF service.dt-start[i] = ""     THEN  service.dt-start[i] = "0000".
            IF service.dt-end[i] = ""       THEN  service.dt-end[i] = "0000".
        END. 
        if service.Arrive-time  = "" THEN service.arrive-time = "0000".
        if service.Close-time   = "" THEN service.close-time = "0000".
        if service.Complete-tm  = "" THEN service.complete-tm = "0000".
        if service.Dispatch-tm  = "" THEN service.dispatch-tm = "0000".
        if service.Entry-time   = "" THEN service.Entry-TIME = "0000".
        if service.Escalate-tm  = "" THEN service.escalate-tm = "0000".
        if service.ETA-time     = "" THEN service.eta-time = "0000".
        if service.Notify-time  = "" THEN service.notify-time = "0000".
        if service.Request-time = "" THEN service.request-time = "0000".
        if service.return-time  = "" THEN service.return-time = "0000".
        if service.Tele-time    = "" THEN service.tele-time = "0000".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipOPCtrlToView) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipOPCtrlToView Procedure 
PROCEDURE ipOPCtrlToView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FOR EACH op-control WHERE
       op-control.orders-to-view = "":
       ASSIGN op-control.orders-to-view = "*".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipOrdCountry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipOrdCountry Procedure 
PROCEDURE ipOrdCountry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   For each order where order.country = "" exclusive-lock:
       FIND country WHERE 
           country.country-code = order.country-code NO-LOCK NO-ERROR.
       IF AVAIL country AND
           country.DESCRIPTION <> "" THEN
           ASSIGN order.country = country.DESCRIPTION.
   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipOrderLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipOrderLine Procedure 
PROCEDURE ipOrderLine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer bOL for order-line.
FOR EACH order-line  no-lock:
 if order-line.c-ordered = "" and
        order-line.qty-open-ord <> 0  then do:
     find bOL where rowid(bOL) = rowid(order-line) exclusive-lock no-error.
     ASSIGN bOL.c-ordered = calcQuant(bOL.qty-open-ord, "", "", bOL.uom-code).
  end.
  if order-line.c-ordered = "" and
        order-line.qty-open-ord = 0 then do:
    find bOL where rowid(bOL) = rowid(order-line) exclusive-lock no-error.
    ASSIGN bOL.c-ordered = "0".
  end.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipOrdTelno) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipOrdTelno Procedure 
PROCEDURE ipOrdTelno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   For each order where order.ship-telephone = "" exclusive-lock:
       FIND cust-add WHERE cust-add.cust-no = order.cust-no
           AND cust-add.ship-to = order.ship-no NO-LOCK NO-ERROR.
       IF AVAIL cust-add THEN 
           order.ship-telephone  = cust-add.telephone.
       ELSE DO:
           FIND customer WHERE 
               customer.cust-no = order.ship-no  NO-LOCK NO-ERROR.
           IF AVAIL customer THEN 
               ASSIGN order.ship-telephone  = customer.telephone.
       END.
   END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipPostoview) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPostoview Procedure 
PROCEDURE ipPostoview :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH po-control WHERE po-control.pos-to-view = "" EXCLUSIVE-LOCK:
    ASSIGN po-control.pos-to-view = "*".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipRegenGUIMenus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRegenGUIMenus Procedure 
PROCEDURE ipRegenGUIMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vct AS INT.

FOR EACH z_gmenu EXCLUSIVE:
    DELETE z_gmenu.
END.

CREATE ttMenu.
ASSIGN
    ttMenu.menuname     = "MXP"
    ttMenu.idx          = 0
    ttMenu.parentname   = ""
    ttMenu.parentidx    = 0
    ttMenu.pgmtype      = 1
    ttMenu.menudesc     = "MXP Epsilon Edition"
    ttMenu.menupos      = 1
    ttMenu.sortorder    = 0
    ttMenu.level1       = 1
    ttMenu.level2       = 0
    ttMenu.level3       = 0
    ttMenu.level4       = 0
    ttMenu.level5       = 0
    ttMenu.level6       = 0
    ttMenu.level7       = 0.
                                                              
FOR EACH z_menu WHERE z_menu.pos > 0
    USE-INDEX menu-pos:
        
    /* EXCLUSIONS */
    IF z_menu.menu-name = "mxp" AND z_menu.funct-name = "edi" THEN NEXT.
    IF z_menu.menu-name = "mxp" AND z_menu.funct-name = "wb" THEN NEXT.
    IF z_menu.menu-name = "mxp" AND z_menu.funct-name = "concepts" THEN NEXT.
    IF z_menu.menu-name = "mxp" AND z_menu.funct-name = "appbuild" THEN NEXT.
    IF z_menu.menu-name = "mxp" AND z_menu.funct-name = "editor" THEN NEXT.
    
    IF z_menu.menu-name = "pb" AND z_menu.funct-name = "unsupported" THEN NEXT.
    IF z_menu.menu-name = "pb" AND z_menu.funct-name = "pb.user.fields" THEN NEXT.
    IF z_menu.menu-name = "pb" AND z_menu.funct-name = "exit" THEN NEXT.
    IF z_menu.menu-name = "pb" AND z_menu.funct-name = "quit" THEN NEXT.
    
    IF z_menu.menu-name = "hd" AND z_menu.funct-name = "quit" THEN NEXT.
    
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "gdb" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "sw" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "ff-disk.req" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "help" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "help.inq" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "menu.inq" THEN NEXT.
    IF z_menu.menu-name = "ff" AND z_menu.funct-name = "quit" THEN NEXT.
    
    IF z_menu.funct-name BEGINS "edi" THEN NEXT.
    
    IF z_menu.funct-name = "wb.maintenance" THEN NEXT.
    IF z_menu.menu-name = "wb.maintenance" THEN NEXT.
    
    FIND FIRST z_funct WHERE
        z_funct.funct-name = z_menu.funct-name NO-LOCK NO-ERROR.
    
    IF AVAIL z_funct AND 
        z_funct.gui-tty <> "tty only" THEN DO:
        CREATE ttMenu.
        ASSIGN
            ttMenu.menuname     = z_menu.funct-name
            ttMenu.idx          = 0
            ttMenu.parentname   = z_menu.menu-name
            ttMenu.parentidx    = 0
            ttMenu.pgmtype      = (IF NOT AVAIL z_funct THEN 1 ELSE
                   IF z_funct.TYPE = "m" THEN 1 ELSE
                   IF z_funct.TYPE = "re" THEN 3 ELSE
                   IF (z_funct.TYPE = "t" OR 
                       INDEX("inq",z_funct.funct-name) <> 0 OR
                       INDEX("inquiry",z_funct.funct-desc) <> 0 OR
                       z_funct.runpgm BEGINS "br/") THEN 5 ELSE
                   4)
            ttMenu.menudesc     = z_funct.funct-desc
            ttMenu.runpgm       = IF z_funct.grunpgm <> "" THEN z_funct.grunpgm ELSE z_funct.runpgm
            ttMenu.menupos      = z_menu.pos
            ttMenu.sortorder    = IF z_funct.TYPE = "m" THEN 1 ELSE
                                  IF z_funct.TYPE = "ru" THEN
                                    (IF INDEX("inq",z_funct.funct-name) <> 0 OR
                                    INDEX("inquiry",z_funct.funct-desc) <> 0 OR
                                    z_funct.runpgm BEGINS "br/" THEN 3 ELSE 2) ELSE
                                  IF z_funct.TYPE = "re" THEN 4 ELSE
                                  IF z_funct.TYPE = "t" THEN 5 ELSE 6
            ttMenu.level1       = 0
            ttMenu.level2       = 0
            ttMenu.level3       = 0
            ttMenu.level4       = 0
            ttMenu.level5       = 0
            ttMenu.level6       = 0
            ttMenu.level7       = 0
            .
    END.
END.

FOR EACH ttMenu WHERE ttMenu.parentname = "MXP" USE-INDEX iparentname:
    vct = vct + 1.
    ASSIGN 
        ttMenu.level1 = 1
        ttMenu.level2 = vct.
END.

FOR EACH ttMenu WHERE ttMenu.level2 > 0 
    BY ttMenu.level2 BY ttMenu.menupos:
    vct = 0.
    FOR EACH bttMenu WHERE bttMenu.parentname = ttMenu.menuname:
        vct = vct + 1.
        ASSIGN 
            bttMenu.level1 = ttMenu.level1
            bttMenu.level2 = ttMenu.level2
            bttMenu.level3 = vct.
    END.
END.

FOR EACH ttMenu WHERE ttMenu.level3 > 0
    BY ttMenu.level2 BY ttMenu.level3 BY ttMenu.menupos:
    vct = 0.
    FOR EACH bttMenu WHERE bttMenu.parentname = ttMenu.menuname:
        vct = vct + 1.
        ASSIGN 
            bttMenu.level1 = ttMenu.level1
            bttMenu.level2 = ttMenu.level2
            bttMenu.level3 = ttMenu.level3
            bttMenu.level4 = vct.
    END.
END.

FOR EACH ttMenu WHERE ttMenu.level4 > 0
    BY ttMenu.level2 BY ttMenu.level3 BY ttMenu.level4 BY ttMenu.menupos:
    vct = 0.
    FOR EACH bttMenu WHERE bttMenu.parentname = ttMenu.menuname:
        vct = vct + 1.
        ASSIGN 
            bttMenu.level1 = ttMenu.level1
            bttMenu.level2 = ttMenu.level2
            bttMenu.level3 = ttMenu.level3
            bttMenu.level4 = ttMenu.level4
            bttMenu.level5 = vct.
    END.
END.

FOR EACH ttMenu WHERE ttMenu.level5 > 0
    BY ttMenu.level2 BY ttMenu.level3 BY ttMenu.level4 BY ttMenu.level5 BY ttMenu.menupos:
    vct = 0.
    FOR EACH bttMenu WHERE bttMenu.parentname = ttMenu.menuname:
        vct = vct + 1.
        ASSIGN 
            bttMenu.level1 = ttMenu.level1
            bttMenu.level2 = ttMenu.level2
            bttMenu.level3 = ttMenu.level3
            bttMenu.level4 = ttMenu.level4
            bttMenu.level5 = ttMenu.level5
            bttMenu.level6 = vct.
    END.
END.

ASSIGN vct = 0.
FOR EACH ttMenu USE-INDEX iSortOrder:
    
    IF ttMenu.level2 > 0 THEN DO:
        ASSIGN vct = vct + 1.
        CREATE z_gmenu.
        ASSIGN
            z_gmenu.menu-name = ttMenu.menuname
            z_gmenu.tr-level  = (IF ttMenu.level6 > 0 THEN 5 ELSE
                                IF ttMenu.level5 > 0 THEN 4 ELSE
                                IF ttMenu.level4 > 0 THEN 3 ELSE
                                IF ttMenu.level3 > 0 THEN 2 ELSE
                                IF ttMenu.level2 > 0 THEN 1 ELSE 0)
            z_gmenu.picture-no = ttMenu.pgmtype
            z_gmenu.srt-order = vct
            z_gmenu.menu-desc = ttMenu.menudesc
            z_gmenu.menu-parent = ttMenu.parentname.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipRegenMenus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRegenMenus Procedure 
PROCEDURE ipRegenMenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH bzmenu WHERE 
        bzmenu.pos      = 0 AND
        bzmenu.user-id  = "" NO-LOCK:
        
        menuname = bzmenu.menu-name.
        isempty = TRUE.
        PAUSE 0.
    
        IF isempty THEN DO:
            FIND z_image WHERE z_image.menu-name = menuname EXCLUSIVE NO-ERROR.
            IF AVAILable z_image THEN DELETE z_image.
        END.
    
        FOR EACH z_menu WHERE z_menu.menu-name = menuname EXCLUSIVE-LOCK:
            IF z_menu.funct-name = "blank" THEN DO:
                FIND FIRST buf-menu WHERE buf-menu.menu-name = z_menu.menu-name AND
                    buf-menu.Pos       = z_menu.pos AND
                    buf-menu.funct-name <> "blank" NO-LOCK NO-ERROR.
                IF AVAILable buf-menu THEN DO:
                    DELETE z_menu.           
                END.    
            END.
        END.    
    
        FOR EACH z_menu WHERE z_menu.menu-name = menuname NO-LOCK:
            FIND z_funct OF z_menu NO-LOCK NO-ERROR.
                isempty = NO.
            IF z_menu.pos = 0 THEN DO:
                IF AVAIL z_funct AND 
                    z_funct.funct-desc BEGINS ":":U THEN DO:
                    isempty = YES.
                    LEAVE.
                END.
                FIND z_image OF z_menu EXCLUSIVE NO-ERROR.
                IF NOT AVAIL z_image THEN CREATE z_image.
                z_image.menu-name = z_menu.menu-name.
                z_image.menu-desc = IF AVAIL z_funct THEN z_funct.funct-desc ELSE "".
                z_image.left-count = 0.
                z_image.right-count = 0.
                j = 0.
                z_image.entry-list = "".
            END.
            ELSE IF AVAIL z_image THEN DO:
                IF j < 17 AND (z_menu.pos > 49 OR
                            z_image.left-count >= 16) THEN DO:
                    DO j = j + 1 to 16:
                        z_image.entry-desc[j] = "".
                            z_image.type-list[j] = "".
                            z_image.funct-list[j] = "".
                    END.
                    j = 16.
                END.
            
                j = j + 1.
                IF j > 32 THEN DO:
                    NEXT.
                END.
                wpos = STRING(z_menu.pos,">9":U).
                z_image.entry-desc[j] = IF (AVAIL z_funct AND 
                        z_funct.funct-desc BEGINS ":":U)
                        THEN "   ":U +
                            (IF AVAIL z_funct AND z_funct.type = "m":U
                            THEN ""
                            ELSE SUBSTRING(z_funct.funct-desc,2)
                            )
                        ELSE wpos + " ":U + IF AVAIL z_funct THEN 
                        z_funct.funct-desc ELSE "".
                IF wpos BEGINS " ":U THEN wpos = substring(wpos,2).
                z_image.entry-list = z_image.entry-list + wpos + ",":U.
                z_image.funct-list[j] = IF AVAIL z_funct THEN z_funct.funct-name ELSE "".
                z_image.type-list[j]  = IF AVAIL z_funct THEN  z_funct.type ELSE "".
                IF j > 16 THEN z_image.right-count = z_image.right-count + 1.
                    ELSE z_image.left-count = z_image.left-count + 1.
            END.
        END.
        
        IF isempty THEN DO:
            FIND z_image WHERE z_image.menu-name = menuname EXCLUSIVE NO-ERROR.
            IF AVAILable z_image THEN DELETE z_image.
        END.
        ELSE DO j = j + 1 to 32:
            IF AVAIL z_image THEN 
            ASSIGN   z_image.entry-desc[j] = ""
                    z_image.funct-list[j] = ""
                    z_image.type-list[j]  = "".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipRegenSchema) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRegenSchema Procedure 
PROCEDURE ipRegenSchema :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH z_MS_file WHERE 
        z_MS_file._file-name <= "z_zzzzzzzz":U :
    FOR EACH z_MS_index WHERE 
            z_ms_index._file-name = z_ms_file._file-name:
        FOR EACH z_MS_index-field WHERE 
            z_MS_index-field._file-name = z_ms_index._file-name AND 
            z_ms_index-field._index-name = z_ms_index._index-name:
                    DELETE z_MS_index-field.
        END.
            DELETE z_MS_index.
    END.
    FOR EACH z_MS_field WHERE 
            z_ms_field._file-name = z_ms_file._file-name:
            DELETE z_MS_field.
    END.  
        DELETE z_MS_file.
    END.     

    FIND FIRST dictdb._db NO-LOCK.
    
    FOR EACH dictdb._file :
        IF CAN-FIND(FIRST z_MS_File WHERE
            z_MS_file._File-name  = dictdb._file._File-name) THEN NEXT.
        CREATE z_MS_file.
        ASSIGN
            z_MS_file._File-name  = dictdb._file._File-name
            z_MS_file._Frozen     = no
            z_MS_file._db-recid =  RECID(dictdb._Db)
            z_MS_file._Can-Create = dictdb._file._Can-Create
            z_MS_file._desc       = dictdb._File._Desc
            z_MS_file._can-read   = dictdb._file._Can-Read
            z_MS_file._can-write  = dictdb._file._Can-Write
            z_MS_file._can-delete = dictdb._file._Can-delete
            z_MS_file._valexp     = dictdb._File._Valexp
            z_MS_file._valmsg     = dictdb._File._Valmsg
            z_MS_file._hidden     = dictdb._file._hidden
            z_MS_file._dump-name  = dictdb._file._dump-name
            /* _db-lang. */
            z_MS_File._For-Format   = dictdb._File._For-Format
            z_MS_File._For-Owner    = dictdb._File._For-Owner
            z_MS_File._For-number   = dictdb._File._For-number
            z_MS_File._For-Cnt1     = dictdb._File._For-Cnt1
            z_MS_File._For-Cnt2     = dictdb._File._For-Cnt2
            z_MS_File._For-Flag     = dictdb._File._For-Flag
            z_MS_File._For-Info     = dictdb._File._For-Info
            z_MS_File._For-Name     = dictdb._File._For-Name
            z_MS_File._For-Size     = dictdb._File._For-Size
            z_MS_File._For-Type     = dictdb._File._For-Type
            z_MS_File._Fil-misc2[1] = dictdb._File._Fil-misc2[1]
            z_MS_File._Fil-misc2[2] = dictdb._File._Fil-misc2[2]
            z_MS_File._Fil-misc2[3] = dictdb._File._Fil-misc2[3]
            z_MS_File._Fil-misc2[4] = dictdb._File._Fil-misc2[4]
            z_MS_File._Fil-misc2[5] = dictdb._File._Fil-misc2[5]
            z_MS_File._Fil-misc2[6] = dictdb._File._Fil-misc2[6]
            z_MS_File._Fil-misc2[7] = dictdb._File._Fil-misc2[7]
            z_MS_File._Fil-misc2[8] = dictdb._File._Fil-misc2[8]
            z_MS_File._Fil-misc1[1] = dictdb._File._Fil-misc1[1]
            z_MS_File._Fil-misc1[2] = dictdb._File._Fil-misc1[2]
            z_MS_File._Fil-misc1[3] = dictdb._File._Fil-misc1[3]
            z_MS_File._Fil-misc1[4] = dictdb._File._Fil-misc1[4]
            z_MS_File._Fil-misc1[5] = dictdb._File._Fil-misc1[5]
            z_MS_File._Fil-misc1[6] = dictdb._File._Fil-misc1[6]
            z_MS_File._Fil-misc1[7] = dictdb._File._Fil-misc1[7]
            z_MS_File._Fil-misc1[8] = dictdb._File._Fil-misc1[8].
    END.

    FOR EACH dictdb._field OF dictdb._file:
        CREATE z_MS_field.
        ASSIGN
            z_MS_field._file-recid  = recid(z_MS_file)
            z_MS_field._field-name  = dictdb._field._field-name
            z_MS_field._data-type   = dictdb._field._data-type
            z_MS_field._extent      = dictdb._field._extent
            z_MS_field._order       = dictdb._field._order
            z_MS_field._decimals    = dictdb._field._decimals
            z_MS_field._format      = dictdb._field._format
            z_MS_field._initial     = dictdb._field._initial
            z_MS_field._mandatory   = dictdb._field._mandatory
            z_MS_field._can-read    = dictdb._field._can-read
            z_MS_field._can-write   = dictdb._field._can-write
            z_MS_field._desc        = dictdb._field._desc
            z_MS_field._label       = dictdb._field._label
            z_MS_field._valexp      = dictdb._field._valexp
            z_MS_field._valmsg      = dictdb._field._valmsg
            z_MS_field._col-label   = dictdb._field._col-label
            z_MS_field._help        = dictdb._field._help 
            z_MS_Field._Col-label-SA  = dictdb._Field._Col-label-SA
            z_MS_Field._Format-SA     = dictdb._Field._Format-SA
            z_MS_Field._Help-SA       = dictdb._Field._Help-SA
            z_MS_Field._Initial-SA    = dictdb._Field._Initial-SA
            z_MS_Field._Label-SA      = dictdb._Field._Label-SA
            z_MS_Field._Valmsg-SA     = dictdb._Field._Valmsg-SA
            z_MS_Field._View-as       = dictdb._Field._View-as
            z_MS_Field._Fld-case      = dictdb._Field._Fld-case
            z_ms_field._file-name     = dictdb._file._file-name
            z_MS_Field._Fld-stlen     = dictdb._Field._Fld-stlen
            z_MS_Field._Fld-stoff     = dictdb._Field._Fld-stoff
            z_MS_Field._Fld-stdtype   = dictdb._Field._Fld-stdtype
            z_MS_Field._For-Id        = dictdb._Field._For-Id
            z_MS_Field._For-Name      = dictdb._Field._For-Name
            z_MS_Field._For-Type      = dictdb._Field._For-Type
            z_MS_Field._For-Xpos      = dictdb._Field._For-Xpos
            z_MS_Field._For-Itype     = dictdb._Field._For-Itype
            z_MS_Field._For-Retrieve  = dictdb._Field._For-Retrieve
            z_MS_Field._For-Scale     = dictdb._Field._For-Scale
            z_MS_Field._For-Spacing   = dictdb._Field._For-Spacing
            z_MS_Field._For-Separator = dictdb._Field._For-Separator
            z_MS_Field._For-Allocated = dictdb._Field._For-Allocated
            z_MS_Field._For-Maxsize   = dictdb._Field._For-Maxsize
            z_MS_Field._Fld-misc2[1]  = dictdb._Field._Fld-misc2[1]
            z_MS_Field._Fld-misc2[2]  = dictdb._Field._Fld-misc2[2]
            z_MS_Field._Fld-misc2[3]  = dictdb._Field._Fld-misc2[3]
            z_MS_Field._Fld-misc2[4]  = dictdb._Field._Fld-misc2[4]
            z_MS_Field._Fld-misc2[5]  = dictdb._Field._Fld-misc2[5]
            z_MS_Field._Fld-misc2[6]  = dictdb._Field._Fld-misc2[6]
            z_MS_Field._Fld-misc2[7]  = dictdb._Field._Fld-misc2[7]
            z_MS_Field._Fld-misc2[8]  = dictdb._Field._Fld-misc2[8]
            z_MS_Field._Fld-misc1[1]  = dictdb._Field._Fld-misc1[1]
            z_MS_Field._Fld-misc1[2]  = dictdb._Field._Fld-misc1[2]
            z_MS_Field._Fld-misc1[3]  = dictdb._Field._Fld-misc1[3]
            z_MS_Field._Fld-misc1[4]  = dictdb._Field._Fld-misc1[4]
            z_MS_Field._Fld-misc1[5]  = dictdb._Field._Fld-misc1[5]
            z_MS_Field._Fld-misc1[6]  = dictdb._Field._Fld-misc1[6]
            z_MS_Field._Fld-misc1[7]  = dictdb._Field._Fld-misc1[7]
            z_MS_Field._Fld-misc1[8]  = dictdb._Field._Fld-misc1[8].
    END.

    FOR EACH dictdb._index of dictdb._file:
        CREATE z_MS_index.
        ASSIGN
            z_MS_index._file-recid  = recid(z_MS_file)
            z_MS_index._index-name  = dictdb._index._index-name
            z_MS_index._unique      = dictdb._index._unique
            z_MS_index._active      = dictdb._index._active
            z_MS_index._I-misc1[1]  = dictdb._index._I-misc1[1]
            z_MS_index._Desc        = dictdb._index._Desc
            z_MS_index._Wordidx     = dictdb._index._Wordidx
            /* z_MS_index._Idx-num     = dictdb._index._Idx-num   */
            z_MS_index._For-name    = dictdb._index._For-name
            z_MS_index._I-misc2[1]  = dictdb._index._I-misc2[1]
            z_ms_index._file-name   = dictdb._file._file-name.

        FOR EACH dictdb._index-field OF dictdb._index:
            FIND dictdb._field WHERE 
                RECID(dictdb._field) = dictdb._index-field._field-recid NO-LOCK.
            FIND z_MS_field WHERE
                z_MS_field._file-recid  = RECID(z_MS_file) AND
                z_MS_field._field-name  = dictdb._field._field-name NO-LOCK.

            CREATE z_MS_index-field.
            ASSIGN
                z_MS_index-field._Index-recid  = RECID(z_MS_index)
                z_MS_index-field._field-recid  = RECID(z_MS_field)
                z_MS_index-field._index-seq    = dictdb._index-field._index-seq
                z_MS_index-field._ascending    = dictdb._index-field._ascending
                z_MS_index-field._abbreviate   = dictdb._index-field._abbreviate.
                z_MS_index-field._Unsorted     = dictdb._index-field._Unsorted.
                z_Ms_index-field._file-name = dictdb._file._file-name.
                z_ms_index-field._index-name = dictdb._index._index-name.
                z_ms_index-field._field-name = dictdb._field._field-name.
        END.
    END.

    FOR EACH z_MS_file:
        FIND dictdb._file WHERE
            dictdb._file._File-name = z_MS_file._File-name
            NO-LOCK NO-ERROR.
            
        IF AVAIL dictdb._file THEN FIND dictdb._index WHERE 
            RECID(dictdb._index) = dictdb._file._prime-index 
            NO-LOCK NO-ERROR.

        IF AVAIL dictdb._index THEN DO:
            FIND z_MS_index WHERE 
                z_MS_index._file-recid = RECID(z_MS_file) AND   
                z_MS_index._index-name = dictdb._index._index-name
                NO-LOCK NO-ERROR.
            IF AVAIL z_MS_index THEN ASSIGN
                z_MS_file._prime-index = RECID(z_MS_index).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipRemoveTpmTqm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRemoveTpmTqm Procedure 
PROCEDURE ipRemoveTpmTqm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH z_menu WHERE z_menu.menu-name BEGINS "t" EXCLUSIVE-LOCK:
        DELETE z_menu.
    END.
    
    FOR EACH z_image WHERE z_image.menu-name BEGINS "t" EXCLUSIVE-LOCK:
        DELETE z_image.
    END.

    FOR EACH z_funct WHERE 
        z_funct.reqd-logon MATCHES "*tpm*" OR
        z_funct.reqd-logon MATCHES "*tqm*" OR
        z_funct.reqd-logon = "tpm" OR
        z_funct.reqd-logon = "tqm" OR
        z_funct.runpgm BEGINS "tqm" OR
        z_funct.runpgm BEGINS "tpm" OR
        z_funct.funct-name BEGINS "b.tq" OR
        z_funct.funct-name BEGINS "b.tp" OR
        z_funct.funct-name BEGINS "d.tq" OR
        z_funct.funct-name BEGINS "d.tp" OR
        z_funct.funct-name BEGINS "l.tq" OR
        z_funct.funct-name BEGINS "l.tp" OR
        z_funct.funct-name MATCHES "*tqm*" OR
        z_funct.funct-name MATCHES "*tpm*" OR
        z_funct.funct-desc MATCHES  "*tpm*" OR
        z_funct.doc-text BEGINS "tpm" OR
        z_funct.param-file BEGINS "tpm" OR
        z_funct.funct-desc MATCHES  "*tqm*" OR
        z_funct.doc-text BEGINS "tqm" OR
        z_funct.param-file BEGINS "tqm"
        EXCLUSIVE-LOCK:
        FOR EACH z_menu WHERE z_menu.funct-name = z_funct.funct-name EXCLUSIVE-LOCK:
            DELETE z_menu.
        END.
        DELETE z_funct.
    END.
    
    FOR EACH z_param WHERE 
        z_param.param-file BEGINS "tqm" OR
        z_param.param-file BEGINS "tqm" EXCLUSIVE:
        DELETE z_param.  
    END.
    
    FOR EACH z_mess WHERE 
        z_mess.mess-id BEGINS "tp" OR
        z_mess.mess-id BEGINS "tq" EXCLUSIVE:
        DELETE z_mess.  
    END.
    
    FOR EACH z_help WHERE 
        z_help.funct-name BEGINS "tqm" OR
        z_help.funct-name BEGINS "tqm" EXCLUSIVE:
    END.
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipRenameCompiler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipRenameCompiler Procedure 
PROCEDURE ipRenameCompiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST z_funct WHERE z_funct.funct-name = "xref.utility" NO-LOCK NO-ERROR.
    IF AVAIL z_funct THEN RETURN.

    FOR EACH z_funct WHERE z_funct.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_funct.funct-name = "xref.utility":U
            z_funct.funct-desc = "XREF/Utility Program":U
            z_funct.zoom-descr = "XREF/Utility Program":U.
    END.
    
    FOR EACH z_menu WHERE z_menu.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_menu.funct-name = "xref.utility":U
            z_menu.function-name = "XREF/Utility Program":U.
    END.
    
    FOR EACH z_gmenu WHERE z_gmenu.menu-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_gmenu.menu-name = "xref.utility":U
            z_gmenu.menu-desc = "XREF/Utility Program":U.
    END.
    
    FOR EACH z_action WHERE z_action.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_action.funct-name = "xref.utility":U.
    END.
        
    FOR EACH z_help WHERE z_help.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_help.funct-name = "xref.utility":U.
    END.
    
    FOR EACH z_image WHERE z_image.menu-name = "pb.support":U EXCLUSIVE-LOCK:
        DO i = 1 to 32:
            IF z_image.funct-list[i] = "compiler.new":U THEN ASSIGN 
                z_image.funct-list[i] = "xref.utility":U
                z_image.entry-desc[i] = "XREF/Utility Program":U.
        END.
    END.  /* z_image */
    
    FOR EACH z_key WHERE z_key.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_key.funct-name = "xref.utility":U.
    END.
    
    FOR EACH z_module WHERE z_module.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_module.funct-name = "xref.utility":U.
    END.
    
    FOR EACH z_pass WHERE z_pass.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_pass.funct-name = "xref.utility":U.
    END.
    
    FOR EACH z_stack WHERE z_stack.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_stack.funct-name = "xref.utility":U.
    END.
    
    FOR EACH z_webfunc WHERE z_webfunc.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_webfunc.funct-name = "xref.utility":U
            z_webfunc.funct-desc = "XREF/Utility Program":U.
    END.
    
    FOR EACH z_webmenu WHERE z_webmenu.funct-name = "compiler.new":U EXCLUSIVE-LOCK:
        ASSIGN 
            z_webmenu.funct-name = "xref.utility":U.
    END.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipSerialSortName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSerialSortName Procedure 
PROCEDURE ipSerialSortName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    set the new sort-name field for serial records.  
------------------------------------------------------------------------------*/
FOR EACH serial WHERE cust-no <> "" EXCLUSIVE-LOCK:
    FIND customer WHERE customer.cust-no = serial.cust-no NO-LOCK NO-ERROR.
    ASSIGN serial.sort-name = IF AVAIL customer THEN customer.sort-name ELSE "".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipServicePrd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipServicePrd Procedure 
PROCEDURE ipServicePrd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF VAR done AS LOG. */
/*                      */
/*   done = NO.         */
    FOR EACH service WHERE 
        service.prd = 00 OR 
        service.prd = ? OR
        service.yr = 0 OR 
        service.yr = ?:
        FOR EACH gl-ctrl-yr WHERE 
            gl-ctrl-yr.gl-entity = service.ar-entity
            BY gl-ctrl-yr.yr:
            ASSIGN i = 1.
            DO i = 1 TO 13:
                IF service.entry-date <= gl-ctrl-yr.cy-end-dates[i] THEN 
                    LEAVE.
            END.
            IF i < 14 AND service.entry-date <= gl-ctrl-yr.cy-end-dates[i] THEN DO:
                ASSIGN 
                    service.yr = gl-ctrl-yr.yr
                    service.prd = i
                    i = 14.
                LEAVE.
            END.
        END.    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ipZfilememo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipZfilememo Procedure 
PROCEDURE ipZfilememo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH z_file:
       IF CAN-DO(vMemo-files,z_file.FILE-NAME) = TRUE THEN
           ASSIGN z_file.allow-memo = YES.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update-Line-Status) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update-Line-Status Procedure 
PROCEDURE Update-Line-Status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER order-line-rowid AS ROWID.
DEF INPUT PARAMETER old-status LIKE order-line.order-status.
DEF OUTPUT PARAMETER new-status LIKE order-line.order-status.
DEF VAR statcnt AS INT EXTENT 8.
FIND order-line WHERE ROWID(order-line) = order-line-rowid EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL order-line THEN DO:
    NEW-status = "NEW":U.
    order-line.order-status = new-status.
    RETURN.
END.

IF old-status <> ""  THEN
IF lookup(old-status,"Allocated,B/O,Invoiced,NEW,Open,Packed,Partial,Picked,PPS,Shipped") = 0 THEN do:
    new-status = OLD-status.
    order-line.order-status = new-status.
    RETURN.
END.
statcnt = 0.
new-status = old-status.
FOR EACH ord-lin-shpmt WHERE ord-lin-shpmt.ar-entity = order-line.ar-entity
    AND ord-lin-shpmt.order-no = order-line.order-no
    AND ord-lin-shpmt.line-no = order-line.line-no  NO-LOCK:
   CASE ord-lin-shpmt.order-status:
      WHEN "partial" THEN statcnt[2] = statcnt[2] + 1.
      when "allocated" then statcnt[3] = statcnt[3] + 1.
      when "PPS" then statcnt[4] = statcnt[4] + 1.
      WHEN "picked" THEN statcnt[5] = statcnt[5] + 1.
      when "packed" then statcnt[6] = statcnt[6] + 1.
      WHEN "shipped" THEN statcnt[7] = statcnt[7] + 1.
      WHEN "invoiced" THEN statcnt[8] = statcnt[8] + 1.
          OTHERWISE statcnt[1] = statcnt[1] + 1.
  END CASE.
END.
if statcnt[8] > 0 then new-status = "Invoiced".
if statcnt[7] > 0 then new-status = "Shipped".
IF statcnt[6] > 0 THEN NEW-status = "Packed".
IF statcnt[5] > 0 THEN NEW-status = "Picked".
if statcnt[4] > 0 then new-status = "PPS".
IF statcnt[3] > 0 THEN new-status = "Allocated".
IF statcnt[2] > 0 THEN new-status = "Partial".
IF statcnt[8] <> 0 AND (statcnt[7] <> 0 or
                        statcnt[6] <> 0 OR
                        statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
    new-status = "Partial".
IF statcnt[7] <> 0 AND (statcnt[6] <> 0 OR
                        statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
    new-status = "Partial".

IF statcnt[6] <> 0 AND (statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                         statcnt[2] <> 0 OR
                         statcnt[1] <> 0) THEN
    new-status = "Partial".
IF statcnt[5] <> 0 AND (statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
       new-status  = "Partial".
IF statcnt[4] <> 0 AND ( statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
       new-status = "Partial".
IF statcnt[3] <> 0 AND (statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
        new-status = "Partial".

IF statcnt[8] = 0 and statcnt[7] = 0 and statcnt[6] = 0 and statcnt[5] = 0 AND 
    statcnt[4] = 0 AND statcnt[3] = 0
    AND statcnt[2] = 0 AND statcnt[1] > 0 THEN
            new-status = "OPEN":U.

/* IF statcnt[1] > 0 THEN leave it alone, nothing has happend, or they
are using the non-standard codes 
*/
    order-line.order-status = NEW-status.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update-next-files) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update-next-files Procedure 
PROCEDURE Update-next-files :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF CAN-FIND(FIRST customer WHERE customer.country = "") THEN DO:
    ASSIGN
        vProcName = "ipCustCountry"
        lDataChanged = TRUE.
    DISPLAY vProcName WITH FRAME fixdisp.
    RUN  ipCustCountry.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update-Order-Status) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update-Order-Status Procedure 
PROCEDURE Update-Order-Status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER order-rowid AS ROWID.
DEF INPUT PARAMETER old-status LIKE order.order-status.
DEF OUTPUT PARAMETER new-status LIKE order.order-status.
DEF VAR statcnt AS INT EXTENT 8.

FIND order WHERE ROWID(order) = order-rowid EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL order THEN DO:
    NEW-status = "NEW":U.
    order.order-status = new-status.
    RETURN.
END.
IF old-status <> ""  THEN
IF LOOKUP(old-status,"Allocated,B/O,Invoiced,NEW,Open,Packed,Partial,Picked,PPS,Shipped") = 0 THEN do:
    new-status = OLD-status.

    order.order-status = new-status.
    RETURN.
END.
statcnt = 0.
new-status = "Open".
FOR EACH order-line WHERE order-line.ar-entity = order.ar-entity
    AND order-line.order-no = order.order-no NO-LOCK:
  CASE order-line.order-status:
     WHEN "partial" THEN statcnt[2] = statcnt[2] + 1.
      when "allocated" then statcnt[3] = statcnt[3] + 1.
      when "PPS" then statcnt[4] = statcnt[4] + 1.
      WHEN "picked" THEN statcnt[5] = statcnt[5] + 1.
      when "packed" then statcnt[6] = statcnt[6] + 1.
      WHEN "shipped" THEN statcnt[7] = statcnt[7] + 1.
      WHEN "invoiced" THEN statcnt[8] = statcnt[8] + 1.
          OTHERWISE statcnt[1] = statcnt[1] + 1.
  END CASE.
END.
if statcnt[8] > 0 then new-status = "Invoiced".
if statcnt[7] > 0 then new-status = "Shipped".
IF statcnt[6] > 0 THEN NEW-status = "Packed".
IF statcnt[5] > 0 THEN NEW-status = "Picked".
if statcnt[4] > 0 then new-status = "PPS".
IF statcnt[3] > 0 THEN new-status = "Allocated".
IF statcnt[2] > 0 THEN new-status = "Partial".
IF statcnt[8] <> 0 AND (statcnt[7] <> 0 or
                        statcnt[6] <> 0 OR
                        statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
    new-status = "Partial".
IF statcnt[7] <> 0 AND (statcnt[6] <> 0 OR
                        statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
    new-status = "Partial".

IF statcnt[6] <> 0 AND (statcnt[5] <> 0 OR
                        statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                         statcnt[2] <> 0 OR
                         statcnt[1] <> 0) THEN
    new-status = "Partial".
IF statcnt[5] <> 0 AND (statcnt[4] <> 0 OR
                        statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
       new-status  = "Partial".
IF statcnt[4] <> 0 AND ( statcnt[3] <> 0 OR
                        statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
       new-status = "Partial".
IF statcnt[3] <> 0 AND (statcnt[2] <> 0 OR
                        statcnt[1] <> 0) THEN
        new-status = "Partial".
IF statcnt[8] = 0 and statcnt[7] = 0 and statcnt[6] = 0 and statcnt[5] = 0 
    AND statcnt[4] = 0 AND statcnt[3] = 0
    AND statcnt[2] = 0 AND statcnt[1] > 0 THEN
            new-status = "OPEN":U.

/* IF statcnt[1] > 0 THEN leave it alone, nothing has happend, or they
are using the non-standard codes 
*/
    
    order.order-status = new-status.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CalcQuant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcQuant Procedure 
FUNCTION CalcQuant RETURNS CHARACTER
 ( INPUT qty AS INT, 
    INPUT entity AS char, 
    INPUT it-code AS CHAR, 
    INPUT in-uom AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Function to RETURN the converted formatted string of the qty  
    Notes: Send qty field (in the qty parameter) , and either
    1)in-entity and item-no  (in the entity and it-code parameters)
    or 
    2)uom-code to use (in the in-uom parameter) 
------------------------------------------------------------------------------*/
   DEF VAR uom-chr AS CHAR.
 
    IF in-uom = "" THEN DO:
        FIND ITEM WHERE 
            ITEM.in-Entity = entity AND
            ITEM.item-no = it-code NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN 
            FIND uom WHERE 
            uom.uom-code = ITEM.uom-code NO-LOCK NO-ERROR.
    END.
    ELSE
        FIND uom WHERE
            uom.uom-code = in-uom NO-LOCK NO-ERROR.
   IF AVAIL uom  THEN DO:
  
    ASSIGN uom-chr = string (truncate (qty  / uom.unit , 0 ) +
            ( qty / uom.unit - truncate ( qty  / uom.unit , 0 ) )
               * uom.unit / uom.exp-decimal , uom.mask ).

    do while substring ( uom-chr , 1 , 1 ) = " ":U :
        uom-chr = substring ( uom-chr , 2 , 24 ) .
    END.
   END.
   ELSE uom-chr = "0".
    RETURN uom-chr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConvQuant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvQuant Procedure 
FUNCTION ConvQuant RETURNS INTEGER
  ( INPUT cQty AS CHAR,
    INPUT entity AS char, 
    INPUT it-code AS CHAR, 
    INPUT in-uom AS CHAR  ) :
/*------------------------------------------------------------------------------
  Purpose:  Function to RETURN integer qty for storing in db...from Char display field
    Notes:  Send qty field (in the qty parameter) , and either
    1)in-entity and item-no  (in the entity and it-code parameters)
    or 
    2)uom-code to use (in the in-uom parameter)   
------------------------------------------------------------------------------*/

  DEF VAR uom--err AS LOG.
DEF VAR uom--dec AS DEC.
DEF VAR uom--int AS INT.
DEF VAR i AS INT.
DEF VAR vv AS CHAR.
DEF VAR uomerr AS LOG.
cQty = TRIM(cQty).


/* i = 1. */
    ASSIGN uomerr = NO.

    DO i = 1 TO LENGTH(cQty):
        IF INDEX("-1234567890.",SUBSTRING(cQty,i,1)) = 0 THEN DO:
            MESSAGE "The number is not numeric. Please re-enter"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN uomerr = YES.
        END.
    END.
    
    IF NOT uomerr THEN DO:
        
    
        uom--int = 0.
        IF in-uom = "" THEN DO:
                FIND ITEM WHERE 
                    ITEM.in-entity = entity AND
                    ITEM.item-no = it-code NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN 
                    FIND uom WHERE 
                    uom.uom-code = ITEM.uom-code NO-LOCK NO-ERROR.
            END.
            ELSE
                FIND uom WHERE
                    uom.uom-code = in-uom NO-LOCK NO-ERROR.
         uom--err = true .
    IF AVAIL uom THEN DO: 
        DO on error undo, LEAVE :
         assign
             uom--dec = decimal ((cQty))
             uom--int = (uom--dec - truncate(uom--dec ,0)) * uom.exp-decimal
             uom--err = maximum ( uom--int , - uom--int ) ge uom.unit or
                        maximum ( uom--dec , - uom--dec ) gt uom.max-value .
        end.
        uom--int = if uomerr then 0 else truncate(uom--dec ,0) * uom.unit + uom--int.
        if uom--err then do :
           MESSAGE "Invalid quantity entered, Unit =" uom.unit "  Max = " uom.MAX-VALUE
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        end.
        uomerr = uom--err.
    END.
      ELSE ASSIGN uom--int = 0.
    END.
  RETURN uom--int.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


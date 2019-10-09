&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oerep\fibreci_MEX-XL.p
    
    Purpose     : Commercial Invoice for FIBRE Mexico procedure

    Description : Generates FIBRE MEXICO Excel Form

    Author(s)   : Eric Panchenko
    
    Created     : Jul 16, 2007
     
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters */
def input param icInvoiceNum      as char no-undo.
def input param icDate            as char no-undo.
def input param icTotalPallets    as INT no-undo.
def input param icTrailerNum      as char no-undo.
def input param icNotes1          as char no-undo.
def input param icNotes2          as char no-undo.
def input param icNotes3          as char no-undo.
def input param icNotes4          as char no-undo.
def input param icNotes5          as char no-undo.
def input param icNotes6          as char no-undo.

/* Includes */
 {sys/inc/var.i shared}

/* Temp-Tables */
DEF TEMP-TABLE tt-freight-class NO-UNDO
    FIELD freight-class AS CHAR
    FIELD qty LIKE oe-boll.qty
    FIELD weight AS DEC DECIMALS 2
    FIELD price AS DEC DECIMALS 4
    FIELD avg-price AS DEC DECIMALS 4
    FIELD total-price LIKE oe-ordl.t-price
    FIELD LANGUAGE AS CHAR
    FIELD freight-class-trans AS CHAR
    INDEX tt-index freight-class
    INDEX tt-lang  LANGUAGE.

def SHARED TEMP-TABLE w-comm-bol NO-UNDO field bol-no as INT INDEX bol-no bol-no.

/* VARIABLE FOR EXCEL OUTPUT */
DEFINE new SHARED VARIABLE LvOutputSelection    AS CHAR NO-UNDO.
DEFINE new SHARED VARIABLE CallingParameter     AS CHAR NO-UNDO.

/* Variables for excel Automation  */
DEFINE NEW SHARED VARIABLE chExcelApplication   AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorkBook           AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chWorksheet          AS COM-HANDLE   NO-UNDO.
DEFINE NEW SHARED VARIABLE chHyper              AS COM-HANDLE   NO-UNDO.
DEFINE            VARIABLE chFile               AS CHAR         NO-UNDO.
DEFINE            VARIABLE CurActivePrinter     AS CHAR         NO-UNDO.
DEFINE            VARIABLE AdobePrinter         AS CHAR         NO-UNDO.
define            variable CommandString        AS CHAR         NO-UNDO.
define            variable WshNetwork           as com-handle.
DEFINE            VARIABLE LvFirstTimePrint     AS LOGICAL      NO-UNDO   INIT no.
DEFINE            VARIABLE CurrDir              AS CHARACTER    NO-UNDO.
DEFINE            VARIABLE vcTemplateFile       AS CHARACTER    NO-UNDO.
DEF VAR mypict AS COM-HANDLE.
def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
def var lv-t-price as dec no-undo.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

/* Build a Table to keep sequence of pdf files */
DEFINE new SHARED TEMP-TABLE tt-filelist
                       FIELD tt-FileCtr         AS INT
                       FIELD tt-FileName        AS CHAR
                       INDEX filelist           IS PRIMARY 
                             TT-FILECTR.

DEF BUFFER tt-freight-class-1 FOR tt-freight-class.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 22.81
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


run InitializeExcel.
run MainLoop.
run Cleanup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-bol-data-proc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bol-data-proc Procedure 
PROCEDURE bol-data-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE tt-freight-class.

  /*Gather data from BOL*/
  FOR EACH w-comm-bol,
      EACH oe-bolh FIELDS(cust-no ship-id company b-no) WHERE
      oe-bolh.company eq cocode AND
      oe-bolh.bol-no  EQ w-comm-bol.bol-no
      NO-LOCK,
      FIRST sys-ctrl-shipto FIELDS(company NAME char-fld cust-vend
            cust-vend-no ship-id) WHERE
            sys-ctrl-shipto.company      EQ cocode AND
            sys-ctrl-shipto.name         EQ "CINVOICE" AND
            sys-ctrl-shipto.char-fld     EQ "FIBREMEXICO" AND
            sys-ctrl-shipto.cust-vend    EQ YES AND
            sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no AND
            sys-ctrl-shipto.ship-id      EQ oe-bolh.ship-id
            NO-LOCK,
      EACH oe-boll FIELDS(ord-no i-no LINE qty weight) WHERE
           oe-boll.company EQ oe-bolh.company AND
           oe-boll.b-no    EQ oe-bolh.b-no
           NO-LOCK,
      FIRST itemfg FIELDS(frt-class case-count) WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no EQ oe-boll.i-no
            NO-LOCK,
      FIRST oe-ordl FIELDS(price pr-uom disc cas-cnt) WHERE
            oe-ordl.company EQ cocode AND
            oe-ordl.ord-no EQ oe-boll.ord-no AND
            oe-ordl.LINE   EQ oe-boll.LINE
            NO-LOCK:
    
          FIND FIRST tt-freight-class WHERE
               tt-freight-class.freight-class EQ itemfg.frt-class
               NO-ERROR.
          
          IF NOT AVAIL tt-freight-class THEN
          DO:
             CREATE tt-freight-class.
             tt-freight-class.freight-class = itemfg.frt-class.

             FIND FIRST freight-class WHERE
                  freight-class.freight-class EQ itemfg.frt-class
                  NO-LOCK NO-ERROR.

             IF AVAIL freight-class THEN
             DO:
                tt-freight-class.freight-class-trans = freight-class.DESCRIPTION.
                RELEASE freight-class.
             END.
          END.
          
          ASSIGN tt-freight-class.qty = tt-freight-class.qty
                                      + oe-boll.qty
                 tt-freight-class.weight = tt-freight-class.weight
                                         + oe-boll.weight
                 v-tmp-price = if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
                                if oe-boll.qty lt 0 then -1 else 1
                                else
                                if oe-ordl.pr-uom eq "CS" then
                                   oe-boll.qty / (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                                                  IF itemfg.case-count ne 0 then itemfg.case-count else
                                                            1)
                                else
                                if oe-ordl.pr-uom eq "C" then
                                   oe-boll.qty / 100
                                else
                                if oe-ordl.pr-uom eq "M" then
                                   oe-boll.qty / 1000
                                else
                                   oe-boll.qty
                                 
                 lv-t-price = v-tmp-price * oe-ordl.price
                 tt-freight-class.price = ROUND(lv-t-price * (1 - (oe-ordl.disc / 100)),2)
                 tt-freight-class.avg-price = tt-freight-class.avg-price
                                            + tt-freight-class.price.

          RELEASE tt-freight-class.
  END.

  FOR EACH tt-freight-class WHERE
      tt-freight-class.LANGUAGE EQ ""
      BREAK BY tt-freight-class.freight-class:

      IF LAST-OF(tt-freight-class.freight-class) THEN
      DO:
         tt-freight-class.total-price = ROUND(tt-freight-class.avg-price,2).

         IF tt-freight-class.qty NE 0 THEN
            tt-freight-class.avg-price = ROUND(tt-freight-class.total-price /
                                               tt-freight-class.qty,4).
         ELSE
            tt-freight-class.avg-price = 0.

         CREATE tt-freight-class-1.
         BUFFER-COPY tt-freight-class EXCEPT weight TO tt-freight-class-1
           ASSIGN tt-freight-class-1.LANGUAGE = "Spanish"
                  tt-freight-class-1.weight = ROUND(tt-freight-class.weight * 0.4536,2).

         FIND FIRST frt-class-desc-trans WHERE
              frt-class-desc-trans.freight-class EQ tt-freight-class.freight-class AND
              frt-class-desc-trans.LANGUAGE EQ "Spanish"
              NO-LOCK NO-ERROR.

         IF AVAIL frt-class-desc-trans THEN
         DO:
            tt-freight-class-1.freight-class-trans = frt-class-desc-trans.translation.
            RELEASE frt-class-desc-trans.
         END.
         
         RELEASE tt-freight-class-1.
      END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp Procedure 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Clean up routine.
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* RELEASE OBJECTS */
  RELEASE OBJECT chWorkbook         NO-ERROR.
  RELEASE OBJECT chWorkSheet        NO-ERROR.
  RELEASE OBJECT chHyper            NO-ERROR.

  /* Delete pre-existing PDF File. */
  os-delete value(v-dir + "TempFile.pdf").

  RUN UTIL/CurrDir.p (output CurrDir).

  /* Set the PDF Merging Utility. */
  assign CommandString = CurrDir + "\util\pdftk ".
  
  /* Add the PDF Filenames to be merged to the command string.  */
  FOR EACH tt-filelist :
    assign CommandString = CommandString + " " + tt-FileName .
  END.
  
  /* Indicate the new filename of combined PDF File. */
  assign CommandString = CommandString + " cat output " + v-dir + "TempFile.pdf".
  
  /* Merge the PDF Files. */
  os-command silent value(CommandString).
  
  /* Delete the "old" PDF Files. */
  FOR EACH tt-filelist :
    os-delete value(tt-FileName).
  END.
  
  /* Reset the Active Printer to the Original Printer. */
  if CurActivePrinter <> '' then
    WshNetwork:SetDefaultPrinter(CurActivePrinter).

  /* For E-mail and Printer jobs, close Excel. */
  IF LvOutputSelection = "PRINTER" OR 
     LvOutputSelection = "EMAIL" THEN
    chExcelApplication:Quit() no-error.
  
  /* Release created objects. */
  RELEASE OBJECT WshNetwork         NO-ERROR.
  RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillData Procedure 
PROCEDURE FillData :
/*------------------------------------------------------------------------------
  Purpose     : Populates the Sheet 
  Parameters  : 
  Notes       :       
------------------------------------------------------------------------------*/
  DEF VAR viCount AS INT INIT 21 NO-UNDO.
  DEF VAR vdTotalDollars AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR viTotalQuantity LIKE oe-boll.qty NO-UNDO.
  DEF VAR viTotalWeightLBS AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR viTotalWeightKGS AS DEC DECIMALS 2 NO-UNDO.
  DEF VAR v-sig-image AS CHAR NO-UNDO.

  ASSIGN
     chWorkSheet:Range("B9"):value = icInvoiceNum 
     chWorkSheet:Range("E9"):value = icDate NO-ERROR.

  RUN bol-data-proc.

  FOR EACH tt-freight-class WHERE
      tt-freight-class.LANGUAGE EQ "":

      ASSIGN
         chWorksheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):borders:lineStyle = 1
         chWorkSheet:Range("A" + STRING(viCount)):value = tt-freight-class.qty
         chWorkSheet:Range("B" + STRING(viCount)):HorizontalAlignment = -4152
         chWorkSheet:Range("B" + STRING(viCount)):VALUE = "(USA)   EA"
         chWorkSheet:Range("C" + STRING(viCount)):VALUE = tt-freight-class.weight
         chWorkSheet:Range("D" + STRING(viCount)):VALUE = "LBS"
         chWorkSheet:Range("E" + STRING(viCount)):VALUE = tt-freight-class.freight-class-trans
         chWorkSheet:Range("F" + STRING(viCount)):VALUE = tt-freight-class.avg-price
         chWorkSheet:Range("G" + STRING(viCount)):VALUE = tt-freight-class.total-price
         vdTotalDollars = vdTotalDollars + tt-freight-class.total-price
         viTotalQuantity = viTotalQuantity + tt-freight-class.qty
         viTotalWeightLBS = viTotalWeightLBS + tt-freight-class.weight
         viCount = viCount + 1.

      FIND FIRST tt-freight-class-1 WHERE
           tt-freight-class-1.freight-class EQ tt-freight-class.freight-class AND
           tt-freight-class-1.LANGUAGE EQ "Spanish"
           NO-LOCK NO-ERROR.

      IF AVAIL tt-freight-class-1 THEN
      DO:
         ASSIGN
            chWorksheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):borders:lineStyle = 1
            chWorkSheet:Range("A" + STRING(viCount)):value = tt-freight-class-1.qty
            chWorkSheet:Range("B" + STRING(viCount)):HorizontalAlignment = -4152
            chWorkSheet:Range("B" + STRING(viCount)):VALUE = "PZ"
            chWorkSheet:Range("C" + STRING(viCount)):VALUE = tt-freight-class-1.weight
            chWorkSheet:Range("D" + STRING(viCount)):VALUE = "KGS"
            chWorkSheet:Range("E" + STRING(viCount)):VALUE = tt-freight-class-1.freight-class-trans
            viTotalWeightKGS = viTotalWeightKGS + tt-freight-class-1.weight
            viCount = viCount + 1.
         RELEASE tt-freight-class-1.
      END.
  END. /*each tt-freight-class*/

  ASSIGN
     chWorkSheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):Interior:ColorIndex = 6
     chWorksheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):borders:lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = viTotalQuantity
     chWorkSheet:Range("C" + STRING(viCount)):value = viTotalWeightLBS
     chWorkSheet:Range("D" + STRING(viCount)):VALUE = "LBS"
     chWorkSheet:Range("E" + STRING(viCount)):VALUE = "Net Weight"
     chWorkSheet:Range("G" + STRING(viCount)):VALUE = vdTotalDollars
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):Interior:ColorIndex = 6
     chWorksheet:Range("A" + STRING(viCount) + ":" + "G" + STRING(viCount)):borders:lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = viTotalQuantity
     chWorkSheet:Range("C" + STRING(viCount)):value = viTotalWeightKGS
     chWorkSheet:Range("D" + STRING(viCount)):VALUE = "KGS"
     viCount = viCount + 2
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(8):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(8):lineStyle = 1
     chWorksheet:Range("C" + STRING(viCount)):borders(8):lineStyle = 1
     chWorksheet:Range("D" + STRING(viCount)):borders(8):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(8):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Maquila:"
     chWorkSheet:Range("C" + STRING(viCount)):VALUE = ": Fibre Container de Mexico S.A. de C.V."
     viCount = viCount + 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Patente Agente Aduanal:"
     chWorkSheet:Range("C" + STRING(viCount)):VALUE = ":#"
     viCount = viCount + 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Agencia Aduanal:"
     chWorkSheet:Range("C" + STRING(viCount)):value = "ROMERO GALAVIZ,S.C."
     viCount = viCount + 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Total Value Dollars:"
     chWorkSheet:Range("C" + STRING(viCount)):value = vdTotalDollars
     chWorkSheet:Range("C" + STRING(viCount)):HorizontalAlignment = -4152
     chWorkSheet:Range("C" + STRING(viCount)):NumberFormat = "###,###,##0.00"
     chWorkSheet:Range("D" + STRING(viCount)):value = "DLLS"
     viCount = viCount + 1.

  ASSIGN
     viTotalWeightLBS = viTotalWeightLBS + (20 * icTotalPallets)
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):NumberFormat = "###,###,##0.00"
     chWorkSheet:Range("A" + STRING(viCount)):value = "Total LBS Weight:"
     chWorkSheet:Range("C" + STRING(viCount)):value = viTotalWeightLBS
     chWorkSheet:Range("C" + STRING(viCount)):HorizontalAlignment = -4152
     chWorkSheet:Range("D" + STRING(viCount)):value = "LBS / GROSS WEIGHT"
     viCount = viCount + 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Total Pallets:"
     chWorkSheet:Range("C" + STRING(viCount)):HorizontalAlignment = -4152
     chWorkSheet:Range("C" + STRING(viCount)):value = icTotalPallets
     chWorkSheet:Range("D" + STRING(viCount)):value = "PALLETS"
     viCount = viCount + 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("C" + STRING(viCount)):borders(9):lineStyle = 1
     chWorksheet:Range("D" + STRING(viCount)):borders(9):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("E" + STRING(viCount)):borders(9):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Trailer#:"
     chWorkSheet:Range("B" + STRING(viCount)):HorizontalAlignment = 3
     chWorkSheet:Range("B" + STRING(viCount)):value = icTrailerNum
     chWorkSheet:Range("C" + STRING(viCount)):value = viTotalQuantity
     chWorkSheet:Range("C" + STRING(viCount)):HorizontalAlignment = -4152
     chWorkSheet:Range("D" + STRING(viCount)):value = "PZS"
     viCount = viCount + 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Driver/Chofer:"
     viCount = viCount + 1
     chWorksheet:Range("B" + STRING(viCount)):borders(10):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(9):lineStyle = 1
     chWorksheet:Range("B" + STRING(viCount)):borders(9):lineStyle = 1
     chWorksheet:Range("A" + STRING(viCount)):borders(7):lineStyle = 1
     chWorkSheet:Range("A" + STRING(viCount)):value = "Plates/Placas:"
     viCount = viCount + 1.

 ASSIGN
     chWorkSheet:Range("A" + STRING(viCount)):value = "Se declara bajo protesta de decir verdad:"
     v-sig-image = "signature\spanish\" + USERID("nosweat") + ".jpg"
     FILE-INFO:FILE-NAME = v-sig-image.
     
  /*ESP - Only way I know how to get signature to work is for it to be on its own line */

  IF SEARCH(FILE-INFO:FULL-PATHNAME) NE ? THEN DO:
     chExcelApplication:Range("A" + STRING(viCount) + ":E" +
                              STRING(viCount)):MergeCells = TRUE.
     chExcelApplication:Goto("R" + STRING(viCount) + "C5") NO-ERROR.
     mypict = chExcelApplication:Range("D" + STRING(viCount)):Parent:Pictures:Insert(FILE-INFO:FULL-PATHNAME).
     mypict:TOP = chExcelApplication:Range("D" + STRING(viCount)):TOP.
     mypict:LEFT = chExcelApplication:Range("D" + STRING(viCount)):LEFT.
     RELEASE OBJECT mypict.
  END.

  ASSIGN
     viCount = viCount + 6
     chWorkSheet:Range("A" + STRING(viCount)):value = "NOTES:"
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes1
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes2
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes3
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes4
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes5
     viCount = viCount + 1
     chWorkSheet:Range("A" + STRING(viCount)):value = icNotes6.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeExcel Procedure 
PROCEDURE InitializeExcel :
/*------------------------------------------------------------------------------
  Purpose   :   Initializes Excel Environment
  Parameters:   None
  Notes     :   
------------------------------------------------------------------------------*/

  /* Capture the current active printer. */
  IF LvOutputSelection = "email" THEN
    assign 
      CurActivePrinter = SESSION:PRINTER-NAME
      AdobePrinter     = "PDFcamp Printer".
  
  vcTemplateFile   = "template\fibreci_MEX-XL.xlt".

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication connect no-error.

  /* If Excel is running close it. */
  if valid-handle (chExcelApplication) then
  do:
    chExcelApplication:Quit()         no-error.
    run CleanUp.
  end.


  /* Network connection checks. */
  CREATE "WScript.Network" WshNetwork NO-ERROR.
  IF NOT(VALID-HANDLE(WshNetwork)) THEN
  DO :
    MESSAGE "Unable to Create Wscript.Network" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  /* Switch Printer to PDFCamp Printer. */
  IF LvOutputSelection = "Email" THEN
     WshNetwork:SetDefaultPrinter(AdobePrinter).

  /* Start a new session of Excel. */
  /*if not (valid-handle (chExcelApplication)) THEN*/
    CREATE "Excel.Application" chExcelApplication NO-ERROR.
  
  /* Check if Excel got initialized. */
  IF not (valid-handle (chExcelApplication)) THEN
  DO :
    MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
  
  FILE-INFO:FILE-NAME = vcTemplateFile.

  /* Set the Excel Template to be used. */
  ASSIGN chFile = search (FILE-INFO:FULL-PATHNAME) no-error.
  
  if search (chFile) = ? then do:
    MESSAGE 'Template File: ' FILE-INFO:FULL-PATHNAME
            'cannot be found. Please verify that the file exists.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    apply 'CLOSE':U to this-procedure.
  end.

  /* Make Excel visible. */
  ASSIGN
     chFile = FILE-INFO:FULL-PATHNAME
     chExcelApplication:VISIBLE = TRUE.
  
  /* If we are going to E-Mail or Print, hide Excel. */
  IF LvOutputSelection = "Email"    or 
     LvOutputSelection = "Printer"  THEN
    chExcelApplication:VISIBLE = FALSE.
  
  /* Clear tt-FileList. */
  empty temp-table tt-filelist.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MainLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MainLoop Procedure 
PROCEDURE MainLoop :
/*------------------------------------------------------------------------------
  Purpose     : Main Loop for the Report Logic
  Parameters  : None
  Notes       :       
------------------------------------------------------------------------------*/

  /* Open our Excel Template. */  
  assign chWorkbook = chExcelApplication:Workbooks:Open(chfile)  no-error.
  
  /* Do not display Excel error messages. */
  chExcelApplication:DisplayAlerts = false  no-error.

  /* Disable screen updating so it will go faster */
  chExcelApplication:ScreenUpdating = False.

  /* Go to the Active Sheet. */
  chWorkbook:WorkSheets(1):Activate no-error.
  chWorkSheet      = chExcelApplication:Sheets:item(1).

  /*Fill in Data*/
  run FillData.

  /*
  /* Preview the Spreadsheet. Excel 2007 doesn't seem to work well*/
  chWorkbook:PrintOut(1,999,1,yes,,no,no) no-error. */

  /* enable screen updating */
  chExcelApplication:ScreenUpdating = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


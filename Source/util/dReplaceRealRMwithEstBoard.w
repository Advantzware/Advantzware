&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters: 
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
{sys/inc/var.i}

/* I considered using outputProcs here, but was not sure it could handle multiple streamed output */
DEFINE STREAM sFile1.
DEFINE STREAM sFile2.

DEFINE BUFFER bf-item FOR item.
DEFINE BUFFER b-ef FOR ef.
DEFINE TEMP-TABLE ttEst 
    FIELD cEst-no AS CHARACTER FORMAT "x(8)"
    FIELD iEstno AS INTEGER
    FIELD rRowid AS ROWID.
DEFINE VARIABLE iCtr AS INT NO-UNDO.
DEFINE VARIABLE jCtr AS INT NO-UNDO.
DEFINE VARIABLE iTotal AS INT NO-UNDO.
DEFINE VARIABLE iDone AS INT NO-UNDO.
DEFINE VARIABLE iTotalWidth AS INT NO-UNDO.
DEFINE VARIABLE iProgressWidth AS INT NO-UNDO.
DEFINE VARIABLE iNumProcessed AS INT NO-UNDO. 
DEFINE VARIABLE iNumSelected AS INT NO-UNDO.
DEFINE VARIABLE lSuccess AS LOG NO-UNDO.
DEFINE VARIABLE cOutFile1 AS CHAR NO-UNDO.
DEFINE VARIABLE cOutFile2 AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rBox rProgress eInstructions Btn_Simulate ~
fiEstStart fiEstEnd Btn_Process fiItemStart fiItemEnd Btn_Reset Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiInstructions eInstructions fiParameters ~
fiFrom fiTo fiEstStart fiEstEnd fiItemStart fiItemEnd fiTotalEstimates ~
fiCurrent fiNumberSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Process 
     LABEL "Process" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON Btn_Reset 
     LABEL "Reset" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Simulate 
     LABEL "Simulate" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 107 BY 8.1 NO-UNDO.

DEFINE VARIABLE fiCurrent AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Processing Est No" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiEstEnd AS INTEGER FORMAT ">>>>>>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiEstStart AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Estimate Number" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFrom AS CHARACTER FORMAT "X(256)":U INITIAL "FROM" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiInstructions AS CHARACTER FORMAT "X(256)":U INITIAL "Instructions:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE fiItemEnd AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiItemStart AS CHARACTER FORMAT "X(256)":U 
     LABEL "RM Item Num." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumberSelected AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Number Selected for Conversion" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiParameters AS CHARACTER FORMAT "X(256)":U INITIAL "Parameters:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTo AS CHARACTER FORMAT "X(256)":U INITIAL "TO" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotalEstimates AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Estimates for this Company" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rBox
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.

DEFINE RECTANGLE rProgress
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 88 BY 1
     BGCOLOR 10 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiInstructions AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     eInstructions AT ROW 1.95 COL 4 NO-LABEL
     fiParameters AT ROW 10.29 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiFrom AT ROW 10.29 COL 35 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiTo AT ROW 10.29 COL 59 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     Btn_Simulate AT ROW 10.29 COL 96
     fiEstStart AT ROW 11.71 COL 35 COLON-ALIGNED
     fiEstEnd AT ROW 11.71 COL 59 COLON-ALIGNED NO-LABEL
     Btn_Process AT ROW 11.95 COL 96
     fiItemStart AT ROW 12.91 COL 35 COLON-ALIGNED
     fiItemEnd AT ROW 12.91 COL 59 COLON-ALIGNED NO-LABEL
     fiTotalEstimates AT ROW 14.57 COL 35 COLON-ALIGNED
     fiCurrent AT ROW 14.57 COL 74 COLON-ALIGNED
     fiNumberSelected AT ROW 15.76 COL 35 COLON-ALIGNED
     Btn_Reset AT ROW 15.76 COL 96
     Btn_Cancel AT ROW 17.43 COL 96
     rBox AT ROW 17.43 COL 5
     rProgress AT ROW 17.43 COL 5
     SPACE(20.99) SKIP(0.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Replace 'Real' Item Numbers with Est. Board Items"
         DEFAULT-BUTTON Btn_Process CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

ASSIGN 
       eInstructions:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiCurrent IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFrom IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       fiFrom:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiInstructions IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       fiInstructions:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiNumberSelected IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiParameters IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       fiParameters:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiTo IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       fiTo:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiTotalEstimates IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Replace 'Real' Item Numbers with Est. Board Items */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Process gDialog
ON CHOOSE OF Btn_Process IN FRAME gDialog /* Process */
OR CHOOSE OF btn_Simulate 
DO:
    OUTPUT STREAM sFile1 TO VALUE(cOutFile1).
    IF SELF:NAME = "btn_process" THEN 
        OUTPUT STREAM sFile2 TO VALUE(cOutFile2).

    PUT STREAM sFile1
        "Estimate"  AT 1
        "Orig Item" AT 11
        "Flute"     AT 26
        "Reg. No."  AT 36
        "New Item"  AT 46
        SKIP
        "--------"  AT 1
        "---------" AT 11
        "-----"     AT 26
        "--------"  AT 36
        "--------"  AT 46
        .
    
    /* When the user elects to process a 'partial' group of estimates, we'll remove the
       out of range estimates from the temp-table so the progress bar calculates correctly */
    ASSIGN 
        iCtr = INT(fiTotalEstimates:SCREEN-VALUE).
    FOR EACH ttEst WHERE 
        ttEst.iEstNo LT INT(fiEstStart:SCREEN-VALUE) OR 
        ttEst.iEstNo GT INT(fiEstEnd:SCREEN-VALUE):
        DELETE ttEst.
        ASSIGN 
            iCtr = iCtr - 1.
    END.  
    ASSIGN 
        fiNumberSelected:SCREEN-VALUE = STRING(iCtr)
        iNumSelected = iCtr
        iNumProcessed = 0
        iProgressWidth = 1.
        
    /* This calls the worker process, using the (properly sorted) estimate number */
    FOR EACH ttEst BY ttEst.iEstNo:
        RUN pProcessRecord (SELF:NAME, ttEst.rRowid, OUTPUT lSuccess).
        ASSIGN 
            iNumProcessed = iNumProcessed + 1
            rProgress:WIDTH-P = MAXIMUM(1,(iNumProcessed / iNumSelected) * rBox:WIDTH-P)
            fiCurrent:SCREEN-VALUE = ttEst.cEst-no.
    END.        
    
    OUTPUT STREAM sFile1 CLOSE.
    IF SELF:NAME = "btn_process" THEN 
        OUTPUT STREAM sFile2 CLOSE.

    MESSAGE "Process complete." VIEW-AS ALERT-BOX INFO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset gDialog
ON CHOOSE OF Btn_Reset IN FRAME gDialog /* Reset */
DO:
    RUN pResetValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

RUN pResetValues.

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiInstructions eInstructions fiParameters fiFrom fiTo fiEstStart 
          fiEstEnd fiItemStart fiItemEnd fiTotalEstimates fiCurrent 
          fiNumberSelected 
      WITH FRAME gDialog.
  ENABLE rBox rProgress eInstructions Btn_Simulate fiEstStart fiEstEnd 
         Btn_Process fiItemStart fiItemEnd Btn_Reset Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessRecord gDialog 
PROCEDURE pProcessRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcMode AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER oplSuccess AS LOG NO-UNDO.

    FIND ef NO-LOCK WHERE
        ROWID(ef) EQ iprRowid
        NO-ERROR.
        
    IF AVAIL ef THEN FIND FIRST item NO-LOCK WHERE 
        item.company EQ ef.company AND 
        item.i-no EQ ef.board AND 
        item.i-code EQ "R" AND 
        item.i-no BEGINS "RM"
        NO-ERROR.
    ELSE DO:
        ASSIGN oplSuccess = FALSE.
        RETURN.
    END.
    
    FIND FIRST bf-item NO-LOCK WHERE 
        bf-item.company EQ item.company AND 
        bf-item.mat-type EQ item.mat-type AND 
        bf-item.flute EQ item.flute AND 
        bf-item.reg-no EQ item.reg-no AND 
        bf-item.i-code EQ "E"
        NO-ERROR.
    
    IF AVAILABLE bf-item THEN DO: 
        PUT STREAM sFile1  
            ef.est-no   AT 1  FORMAT "x(8)"
            ITEM.i-no   AT 11
            ITEM.flute  AT 26
            ITEM.reg-no AT 36 FORMAT "x(8)"
            bf-item.i-no AT 46
            SKIP. 
            
        IF ipcMode = "Process" THEN DO:
            FIND b-ef EXCLUSIVE WHERE 
                ROWID(b-ef) EQ ROWID(ef).
            EXPORT STREAM sFile2 b-ef.
            ASSIGN 
                b-ef.board = bf-item.i-no.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetValues gDialog 
PROCEDURE pResetValues :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR cTime AS CHAR NO-UNDO.
    ASSIGN
        cTime = STRING(TIME)
        cOutFile1 = "C:\tmp\EstBoardChanges_" + 
                    STRING(YEAR(TODAY),"9999") +
                    STRING(MONTH(TODAY),"99") + 
                    STRING(DAY(TODAY),"99") + "_" +
                    cTime + ".txt"
        cOutFile2 = "C:\tmp\EstBoardChanges_" + 
                    STRING(YEAR(TODAY),"9999") +
                    STRING(MONTH(TODAY),"99") + 
                    STRING(DAY(TODAY),"99") + "_" +
                    cTime + ".d"
        cocode = IF cocode = "" THEN "001" ELSE cocode
        iCtr = 0
        iTotalWidth = rBox:WIDTH-P IN FRAME {&frame-name} 
        iProgressWidth = 1
        rProgress:WIDTH-P = iProgressWidth
        eInstructions = "Use this function to scan your estimates locating every instances where the board item has been associated with " + 
                        "an item of type 'Real Material' instead of 'Estimated'. When this case is found, this utility will attempt to " +
                        "locate a corresponding item of type 'Estimated', and replace the board item with the 'Estimated' item number." + CHR(10) + CHR(10) +
                        "You can run this utility in 'Simulation' mode to only output a report of estimates with this condition, or in " +
                        "'Processing' mode to actually make the changes." + CHR(10) + CHR(10) +
                        "A list of estimates/board items with this condition will be written to a file in the form 'C:\Tmp\EstBoardChanges_xxxx.txt' " +
                        "(where xxxx is a number representing today's data and time).  If you 'Process' with this utility, an output file " + 
                        "of the records PRIOR TO the conversion will also be written to file 'C:\tmp\EstBoardChanges_xxxx.d'.  This file may " +
                        "be used to restore the previous records if required.". 
                        
    /* This probably requires some explanation.  The ef file contains an 'est-no' which is a space-padded character value.
       As we know, this is terrible for sorting.  To resolve this, our temp-table (which is built before the window displays,
       or on CHOOSE of the Reset button) reads the ef table to build this TT.  Within the TT, we store the CHAR-based est-no 
       for reference, the rowid of the record for fast FINDS, and the INTEGER value of the est no.  This integer value allows the
       user to select beginning and ending estimate numbers without having to be concerned about the space-padding. 
       Since we're reading the entire table (NO-LOCK) anyway, we also have a "starting count" of all records.  With this, we're able
       to use the cool progress bar to graphically display the progress of the operation to the user. 
       FYI, on Premier DB, with over 100K estimates, this process takes about 9 seconds. */
    EMPTY TEMP-TABLE ttEst.
    FOR EACH ef WHERE company EQ cocode NO-LOCK:
        CREATE
            ttEst.
        ASSIGN
            ttEst.cEst-no = ef.est-no
            ttEst.iEstno = INTEGER(ef.est-no)
            ttEst.rRowid = ROWID(ef)
            iCtr = iCtr + 1.
    END.
    ASSIGN 
        fiTotalEstimates = iCtr
        fiNumberSelected = iCtr
        fiEstStart = 0
        fiEstEnd = 99999999
        fiItemStart = ""
        fiItemEnd = "zzzzzzzzzz".
    DISPLAY {&displayed-objects} WITH FRAME {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ap-ctrl.w.w

  Description: G/L Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEF BUFFER b-ar-cashl FOR ar-cashl.

DEF STREAM out1.
DEF STREAM out2.
DEF STREAM out3.
DEF STREAM out4.
DEF STREAM out5.
DEF STREAM out6.

DEF VAR cDestroy AS CHAR NO-UNDO.
DEF VAR iPurgeCount AS INT NO-UNDO.
DEF VAR iPurgeCount2 AS INT NO-UNDO.
DEF VAR iPurgeCount3 AS INT NO-UNDO.
DEF VAR iPurgeCount4 AS INT NO-UNDO.
DEF VAR iPurgeCount5 AS INT NO-UNDO.
DEF VAR iPurgeCount6 AS INT NO-UNDO.
DEF VAR invBalAmt AS DEC NO-UNDO.
DEF VAR totWriteOff AS DEC NO-UNDO.
DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN
    totWriteOff = 0.
    
DEFINE TEMP-TABLE ttPurgeInvCsv
    FIELD company       AS CHARACTER LABEL "Company"
    FIELD iInvNo        AS INTEGER   LABEL "Invoice#"
    FIELD cCustNo       AS CHARACTER LABEL "Customer"
    FIELD dtInvDate     AS DATE LABEL "Inv Date"
    FIELD dInvAmount    AS DECIMAL LABEL "Amount" 
    FIELD dBalDue       AS DECIMAL LABEL "Balance Due"
    FIELD cNote         AS CHARACTER LABEL "Notes"
    .      
    
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btSimulatePurge btStartProcess tbNonZero ~
end_date begin_cust-no end_cust-no begin_inv end_inv tbInactive fiDirectory ~
tbOpenFile 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 tbNonZero end_date begin_cust-no ~
end_cust-no begin_inv end_inv tbInactive fiDirectory tbOpenFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btSimulatePurge 
     IMAGE-UP FILE "Graphics/32x32/simulate.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Simulate" 
     SIZE 19 BY 1.14
     BGCOLOR 14 .

DEFINE BUTTON btStartProcess 
     IMAGE-UP FILE "Graphics/32x32/execute.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Start Process" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 65 BY 4 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Purge records through (date)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "To Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE fiDirectory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 64.8 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE tbInactive AS LOGICAL INITIAL no 
     LABEL "Inactivate Customers with no activity since purge date" 
     VIEW-AS TOGGLE-BOX
     SIZE 66 BY .95 NO-UNDO.

DEFINE VARIABLE tbNonZero AS LOGICAL INITIAL no 
     LABEL "Purge Invoices with non-zero balance?" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE tbOpenFile AS LOGICAL INITIAL no 
     LABEL "Open File" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.4 BY .81
     FONT 22 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     btSimulatePurge AT ROW 20.52 COL 20 WIDGET-ID 28
     btStartProcess AT ROW 20.52 COL 47.2 WIDGET-ID 12
     EDITOR-1 AT ROW 5.29 COL 15 NO-LABEL WIDGET-ID 10 NO-TAB-STOP 
     tbNonZero AT ROW 9.81 COL 21
     end_date AT ROW 11 COL 49 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_cust-no AT ROW 12.67 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 12.67 COL 54 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_inv AT ROW 14.1 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 14.1 COL 54 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     tbInactive AT ROW 15.76 COL 21 WIDGET-ID 36
     fiDirectory AT ROW 18.57 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     tbOpenFile AT ROW 18.67 COL 70.4 WIDGET-ID 26
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Records to view  will be stored in directory:" VIEW-AS TEXT
          SIZE 50.6 BY .62 AT ROW 17.67 COL 11.4 WIDGET-ID 16
          FONT 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 21.33.

DEFINE FRAME FRAME-B
     "and may take hours to complete!" VIEW-AS TEXT
          SIZE 46 BY .95 AT ROW 3.62 COL 21
          FONT 17
     "You MUST perform a database backup before running this" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.24 COL 4
          FONT 17
     "procedure.  Please note this process is VERY time intensive" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 2.43 COL 4 WIDGET-ID 2
          FONT 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 14 FGCOLOR 12  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge AR Invoices"
         HEIGHT             = 21.33
         WIDTH              = 89.6
         MAX-HEIGHT         = 22.52
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 22.52
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge AR Invoices */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge AR Invoices */
DO:
   IF VALID-HANDLE(hdOutputProcs) THEN 
      DELETE PROCEDURE hdOutputProcs.
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSimulatePurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSimulatePurge C-Win
ON CHOOSE OF btSimulatePurge IN FRAME FRAME-A /* Simulate */
DO:
        RUN pRunProcess(
            INPUT NO
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btStartProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btStartProcess C-Win
ON CHOOSE OF btStartProcess IN FRAME FRAME-A /* Start Process */
DO:
         MESSAGE 
        "Are you sure you want to permanently delete the selected records?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lProcess AS LOG.

        IF lProcess 
        AND tbNonZero:checked in frame {&frame-name} THEN DO:
            UPDATE 
                cDestroy LABEL "Enter 'DESTROY' to confirm purge"
                WITH FRAME b VIEW-AS DIALOG-BOX THREE-D.
            IF cDestroy NE "DESTROY" THEN DO:
                MESSAGE
                    "Purge Cancelled."
                    VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
        END.
        ELSE IF NOT lProcess THEN DO:
            MESSAGE
                "Purge Cancelled."
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    
        RUN pRunProcess(
            INPUT YES
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Purge records through (date) */
DO:
    IF DATE(SELF:SCREEN-VALUE) > DATE(STRING(MONTH(TODAY),"99") + "/" +
                                STRING(DAY(TODAY),"99") + "/" +
                                STRING(YEAR(TODAY) - 7,"9999")) THEN DO:
        MESSAGE
            "It is unusual to purge financial data more" SKIP
            "recent than 7 years.  Are you sure?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lSure AS LOG.
        IF NOT lSure THEN DO:
            ASSIGN
                SELF:SCREEN-VALUE = STRING(MONTH(TODAY),"99") + "/" +
                                    STRING(DAY(TODAY),"99") + "/" +
                                    STRING(YEAR(TODAY) - 7,"9999").
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* check security */
    IF access-close THEN DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    
    FIND ar-ctrl NO-LOCK WHERE 
        ar-ctrl.company = gcompany 
        NO-ERROR.
  
  RUN enable_UI.

    ASSIGN
        editor-1:SCREEN-VALUE = "This process will purge ALL posted AR invoices " +
                                "and their associated details based on the criteria " +
                                "you select below. Purged records include: invoices, " +
                                "invoice lines, cash payments against invoices, and " +
                                "all associated cash payment details. Invoices not " +
                                "marked as POSTED will NOT be purged."
        end_date:SCREEN-VALUE = STRING(MONTH(TODAY),"99") + "/" +
                                STRING(DAY(TODAY),"99") + "/" +
                                STRING(YEAR(TODAY) - 7,"9999").
    fiDirectory:SCREEN-VALUE = ".\custfiles\dump\AR\PurgeArInvoice".                            
    
    RUN FileSys_CreateDirectory(
            INPUT  fiDirectory,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).   
                                
    APPLY 'entry' to tbNonZero.                            
    {methods/nowait.i}
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY EDITOR-1 tbNonZero end_date begin_cust-no end_cust-no begin_inv 
          end_inv tbInactive fiDirectory tbOpenFile 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE btSimulatePurge btStartProcess tbNonZero end_date begin_cust-no 
         end_cust-no begin_inv end_inv tbInactive fiDirectory tbOpenFile 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerInActive C-Win 
PROCEDURE pCustomerInActive PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Private procedure to delete orphan records 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    
    FOR EACH bf-cust EXCLUSIVE-LOCK 
        WHERE bf-cust.company EQ ipcCompany
          AND bf-cust.ACTIVE  NE "I"
          AND bf-cust.ACTIVE  NE "X"
          AND NOT bf-cust.internal
          AND bf-cust.cust-no EQ ipcCustomer
          AND bf-cust.balanceCurrent EQ 0
          AND NOT CAN-FIND(FIRST ar-inv NO-LOCK
              WHERE ar-inv.company EQ bf-cust.company
                AND ar-inv.cust-no EQ bf-cust.cust-no) 
          AND NOT CAN-FIND(FIRST oe-ord NO-LOCK
              WHERE oe-ord.company EQ bf-cust.company
                AND oe-ord.cust-no EQ bf-cust.cust-no
                AND oe-ord.opened EQ YES)      
                USE-INDEX ACTIVE:
                          
              bf-cust.ACTIVE = "I".                  
    END.      
        
    RELEASE bf-cust.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgeArLedger C-Win 
PROCEDURE pPurgeArLedger PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Private procedure to delete ar-ledger records 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDate        AS DATE NO-UNDO.
                      
    FOR EACH ar-ledger EXCLUSIVE WHERE
                ar-ledger.company EQ ipcCompany AND
                ar-ledger.tr-date LE ipdtDate:                        
                       
                {custom/statusMsg.i " 'Customer:'  + ar-ledger.cust-no "}       
                EXPORT STREAM out5 ar-ledger.                                  
                DELETE ar-ledger.
               
                ASSIGN
                    iPurgeCount5 = iPurgeCount5 + 1.
    END. 
    FOR EACH ar-mcash EXCLUSIVE-LOCK
        WHERE ar-mcash.company   EQ ipcCompany
          AND check-date LE ipdtDate
          USE-INDEX m-cash:                  
      
         EXPORT STREAM out6 ar-mcash.                          
         DELETE ar-mcash.
         iPurgeCount6 = iPurgeCount6 + 1.
                              
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPurgeOrphanRecords C-Win 
PROCEDURE pPurgeOrphanRecords PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Private procedure to delete orphan records 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ar-cashl FOR ar-cashl.        
    
    /* Delete ar-ledger records with no customer ,*/
    FOR EACH ar-ledger EXCLUSIVE-LOCK 
        WHERE ar-ledger.company EQ ipcCompany
          AND ar-ledger.cust-no  EQ ""           
          : 
           FOR EACH ar-mcash EXCLUSIVE-LOCK
                WHERE ar-mcash.company   EQ ipcCompany
                  AND STRING(ar-mcash.m-no) + " " + ar-mcash.payer EQ ar-ledger.ref-num
                  :
                                   
                 EXPORT STREAM out6 ar-mcash.                                   
                 DELETE ar-mcash.
                 iPurgeCount6 = iPurgeCount6 + 1.                                        
            END.   
                         
            EXPORT STREAM out5 ar-ledger.                              
            DELETE ar-ledger.

            ASSIGN
                iPurgeCount5 = iPurgeCount5 + 1.                                     
    END. 
    
    RELEASE ar-cashl.
    RELEASE ar-cash.
         
    MAIN-LOOP:
    FOR EACH ar-cashl NO-LOCK 
        WHERE ar-cashl.company  EQ ipcCompany
          AND ar-cashl.posted   EQ yes 
          AND ar-cashl.cust-no  GE begin_cust-no
          AND ar-cashl.cust-no  LE end_cust-no 
          AND ar-cashl.inv-no   EQ 0
          USE-INDEX inv-no:
                    
        FIND ar-cash NO-LOCK WHERE
            ar-cash.c-no EQ ar-cashl.c-no
            USE-INDEX c-no NO-ERROR.
        
        IF AVAIL ar-cash THEN DO:
            IF ar-cash.check-date > end_date THEN
                NEXT MAIN-LOOP.  
            FIND FIRST bf-ar-cashl NO-LOCK WHERE
                bf-ar-cashl.c-no EQ ar-cashl.c-no AND
                bf-ar-cashl.company EQ ar-cashl.company AND                 
                ROWID(bf-ar-cashl) NE ROWID(ar-cashl)
                NO-ERROR.  
            IF NOT AVAIL bf-ar-cashl THEN DO:                                                  
                EXPORT STREAM out3 ar-cash.
                FIND CURRENT ar-cash EXCLUSIVE-LOCK NO-ERROR.
                DELETE ar-cash.
                     
                ASSIGN
                    iPurgeCount = iPurgeCount + 1.
            END.
        END. 
                          
        EXPORT STREAM out4 ar-cashl.
        FIND CURRENT ar-cashl EXCLUSIVE-LOCK NO-ERROR.
        DELETE ar-cashl.
        
        ASSIGN
            iPurgeCount2 = iPurgeCount2 + 1.
    END.        
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExecute AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE cOutputArInv AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputArInvl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputArCash AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputArCashl AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputArLedger AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOutputArMcash  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustNo         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ar-inv FOR ar-inv.
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    DEFINE BUFFER bf-ar-cashl FOR ar-cashl.
    
    DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
    END.
    EMPTY TEMP-TABLE ttPurgeInvCsv.
    ASSIGN    
        iPurgeCount  = 0
        iPurgeCount2 = 0
        iPurgeCount3 = 0
        iPurgeCount4 = 0
        iPurgeCount5 = 0
        iPurgeCount6 = 0
        totWriteOff  = 0
        .
    
    cFileName = "\PurageArInvoice" + STRING(year(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME) + ".csv".
    
    cOutputArInv = fiDirectory + "\ar-inv" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d"  .
                                
     cOutputArInvl = fiDirectory +  "\ar-invl" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d"  .
                                
     cOutputArCash = fiDirectory +  "\ar-cash" + 
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d"  .
                                
      cOutputArCashl = fiDirectory + "\ar-cashl" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d" .
                                
      cOutputArLedger = fiDirectory + "\ar-ledger" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d" . 
                                
      cOutputArMcash = fiDirectory + "\ar-mcash" +
                                STRING(YEAR(TODAY),"9999") +
                                STRING(MONTH(TODAY),"99") +
                                STRING(DAY(TODAY),"99") + ".d" .                            
                              
      
    RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArInv,
        INPUT  YES,    
        OUTPUT cOutputArInv, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ).  
        
    RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArInvl,
        INPUT  YES,    
        OUTPUT cOutputArInvl, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ). 
        
    RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArCash,
        INPUT  YES,    
        OUTPUT cOutputArCash, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ). 
        
    RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArCashl,
        INPUT  YES,    
        OUTPUT cOutputArCashl, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ).         
    
    RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArLedger,
        INPUT  YES,    
        OUTPUT cOutputArLedger, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ).
   
   RUN FileSys_GetUniqueFileName (
        INPUT  cOutputArMcash,
        INPUT  YES,    
        OUTPUT cOutputArMcash, 
        OUTPUT lSuccess, 
        OUTPUT cMessage  
        ).         
        
    RUN FileSys_CreateDirectory(
            INPUT  fiDirectory,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).      
    IF ipcExecute THEN 
    DO:   
        OUTPUT STREAM out1 TO VALUE(cOutputArInv).
        OUTPUT STREAM out2 TO VALUE(cOutputArInvl).
        OUTPUT STREAM out3 TO VALUE(cOutputArCash).
        OUTPUT STREAM out4 TO VALUE(cOutputArCashl).
        OUTPUT STREAM out5 TO VALUE(cOutputArLedger).
        OUTPUT STREAM out6 TO VALUE(cOutputArMcash).
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    SESSION:SET-WAIT-STATE ("general").

    DISABLE TRIGGERS FOR LOAD OF ar-inv.
    DISABLE TRIGGERS FOR LOAD OF ar-invl.
    DISABLE TRIGGERS FOR LOAD OF ar-cash.
    DISABLE TRIGGERS FOR LOAD OF ar-cashl.
    DISABLE TRIGGERS FOR LOAD OF ar-ledger.
    DISABLE TRIGGERS FOR LOAD OF ar-mcash.

    MAIN-LOOP:
    FOR EACH bf-ar-inv WHERE 
        bf-ar-inv.company  EQ cocode AND 
        bf-ar-inv.inv-date LE end_date AND 
        bf-ar-inv.inv-no   GE begin_inv AND
        bf-ar-inv.inv-no   LE end_inv AND
        bf-ar-inv.cust-no  GE begin_cust-no AND
        bf-ar-inv.cust-no  LE end_cust-no AND
        bf-ar-inv.posted   EQ yes
        NO-LOCK USE-INDEX posted 
        BREAK BY bf-ar-inv.cust-no:
        
        {custom/statusMsg.i " 'Customer:'  + bf-ar-inv.cust-no + '  Invoice:'  + string(bf-ar-inv.inv-no) "}
        
        IF LAST-OF(bf-ar-inv.cust-no) THEN
        cCustNo = bf-ar-inv.cust-no.

        ASSIGN
            invBalAmt = IF bf-ar-inv.net = bf-ar-inv.gross + bf-ar-inv.freight + bf-ar-inv.tax-amt THEN
                    bf-ar-inv.net 
                  ELSE 
                    bf-ar-inv.gross.        
          
        FOR EACH ar-cashl NO-LOCK WHERE
            ar-cashl.company  EQ bf-ar-inv.company AND
            ar-cashl.posted   EQ yes AND
            ar-cashl.cust-no  EQ bf-ar-inv.cust-no AND
            ar-cashl.inv-no   EQ bf-ar-inv.inv-no
            USE-INDEX inv-no:

            
            FIND FIRST ar-cash NO-LOCK WHERE
                ar-cash.c-no EQ ar-cashl.c-no
                USE-INDEX c-no NO-ERROR.
            
            /* If ar-cashl is an orphan, delete it */
            IF NOT AVAIL ar-cash THEN DO:
                FIND bf-ar-cashl EXCLUSIVE WHERE 
                    ROWID(bf-ar-cashl) EQ ROWID(ar-cashl)
                    NO-ERROR.
                IF AVAIL bf-ar-cashl THEN 
                    DELETE bf-ar-cashl.
                NEXT.
            END.
            
            IF ar-cash.check-date > end_date THEN
                NEXT MAIN-LOOP.

            IF ar-cashl.memo THEN DO:
                IF ar-cashl.dscr BEGINS "CREDIT MEMO CREATED FROM OE RETURN" THEN ASSIGN
                    invBalAmt = invBalAmt - ar-cashl.amt-disc.
                ELSE IF ar-cashl.amt-paid + ar-cashl.amt-disc <> 0 THEN ASSIGN
                    invBalAmt = invBalAmt + (ar-cashl.amt-paid + ar-cashl.amt-disc).
                ELSE ASSIGN
                    invBalAmt = invBalAmt + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END.
            ELSE ASSIGN
                invBalAmt = invBalAmt + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1)).
        END. 
         
        IF invBalAmt = 0 
        OR tbNonZero THEN DO:
        
            CREATE ttPurgeInvCsv.
            ASSIGN
                ttPurgeInvCsv.company = bf-ar-inv.company
                ttPurgeInvCsv.iInvNo  = bf-ar-inv.inv-no
                ttPurgeInvCsv.cCustNo = bf-ar-inv.cust-no
                ttPurgeInvCsv.dtInvDate = bf-ar-inv.inv-date
                ttPurgeInvCsv.dInvAmount = invBalAmt 
                ttPurgeInvCsv.dBalDue    = bf-ar-inv.due
                ttPurgeInvCsv.cNote      = "".
        
            ASSIGN
                totWriteOff = totWriteOff + invBalAmt.
            
            IF ipcExecute THEN    
            FOR EACH ar-cashl NO-LOCK WHERE
                ar-cashl.company  EQ bf-ar-inv.company AND
                ar-cashl.posted   EQ yes AND
                ar-cashl.cust-no  EQ bf-ar-inv.cust-no AND
                ar-cashl.inv-no   EQ bf-ar-inv.inv-no
                USE-INDEX inv-no:

                FIND ar-cash NO-LOCK WHERE
                    ar-cash.c-no EQ ar-cashl.c-no
                    USE-INDEX c-no NO-ERROR.
                
                IF AVAIL ar-cash THEN DO:
                    FIND FIRST b-ar-cashl NO-LOCK WHERE
                        b-ar-cashl.c-no EQ ar-cashl.c-no AND 
                        ROWID(b-ar-cashl) NE ROWID(ar-cashl)
                        NO-ERROR.
                    IF NOT AVAIL b-ar-cashl THEN DO:                                                  
                        EXPORT STREAM out3 ar-cash.
                        FIND CURRENT ar-cash EXCLUSIVE-LOCK NO-ERROR.
                        DELETE ar-cash.
                        
                        ASSIGN
                            iPurgeCount = iPurgeCount + 1.
                    END.
                END. 
                                   
                EXPORT STREAM out4 ar-cashl.
                FIND CURRENT ar-cashl EXCLUSIVE-LOCK NO-ERROR.
                DELETE ar-cashl.
                
                ASSIGN
                    iPurgeCount2 = iPurgeCount2 + 1.
            END.

            IF ipcExecute THEN
            FOR EACH bf-ar-invl NO-LOCK WHERE
                bf-ar-invl.company EQ cocode AND
                bf-ar-invl.x-no EQ bf-ar-inv.x-no USE-INDEX x-no:
                               
                EXPORT STREAM out2 bf-ar-invl.
                FIND FIRST ar-invl WHERE ROWID(ar-invl) EQ ROWID(bf-ar-invl) EXCLUSIVE-LOCK NO-ERROR.
                DELETE ar-invl.
                ttPurgeInvCsv.cNote      = "Invoice Deleted".
                
                ASSIGN
                    iPurgeCount3 = iPurgeCount3 + 1.
            END.
                                                
            IF ipcExecute THEN 
            DO:
                EXPORT STREAM out1 bf-ar-inv.
                FIND FIRST ar-inv WHERE ROWID(ar-inv) EQ ROWID(bf-ar-inv) EXCLUSIVE-LOCK NO-ERROR.
                DELETE ar-inv.
                IF cCustNo NE "" AND tbInactive THEN do:
                   RUN pCustomerInActive(cocode, cCustNo).
                   cCustNo = "".
                END.
            END.
            ASSIGN
                iPurgeCount4 = iPurgeCount4 + 1.
        END.  
    END.
    
    IF ipcExecute THEN 
    do:                         
        RUN pPurgeArLedger(cocode, end_date).     
        RUN pPurgeOrphanRecords(cocode).
    END.    
        
    SESSION:SET-WAIT-STATE("").
    
    IF ipcExecute THEN 
    DO: 
        OUTPUT STREAM out1 CLOSE.
        OUTPUT STREAM out2 CLOSE.
        OUTPUT STREAM out3 CLOSE.
        OUTPUT STREAM out4 CLOSE.
        OUTPUT STREAM out5 CLOSE.
        OUTPUT STREAM out6 CLOSE.
    END.
    
    STATUS DEFAULT "Processing Complete".
           
    RUN Output_TempTableToCSV IN hdOutputProcs (
        INPUT TEMP-TABLE ttPurgeInvCsv:HANDLE,
        INPUT fiDirectory + cFileName,
        INPUT TRUE,  /* Export Header */
        INPUT FALSE, /* Auto increment File name */
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
    
    IF NOT ipcExecute THEN     
    MESSAGE "Simulation Completed." SKIP
        "Check " + TRIM(fiDirectory) + " directory for the CSV file."
        VIEW-AS ALERT-BOX INFORMATION.  
        
   IF tbOpenFile:CHECKED THEN 
        RUN OS_RunFile(
            INPUT fiDirectory + cFileName,
            OUTPUT lSuccess,
            OUTPUT cMessage).  
   
   
    IF ipcExecute THEN
    MESSAGE  
        "Purge process completed." SKIP
        STRING(iPurgeCount4) + " invoices records were deleted." SKIP
        STRING(iPurgeCount3) + " invoice line records were deleted." SKIP
        STRING(iPurgeCount) + " ar-cash records were deleted." SKIP
        STRING(iPurgeCount2) + " ar-cashl records were deleted." SKIP
         STRING(iPurgeCount5) + " ar-ledger records were deleted." SKIP
         STRING(iPurgeCount6) + " ar-mcash records were deleted." SKIP
        STRING(totWriteOff,"->>>,>>>,>>9.99") + " dollars were written off." SKIP
        "Backup files were placed in then C:\tmp directory" SKIP
        "for retrieval if necessary."
        VIEW-AS ALERT-BOX.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


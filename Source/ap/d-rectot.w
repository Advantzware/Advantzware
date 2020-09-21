&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = g_company
    locode = g_loc.

DEFINE NEW SHARED VARIABLE v-tax-rate     AS DECIMAL.
DEFINE NEW SHARED VARIABLE v-frt-tax-rate AS DECIMAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 begin_vend tb_acct-bal tb_avg-days ~
tb_high-bal Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend tb_acct-bal tb_avg-days ~
tb_high-bal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 FONT 6.

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "&OK" 
    SIZE 15 BY 1.14
    BGCOLOR 8 FONT 6.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
    LABEL "Vendor" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-25
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 64 BY 5.48.

DEFINE VARIABLE tb_acct-bal AS LOGICAL INITIAL YES 
    LABEL "Recalculate Account Balances" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE tb_avg-days AS LOGICAL INITIAL NO 
    LABEL "Recalculate Average Days To Pay" 
    VIEW-AS TOGGLE-BOX
    SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE tb_high-bal AS LOGICAL INITIAL NO 
    LABEL "Recalculate High Balance" 
    VIEW-AS TOGGLE-BOX
    SIZE 48 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    begin_vend AT ROW 1.86 COL 15 COLON-ALIGNED HELP
    "Please enter vendor number to be processed"
    tb_acct-bal AT ROW 3.48 COL 6 WIDGET-ID 2
    tb_avg-days AT ROW 4.48 COL 6 WIDGET-ID 4
    tb_high-bal AT ROW 5.43 COL 6 WIDGET-ID 6
    Btn_OK AT ROW 6.95 COL 11
    Btn_Cancel AT ROW 6.95 COL 41
    "Enter * for all vendor" VIEW-AS TEXT
    SIZE 28 BY 1 AT ROW 1.86 COL 33
    FONT 6
    RECT-25 AT ROW 1.24 COL 1
    SPACE(0.19) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Recalculate Vendor Acct Balances"
    DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Recalculate Vendot Acct Balances */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend Dialog-Frame
ON LEAVE OF begin_vend IN FRAME Dialog-Frame /* Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
    DO:
        DEFINE VARIABLE lProcess AS LOG INIT NO NO-UNDO.
        
        RUN validateInputs(OUTPUT lProcess).  
        IF lProcess THEN RUN runProcess.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_acct-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_acct-bal Dialog-Frame
ON VALUE-CHANGED OF tb_acct-bal IN FRAME Dialog-Frame /* Recalculate Account Balances */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_avg-days
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_avg-days Dialog-Frame
ON VALUE-CHANGED OF tb_avg-days IN FRAME Dialog-Frame /* Recalculate Average Days To Pay */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_high-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_high-bal Dialog-Frame
ON VALUE-CHANGED OF tb_high-bal IN FRAME Dialog-Frame /* Recalculate High Balance */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND vend WHERE ROWID(vend) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE vend THEN begin_vend = vend.vend-no.

    RUN enable_UI.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
    HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
    DISPLAY begin_vend tb_acct-bal tb_avg-days tb_high-bal 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-25 begin_vend tb_acct-bal tb_avg-days tb_high-bal Btn_OK 
        Btn_Cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runProcess Dialog-Frame 
PROCEDURE runProcess :
    /* ---------------------------------------------------  */
    /* -------------------------------------------------------------------------- */
    
    DEFINE VARIABLE as_of_date   AS DATE      NO-UNDO.
    DEFINE VARIABLE dtCheckDate  AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmount      AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-refnum     AS CHARACTER NO-UNDO.

    DEFINE BUFFER xap-ledger FOR ap-ledger.
    DEFINE BUFFER b-vend     FOR vend.


    SESSION:SET-WAIT-STATE("General").
    as_of_date = TODAY .

    FOR EACH vend NO-LOCK
        WHERE vend.company EQ cocode
        AND (begin_vend EQ "*" OR vend.vend-no EQ begin_vend):

        STATUS DEFAULT "Please wait...  Updating Vendor: " + TRIM(vend.vend-no).

        IF tb_acct-bal OR tb_avg-days OR tb_high-bal THEN 
        DO:    
            ASSIGN
                dAmount = 0
                .      
       
            FOR EACH ap-inv NO-LOCK
                WHERE ap-inv.company   EQ vend.company
                AND ap-inv.vend-no   EQ vend.vend-no
                AND ap-inv.posted    EQ YES
                AND (ap-inv.inv-date LE as_of_date )
                USE-INDEX ap-inv ,
    
                FIRST ap-ledger NO-LOCK 
                WHERE ap-ledger.company  EQ vend.company
                AND ap-ledger.vend-no  EQ ap-inv.vend-no
                AND ap-ledger.ref-date EQ ap-inv.inv-date
                AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
                AND (ap-ledger.tr-date LE as_of_date )
                USE-INDEX ap-ledger
    
                BREAK BY ap-inv.vend-no
                BY (ap-ledger.tr-date)
                BY ap-inv.inv-no:
          
          
                FOR EACH ap-payl NO-LOCK
                    WHERE ap-payl.inv-no   EQ ap-inv.inv-no
                    AND ap-payl.vend-no  EQ ap-inv.vend-no
                    AND ap-payl.posted   EQ YES 
                    AND ap-payl.due-date EQ ap-inv.due-date
                    USE-INDEX inv-no:

                    FIND FIRST ap-pay NO-LOCK
                        WHERE ap-pay.company EQ vend.company
                        AND ap-pay.c-no EQ ap-payl.c-no
                        USE-INDEX c-no NO-ERROR.

                    IF AVAILABLE ap-pay THEN
                    DO:       
                        dtCheckDate = ap-pay.check-date.
                        /*check for voided check transaction date*/
                        IF ap-payl.amt-paid LT 0  AND
                            ap-payl.memo                EQ NO AND
                            ap-inv.net + ap-inv.freight GT 0 THEN
                        DO:
                            v-refnum = "VOIDED CHECK"
                                + STRING(ap-pay.check-no, "zzzzzzz9").

                            FIND FIRST xap-ledger NO-LOCK WHERE
                                xap-ledger.company EQ vend.company AND
                                xap-ledger.vend-no EQ ap-pay.vend-no AND
                                xap-ledger.refnum  EQ v-refnum
                                NO-ERROR.

                            IF AVAILABLE xap-ledger THEN
                            DO:
                                dtCheckDate = xap-ledger.tr-date.
                                RELEASE xap-ledger.
                            END.
                        END.

                        IF dtCheckDate LE as_of_date THEN 
                        DO:
                            IF ap-payl.amt-paid NE 0 THEN dAmount = dAmount - ap-payl.amt-paid.
                            IF ap-payl.amt-disc NE 0 THEN 
                            DO:
                                IF NOT ap-payl.memo THEN dAmount = dAmount - ap-payl.amt-disc.
                                IF ap-payl.memo THEN dAmount = dAmount + ap-payl.amt-disc.
                            END.
                        END.
                    END.

                    RELEASE ap-pay.
    
                END. /* for each ap-payl */
  
                dAmount = dAmount + ap-inv.net + ap-inv.freight .
                FIND FIRST b-vend EXCLUSIVE WHERE ROWID(b-vend) EQ ROWID(vend)
                NO-ERROR NO-WAIT.
                IF tb_acct-bal THEN
                   b-vend.acc-bal =  dAmount .  
                   
                IF tb_high-bal AND dAmount GE vend.hibal THEN
                 ASSIGN
                 vend.hibal      = dAmount
                 vend.hibal-date = ap-inv.inv-date.
                  
                
                 IF tb_avg-days THEN 
                 DO:
                    IF ap-inv.due      LE 0 AND
                       ap-inv.pay-date NE ? AND
                       ap-inv.inv-date NE ? THEN
                       
                 ASSIGN
                   b-vend.avg-pay = ((vend.avg-pay * vend.num-inv) +
                                 (ap-inv.pay-date - ap-inv.inv-date)) /
                                 (vend.num-inv + 1) .                     
                 END.  /*tb_avg-days */                 
            END.    /* for each ap-inv */
        END. /*if tb_acct-bal or tb_avg-days*/              
        
    END. /*each vend*/

    SESSION:SET-WAIT-STATE("").  


    MESSAGE "Recalculation Process Is Completed."
        VIEW-AS ALERT-BOX.

    APPLY "close" TO THIS-PROCEDURE.
    
/* end ---------------------------------- copr. 2002  Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateInputs Dialog-Frame 
PROCEDURE validateInputs :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplProcess AS LOG INIT NO.

    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    IF tb_acct-bal THEN 
    DO:
        cMessage = "recalculate account balances".
        IF tb_avg-days AND tb_high-bal THEN
            cMessage = cMessage + ", average days to pay and high balance".
        IF tb_avg-days AND NOT tb_high-bal THEN
            cMessage = cMessage + " and average days to pay".
        IF NOT tb_avg-days AND tb_high-bal THEN
            cMessage = cMessage + " and high balance".
    END.
    ELSE IF tb_avg-days OR tb_high-bal THEN 
        DO: 
            IF tb_avg-days AND tb_high-bal THEN
                cMessage = cMessage + "recalculate average days to pay and high balance".
            IF tb_avg-days AND NOT tb_high-bal THEN 
                cMessage = "recalculate average days to pay".
            IF NOT tb_avg-days AND tb_high-bal THEN
                cMessage = "recalculate high balance".
        END.
        ELSE 
        DO:
            MESSAGE "You must select a recalculation option."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            RETURN NO-APPLY.
        END.
    IF begin_vend = "*" THEN
        cMessage = cMessage + " for all Vendor?".
    ELSE IF begin_vend NE "" THEN
            cMessage = cMessage + " for vendor " + begin_vend + "?".
        ELSE 
        DO:
            MESSAGE "You must enter a Vendor."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            APPLY "entry" TO begin_vend IN FRAME {&FRAME-NAME}.
            RETURN.
        END.
    
    MESSAGE "Are you sure you want to " + cMessage
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE oplProcess.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


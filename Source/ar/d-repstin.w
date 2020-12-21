&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowid  AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iprRowid2 AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER iopArRowid AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER ioplCancel AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE ldummy AS LOG     NO-UNDO.
DEFINE VARIABLE i      AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS  sman_first sman_per1 sman_com1 ~
sman_sec sman_per2 sman_com2 sman_third sman_per3 sman_com3 cust-po Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS sman_first sman_first-dscr sman_per1 ~
sman_com1 sman_sec sman_sec-dscr sman_per2 sman_com2 sman_third ~
sman_third-dscr sman_per3 sman_com3 cust-po 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "OK" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE cust-po         AS CHARACTER FORMAT "X(15)":U 
    LABEL "Customer Po#" 
    VIEW-AS FILL-IN 
    SIZE 33.2 BY 1 
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_com1       AS DECIMAL   FORMAT ">>9.99" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_com2       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO  .

DEFINE VARIABLE sman_com3       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO  .

DEFINE VARIABLE sman_first      AS CHARACTER FORMAT "X(3)" 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_first-dscr AS CHARACTER FORMAT "X(30)" 
    VIEW-AS FILL-IN 
    SIZE 32.2 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_per1       AS DECIMAL   FORMAT ">>9.99" INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_per2       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO  .

DEFINE VARIABLE sman_per3       AS DECIMAL   FORMAT ">>9.99":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 11 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO  .

DEFINE VARIABLE sman_sec        AS CHARACTER FORMAT "X(3)":U 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO  .

DEFINE VARIABLE sman_sec-dscr   AS CHARACTER FORMAT "X(30)" 
    VIEW-AS FILL-IN 
    SIZE 32.2 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_third      AS CHARACTER FORMAT "X(3)":U 
    VIEW-AS FILL-IN 
    SIZE 9 BY 1 
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE VARIABLE sman_third-dscr AS CHARACTER FORMAT "X(30)" 
    VIEW-AS FILL-IN 
    SIZE 32.2 BY 1
    /*BGCOLOR 15 FONT 1*/ NO-UNDO .

DEFINE RECTANGLE RECT-30
    EDGE-PIXELS 1 GRAPHIC-EDGE  ROUNDED   
    SIZE 79.8 BY 6.91  .

DEFINE RECTANGLE RECT-20
    EDGE-PIXELS 1 GRAPHIC-EDGE  ROUNDED   
    SIZE 19 BY 2.38  .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    sman_first AT ROW 2.86 COL 4.2 COLON-ALIGNED NO-LABELS
    sman_first-dscr AT ROW 2.86 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 64
    sman_per1 AT ROW 2.86 COL 48.2 COLON-ALIGNED NO-LABELS WIDGET-ID 70
    sman_com1 AT ROW 2.86 COL 57.8 COLON-ALIGNED NO-LABELS WIDGET-ID 76
    sman_sec AT ROW 3.95 COL 4.2 COLON-ALIGNED NO-LABELS
    sman_sec-dscr AT ROW 3.95 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 66
    sman_per2 AT ROW 3.95 COL 48.2 COLON-ALIGNED NO-LABELS WIDGET-ID 72
    sman_com2 AT ROW 3.95 COL 57.8 COLON-ALIGNED NO-LABELS WIDGET-ID 78
    sman_third AT ROW 5.05 COL 4.2 COLON-ALIGNED NO-LABELS
    sman_third-dscr AT ROW 5.05 COL 14.6 COLON-ALIGNED NO-LABELS WIDGET-ID 68
    sman_per3 AT ROW 5.05 COL 48.2 COLON-ALIGNED NO-LABELS WIDGET-ID 74
    sman_com3 AT ROW 5.05 COL 57.8 COLON-ALIGNED NO-LABELS WIDGET-ID 80
    cust-po AT ROW 6.33 COL 19 COLON-ALIGNED WIDGET-ID 82
    Btn_OK AT ROW 8.39 COL 62.9
    Btn_Cancel AT ROW 8.39 COL 72.4
    "Sales" VIEW-AS TEXT
    SIZE 7 BY .81 AT ROW 1.86 COL 6.8 WIDGET-ID 38
    "% Comm" VIEW-AS TEXT
    SIZE 11 BY .81 AT ROW 1.86 COL 59.6 WIDGET-ID 62
    "% Sale" VIEW-AS TEXT
    SIZE 8 BY .81 AT ROW 1.86 COL 50.6 WIDGET-ID 60
    "Name" VIEW-AS TEXT
    SIZE 7 BY .81 AT ROW 1.86 COL 18 WIDGET-ID 58
    RECT-30 AT ROW 1.19 COL 1.2 
    RECT-20 AT ROW 8.20 COL 62 
    SPACE(1.39) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Update Sales Details"
    /*DEFAULT-BUTTON Btn_OK*/ CANCEL-BUTTON Btn_Cancel  .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN sman_first-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman_sec-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman_third-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF  FRAME Dialog-Frame
    DO:
        DEFINE VARIABLE li            AS INTEGER       NO-UNDO.
        DEFINE VARIABLE lw-focus      AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE cFieldsValue  AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cFoundValue   AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE recFoundRecID AS RECID         NO-UNDO.
        
        RUN system/openLookup.p (
            INPUT  ar-inv.company, 
            INPUT  "",  /* Lookup ID */
            INPUT  29,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).
        lw-focus = FOCUS.

        CASE lw-focus:NAME :          
            WHEN "sman_first" THEN 
                DO:
                    sman_first:screen-value      = cFoundValue.
                    sman_first-dscr:screen-value = DYNAMIC-FUNCTION("sfDynLookupValue", "sname", cFieldsValue).
                END.
            WHEN "sman_sec" THEN 
                DO:
                    sman_sec:screen-value      = cFoundValue.
                    sman_sec-dscr:screen-value = DYNAMIC-FUNCTION("sfDynLookupValue", "sname", cFieldsValue).
                END.
            WHEN "sman_third" THEN 
                DO:
                    sman_third:screen-value      = cFoundValue.
                    sman_third-dscr:screen-value = DYNAMIC-FUNCTION("sfDynLookupValue", "sname", cFieldsValue).
                END.
        END CASE.

        APPLY "entry" TO lw-focus.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bank Info */
    DO:
        ioplCancel = YES .   
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* OK */
    DO:
        iopArRowid = iprRowid2 .
        ioplCancel = YES .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.

            RUN valid-sman (0) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-s-pct (0) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            FIND FIRST ar-invl EXCLUSIVE-LOCK WHERE ROWID(ar-invl) EQ iprRowid2  NO-ERROR .
            IF AVAILABLE ar-invl THEN 
            DO:
                ASSIGN
                    ar-invl.sman[1]   = sman_first:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                    ar-invl.s-pct[1]  = DECIMAL(sman_per1:SCREEN-VALUE IN FRAME {&FRAME-NAME})  
                    ar-invl.s-comm[1] = DECIMAL(sman_com1:SCREEN-VALUE IN FRAME {&FRAME-NAME})   
                    ar-invl.sman[2]   = sman_sec:SCREEN-VALUE IN FRAME {&FRAME-NAME}    
                    ar-invl.s-pct[2]  = DECIMAL(sman_per2:SCREEN-VALUE IN FRAME {&FRAME-NAME})   
                    ar-invl.s-comm[2] = DECIMAL(sman_com2:SCREEN-VALUE IN FRAME {&FRAME-NAME})   
                    ar-invl.sman[3]   = sman_third:SCREEN-VALUE IN FRAME {&FRAME-NAME}  
                    ar-invl.s-pct[3]  = DECIMAL(sman_per3:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                    ar-invl.s-comm[3] = DECIMAL(sman_com3:SCREEN-VALUE IN FRAME {&FRAME-NAME})   
                    ar-invl.po-no     = cust-po:SCREEN-VALUE IN FRAME {&FRAME-NAME}    .
            END.
            FIND CURRENT ar-invl NO-LOCK NO-ERROR .
        END.
        iopArRowid = iprRowid2 .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME sman_first
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_first Dialog-Frame
ON LEAVE OF sman_first IN FRAME Dialog-Frame /* Slsmn */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-sman (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman_per1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_per1 Dialog-Frame
ON LEAVE OF sman_per1 IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman_sec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_sec Dialog-Frame
ON LEAVE OF sman_sec IN FRAME Dialog-Frame /* Slsmn */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-sman (2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman_per2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_per2 Dialog-Frame
ON LEAVE OF sman_per2 IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman_third
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_third Dialog-Frame
ON LEAVE OF sman_third IN FRAME Dialog-Frame /* Slsmn */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-sman (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman_per3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman_per3 Dialog-Frame
ON LEAVE OF sman_per3 IN FRAME Dialog-Frame /* % of Sale */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
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

    FIND FIRST ar-inv NO-LOCK WHERE ROWID(ar-inv) EQ iprRowid  NO-ERROR .
    FIND FIRST ar-invl NO-LOCK WHERE ROWID(ar-invl) EQ iprRowid2  NO-ERROR .
    IF NOT AVAILABLE ar-invl THEN RETURN .
    IF AVAILABLE ar-invl THEN 
    DO:
        sman_first = ar-invl.sman[1].
        sman_per1  = ar-invl.s-pct[1]. 
        sman_com1  = ar-invl.s-comm[1].
        sman_sec   = ar-invl.sman[2].
        sman_per2  = ar-invl.s-pct[2].
        sman_com2  = ar-invl.s-comm[2].
        sman_third = ar-invl.sman[3].
        sman_per3  = ar-invl.s-pct[3].
        sman_com3  = ar-invl.s-comm[3].
        cust-po = ar-invl.po-no.

        sman_first:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.sman[1]).
        sman_per1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-pct[1]). 
        sman_com1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-comm[1]).
        sman_sec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.sman[2]).
        sman_per2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-pct[2]).
        sman_com2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-comm[2]).
        sman_third:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.sman[3]).
        sman_per3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-pct[3]).
        sman_com3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.s-comm[3]).
        cust-po:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ar-invl.po-no).

        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ ar-invl.company
            AND sman.sman    EQ ar-invl.sman[1]
            NO-ERROR.
        IF AVAILABLE sman THEN 
            sman_first-dscr = sman.sname.
        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ ar-invl.company
            AND sman.sman    EQ ar-invl.sman[2]
            NO-ERROR.
        IF AVAILABLE sman THEN 
            sman_sec-dscr = sman.sname.
        FIND FIRST sman NO-LOCK
            WHERE sman.company EQ ar-invl.company
            AND sman.sman    EQ ar-invl.sman[3]
            NO-ERROR.
        IF AVAILABLE sman THEN 
            sman_third-dscr = sman.sname.
    END.
   
    RUN enable_UI.
    IF ipType EQ "UpdateAll" THEN
        ASSIGN
            cust-po:HIDDEN IN FRAME {&FRAME-NAME} = YES .

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
    DISPLAY sman_first sman_first-dscr sman_per1 sman_com1 sman_sec sman_sec-dscr 
        sman_per2 sman_com2 sman_third sman_third-dscr sman_per3 sman_com3 
        cust-po 
        WITH FRAME Dialog-Frame.
    ENABLE sman_first sman_per1 sman_com1 sman_sec sman_per2 sman_com2 
        sman_third sman_per3 sman_com3 cust-po Btn_OK Btn_Cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


  &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-pct Dialog-Frame 
PROCEDURE valid-s-pct :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll     AS LOG     NO-UNDO.

   
    DO WITH FRAME {&FRAME-NAME}:
        ld-pct = IF ip-int EQ 1 THEN DEC(sman_per1:SCREEN-VALUE )
        ELSE
            IF ip-int EQ 2 THEN DEC(sman_per2:SCREEN-VALUE )
            ELSE
            IF ip-int EQ 3 THEN DEC(sman_per3:SCREEN-VALUE )
            ELSE (DEC(sman_per1:SCREEN-VALUE ) +
            DEC(sman_per2:SCREEN-VALUE ) +
            DEC(sman_per3:SCREEN-VALUE )).

        IF (sman_first:SCREEN-VALUE  NE "" OR
            sman_sec:SCREEN-VALUE  NE "" OR
            sman_third:SCREEN-VALUE  NE "")   AND
            ((ip-int EQ 0 AND ld-pct NE 100) OR
            (ip-int NE 0 AND ld-pct GT 100)) THEN 
        DO:
            IF ip-int EQ 0 THEN
                MESSAGE "Item's Sales Rep Commission % of Sales does not equal 100%, continue?" SKIP(1)
                    "(Please Note: Yes will result in inaccurate totals on some Sales History Reports)"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.
            ELSE
                MESSAGE "Sales Rep Commission % of Sales is over 100%..."
                    VIEW-AS ALERT-BOX ERROR.
            IF NOT ll THEN 
            DO:
                IF ip-int EQ 3 THEN APPLY "entry" TO sman_per3.
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO sman_per2.
                    ELSE APPLY "entry" TO sman_per1.
                RETURN ERROR.
            END.
        END.
    END.
     
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman Dialog-Frame 
PROCEDURE valid-sman :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE li      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-sman LIKE sman.sman NO-UNDO.


    li = ip-int.

    IF li EQ 0 THEN
        ASSIGN
            ip-int = 1
            li     = 3.

    DO ip-int = ip-int TO li WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN sman_third:SCREEN-VALUE 
        ELSE
            IF ip-int EQ 2 THEN sman_sec:SCREEN-VALUE 
            ELSE sman_first:SCREEN-VALUE .
    
        IF lv-sman NE "" THEN 
        DO:
            IF NOT CAN-FIND(FIRST sman
                            WHERE sman.company  EQ ar-invl.company
                              AND sman.sman     EQ lv-sman
                              AND sman.inactive EQ NO) THEN 
            DO:
                MESSAGE "Inactive/Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
                IF ip-int EQ 3 THEN APPLY "entry" TO sman_third .
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO sman_sec .
                    ELSE APPLY "entry" TO sman_first .
                RETURN ERROR.
            END.

            FIND FIRST sman NO-LOCK
                 WHERE sman.company  EQ ar-invl.company
                   AND sman.sman     EQ lv-sman
                   AND sman.inactive EQ NO
                 NO-ERROR.
            IF AVAILABLE sman THEN 
            DO:
                IF ip-int EQ 3 THEN
                    ASSIGN
                        sman_third-dscr:SCREEN-VALUE = sman.sname .
                ELSE
                    IF ip-int EQ 2 THEN
                        ASSIGN
                            sman_sec-dscr:SCREEN-VALUE = sman.sname  .
                    ELSE
                        ASSIGN
                            sman_first-dscr:SCREEN-VALUE = sman.sname .
            END.

        END.

        ELSE 
        DO:
        
            IF ip-int EQ 3 THEN
                ASSIGN
                    sman_per3:SCREEN-VALUE       = "0"
                    sman_com3:SCREEN-VALUE       = "0" 
                    sman_first-dscr:SCREEN-VALUE = sman.sname 
                    .
            ELSE
                IF ip-int EQ 2 THEN
                    ASSIGN
                        sman_per2:SCREEN-VALUE = "0"
                        sman_com2:SCREEN-VALUE = "0".
                ELSE
                    ASSIGN
                        sman_per1:SCREEN-VALUE = "0"
                        sman_com1:SCREEN-VALUE = "0".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


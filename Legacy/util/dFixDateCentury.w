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
DEF VAR cCompany AS CHAR NO-UNDO.

{src/adm2/widgetprto.i}

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
&Scoped-Define ENABLED-OBJECTS Btn_OK EDITOR-1 Btn_Cancel cbCompany ~
tbOrderFiles tbFgFiles tbInvoiceFiles tbRmFiles 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-2 EDITOR-1 cbCompany FILL-IN-3 ~
tbOrderFiles tbFgFiles tbInvoiceFiles tbRmFiles 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFixYear gDialog 
FUNCTION fFixYear RETURNS DATE
  (INPUT daDate AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Select the company where the bad dates are occurring" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Use this function to correct errors in dates where the century is incorrect.  For example, a date that should display as 12/31/2018 is displaying as 12/31/18." 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 102 BY 1.67 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "IMPORTANT!  YOU SHOULD BACK UP YOUR DATABASE BEFORE PERFORMING THIS UPDATE" 
     VIEW-AS FILL-IN 
     SIZE 102 BY 1
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Choose the types of records that have the bad date issue" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE tbFgFiles AS LOGICAL INITIAL no 
     LABEL "Finished goods-related files (includes history)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tbInvoiceFiles AS LOGICAL INITIAL no 
     LABEL "Invoice-related tables" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tbOrderFiles AS LOGICAL INITIAL no 
     LABEL "Order-related tables (includes BOLs)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE tbRmFiles AS LOGICAL INITIAL no 
     LABEL "Raw materials-related files (includes history)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-2 AT ROW 1 COL 9 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.24 COL 119
     EDITOR-1 AT ROW 1.95 COL 11 NO-LABEL
     Btn_Cancel AT ROW 2.48 COL 119
     cbCompany AT ROW 4.1 COL 63 COLON-ALIGNED
     FILL-IN-3 AT ROW 5.76 COL 9 COLON-ALIGNED NO-LABEL
     tbOrderFiles AT ROW 7.19 COL 11
     tbFgFiles AT ROW 7.19 COL 71
     tbInvoiceFiles AT ROW 8.38 COL 11
     tbRmFiles AT ROW 8.38 COL 71
     SPACE(26.39) SKIP(0.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fix dates with invalid century values"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
       EDITOR-1:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-3:READ-ONLY IN FRAME gDialog        = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Fix dates with invalid century values */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
    RUN ipProcessAll IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbCompany gDialog
ON VALUE-CHANGED OF cbCompany IN FRAME gDialog /* Select the company where the bad dates are occurring */
DO:
    ASSIGN 
        cCompany = ENTRY(1, self:screen-value, "-").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

ASSIGN 
    cbCompany:LIST-ITEMS = "".
FOR EACH company:
    cbCompany:ADD-LAST(company.company + "-" + company.name).
END.

APPLY 'entry' TO cbCompany.

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
  DISPLAY FILL-IN-2 EDITOR-1 cbCompany FILL-IN-3 tbOrderFiles tbFgFiles 
          tbInvoiceFiles tbRmFiles 
      WITH FRAME gDialog.
  ENABLE Btn_OK EDITOR-1 Btn_Cancel cbCompany tbOrderFiles tbFgFiles 
         tbInvoiceFiles tbRmFiles 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessAll gDialog 
PROCEDURE ipProcessAll :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF tbOrderFiles:CHECKED IN FRAME {&frame-name} THEN DO :
        FOR EACH oe-ord EXCLUSIVE where oe-ord.company EQ cCompany AND (oe-ord.ord-date GT 09/01/2018 
                               OR oe-ord.ord-date LT 12/31/0100):
            /* Note: do in multiple assigns, else function only evaluates once */
            ASSIGN oe-ord.ord-date = fFixYear(oe-ord.ord-date).
            ASSIGN oe-ord.prod-date = fFixYear(oe-ord.prod-date).
            ASSIGN oe-ord.due-date = fFixYear(oe-ord.due-date).
            ASSIGN oe-ord.last-date = fFixYear(oe-ord.last-date).
            ASSIGN oe-ord.inv-date = fFixYear(oe-ord.inv-date).
            ASSIGN oe-ord.upd-date = fFixYear(oe-ord.upd-date).
            ASSIGN oe-ord.approved-date = fFixYear(oe-ord.approved-date).
            ASSIGN oe-ord.entered-date = fFixYear(oe-ord.entered-date).
            ASSIGN oe-ord.updated-date = fFixYear(oe-ord.updated-date).
            ASSIGN oe-ord.closedate = fFixYear(oe-ord.closedate).
            FOR EACH oe-ordl OF oe-ord:
                ASSIGN oe-ordl.req-date = fFixYear(oe-ordl.req-date).
                ASSIGN oe-ordl.prom-date = fFixYear(oe-ordl.prom-date).
                ASSIGN oe-ordl.upd-date = fFixYear(oe-ordl.upd-date).
                ASSIGN oe-ordl.job-start-date = fFixYear(oe-ordl.job-start-date).
            END.
            FOR EACH oe-rel OF oe-ord:
                ASSIGN oe-rel.rel-date = fFixYear(oe-rel.rel-date).
                ASSIGN oe-rel.ship-date = fFixYear(oe-rel.ship-date).
                ASSIGN oe-rel.upd-date = fFixYear(oe-rel.upd-date).
                FOR EACH oe-relh OF oe-rel:
                    ASSIGN oe-relh.rel-date = fFixYear(oe-relh.rel-date).
                    ASSIGN oe-relh.upd-date = fFixYear(oe-relh.upd-date).
                    ASSIGN oe-relh.prt-date = fFixYear(oe-relh.prt-date).
                END.
                FOR EACH oe-rell OF oe-rel:
                    ASSIGN oe-relh.upd-date = fFixYear(oe-rell.upd-date).
                END.
            END.
        
        FOR EACH bolh EXCLUSIVE where bolh.company EQ cCompany AND bolh.bol-date LT 12/31/0100:
            ASSIGN bolh.bol-date = fFixYear(bolh.bol-date).
            ASSIGN bolh.rel-date = fFixYear(bolh.rel-date).
            ASSIGN bolh.ship-date = fFixYear(bolh.ship-date).
            FOR EACH boll OF bolh EXCLUSIVE:
                ASSIGN boll.bol-date = fFixYear(boll.bol-date).
                ASSIGN boll.rel-date = fFixYear(boll.rel-date).
            END.
        END.
            
        END.
    END.
    IF tbInvoiceFiles:CHECKED THEN DO :
        FOR EACH inv-head EXCLUSIVE where inv-head.company EQ cCompany AND inv-head.inv-date LT 12/31/0100:
            ASSIGN inv-head.inv-date = fFixYear(inv-head.inv-date).
            ASSIGN inv-head.upd-date = fFixYear(inv-head.upd-date).
            FOR EACH inv-line OF inv-head EXCLUSIVE:
                ASSIGN inv-line.ord-date = fFixYear(inv-line.ord-date).
                ASSIGN inv-line.prom-date = fFixYear(inv-line.prom-date).
                ASSIGN inv-line.req-date = fFixYear(inv-line.req-date).
                ASSIGN inv-line.upd-date = fFixYear(inv-line.upd-date).
            END.
        END.
    END.
    IF tbFGFiles:CHECKED THEN DO :
        FOR EACH fg-hist EXCLUSIVE where fg-hist.company EQ cCompany AND fg-hist.trans-date LT 12/31/0100:
            ASSIGN fg-hist.posted-date = fFixYear(fg-hist.posted-date).
            ASSIGN fg-hist.trans-date = fFixYear(fg-hist.trans-date).
            ASSIGN fg-hist.upd-date = fFixYear(fg-hist.upd-date).
        END.
    
        FOR EACH fg-rcpth EXCLUSIVE where fg-rcpth.company EQ cCompany AND fg-rcpth.trans-date LT 12/31/0100:
            ASSIGN fg-rcpth.post-date = fFixYear(fg-rcpth.post-date).
            ASSIGN fg-rcpth.trans-date = fFixYear(fg-rcpth.trans-date).
            ASSIGN fg-rcpth.upd-date = fFixYear(fg-rcpth.upd-date).
        END.
    
        FOR EACH fg-rcpts EXCLUSIVE where fg-rcpts.company EQ cCompany AND fg-rcpts.trans-date LT 12/31/0100:
            ASSIGN fg-rcpts.trans-date = fFixYear(fg-rcpts.trans-date).
            ASSIGN fg-rcpts.upd-date = fFixYear(fg-rcpts.upd-date).
        END.
    
        FOR EACH fg-rctd EXCLUSIVE where fg-rctd.company EQ cCompany AND fg-rctd.rct-date LT 12/31/0100:
            ASSIGN fg-rctd.post-date = fFixYear(fg-rctd.post-date).
            ASSIGN fg-rctd.rct-date = fFixYear(fg-rctd.rct-date).
            ASSIGN fg-rctd.upd-date = fFixYear(fg-rctd.upd-date).
        END.
    
        FOR EACH fg-rdtl EXCLUSIVE where fg-rdtl.company EQ cCompany AND fg-rdtl.upd-date LT 12/31/0100:
            ASSIGN fg-rdtl.upd-date = fFixYear(fg-rdtl.upd-date).
        END.
    
        FOR EACH fg-rdtlh EXCLUSIVE where fg-rdtlh.company EQ cCompany AND fg-rdtlh.trans-date LT 12/31/0100:
            ASSIGN fg-rdtlh.trans-date = fFixYear(fg-rdtlh.trans-date).
            ASSIGN fg-rdtlh.upd-date = fFixYear(fg-rdtlh.upd-date).
        END.
    END.
    IF tbRMFiles:CHECKED THEN DO :
        FOR EACH rm-rcpt EXCLUSIVE where rm-rcpt.company EQ cCompany AND rm-rcpt.trans-date LT 12/31/0100:
            ASSIGN rm-rcpt.trans-date = fFixYear(rm-rcpt.trans-date).
            ASSIGN rm-rcpt.upd-date = fFixYear(rm-rcpt.upd-date).
        END.
    
        FOR EACH rm-rcpth EXCLUSIVE where rm-rcpth.company EQ cCompany AND rm-rcpth.post-date LT 12/31/0100:
            ASSIGN rm-rcpth.post-date = fFixYear(rm-rcpth.post-date).
            ASSIGN rm-rcpth.trans-date = fFixYear(rm-rcpth.trans-date).
            ASSIGN rm-rcpth.upd-date = fFixYear(rm-rcpth.upd-date).
        END.
    
        FOR EACH rm-rctd EXCLUSIVE where rm-rctd.company EQ cCompany AND rm-rctd.post-date LT 12/31/0100:
            ASSIGN rm-rctd.post-date = fFixYear(rm-rctd.post-date).
            ASSIGN rm-rctd.rct-date = fFixYear(rm-rctd.rct-date).
            ASSIGN rm-rctd.upd-date = fFixYear(rm-rctd.upd-date).
        END.
    
        FOR EACH rm-rcth EXCLUSIVE where rm-rcth.company EQ cCompany AND rm-rcth.trans-date LT 12/31/0100:
            ASSIGN rm-rcth.trans-date = fFixYear(rm-rcth.trans-date).
            ASSIGN rm-rcth.upd-date = fFixYear(rm-rcth.upd-date).
        END.
    
        FOR EACH rm-rdtl EXCLUSIVE where rm-rdtl.company EQ cCompany AND rm-rdtl.upd-date LT 12/31/0100:
            ASSIGN rm-rdtl.upd-date = fFixYear(rm-rdtl.upd-date).
        END.
    
        FOR EACH rm-rdtlh EXCLUSIVE where rm-rdtlh.company EQ cCompany AND rm-rdtlh.trans-date LT 12/31/0100:
            ASSIGN rm-rdtlh.trans-date = fFixYear(rm-rdtlh.trans-date).
            ASSIGN rm-rdtlh.upd-date = fFixYear(rm-rdtlh.upd-date).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFixYear gDialog 
FUNCTION fFixYear RETURNS DATE
  (INPUT daDate AS DATE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE outDate AS DATE NO-UNDO.
    
    IF daDate EQ ? THEN RETURN ?.
    ELSE DO:
        IF YEAR(daDate) LT 2000 
        AND YEAR(daDate) GT 50 THEN ASSIGN 
            outDate = DATE(MONTH(daDate), DAY(daDate), YEAR(daDate + 1900)).  
        ELSE IF YEAR(daDate) LT 2000 THEN ASSIGN 
            outDate = DATE(MONTH(daDate), DAY(daDate), YEAR(daDate + 2000)). 
        RETURN outDate.
    END. 
        
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghBrowse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE ghBrowse3 AS HANDLE NO-UNDO.
DEFINE VARIABLE ghQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE ghtteb AS HANDLE NO-UNDO.
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountBlank AS INTEGER NO-UNDO.
DEFINE VARIABLE ghColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE ghColumn2 AS HANDLE NO-UNDO.
DEFINE VARIABLE giFormCopyFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE giBlankCopyFrom AS INTEGER NO-UNDO.
DEFINE VARIABLE gcFormCopyTo AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBlankCopyTo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCopyFromSelected AS LOGICAL NO-UNDO INITIAL NO.

DEFINE VARIABLE cestyle-log LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE cestyle-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE cestyle-int LIKE sys-ctrl.int-fld NO-UNDO.
DEFINE VARIABLE cestyle-dec LIKE sys-ctrl.dec-fld NO-UNDO.

DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf2-eb FOR eb.
DEFINE BUFFER buf3-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf2-ef FOR ef.
DEFINE BUFFER buf3-ef FOR ef.
DEFINE BUFFER buf-est FOR est.

DEFINE TEMP-TABLE tteb NO-UNDO 
                  LIKE eb
                  FIELD lSelectable AS LOGICAL.

DEFINE NEW SHARED VARIABLE cocode  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode  AS CHARACTER NO-UNDO.                  

DEFINE TEMP-TABLE ttBoxDesignHdr NO-UNDO LIKE box-design-hdr.
DEFINE TEMP-TABLE ttBoxDesignLine NO-UNDO LIKE box-design-line.

{custom/globdefs.i}

cocode = g_company.
       
RUN pGetCeStyle.

{cec/descalc.i new}                  

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tteb

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tteb.lSelectable tteb.Form-No tteb.Blank-No tteb.Part-No tteb.Part-Dscr1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tteb.lSelectable   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tteb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tteb
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tteb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tteb


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tteb.lSelectable tteb.Form-No tteb.Blank-No tteb.Part-No tteb.Part-Dscr1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 tteb.lSelectable   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-3 tteb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-3 tteb
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tteb       WHERE tteb.est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tteb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tteb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BROWSE-3 TOGGLE-DIE TOGGLE-CAD ~
TOGGLE-Oth-Attributes TOGGLE-RecalcBoxDesign Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-DIE TOGGLE-CAD ~
TOGGLE-Oth-Attributes TOGGLE-RecalcBoxDesign 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Copy" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE TOGGLE-CAD AS LOGICAL INITIAL no 
     LABEL "Copy CAD#" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-DIE AS LOGICAL INITIAL no 
     LABEL "Copy Die#" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Oth-Attributes AS LOGICAL INITIAL no 
     LABEL "Copy Other Attributes" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-RecalcBoxDesign AS LOGICAL INITIAL no 
     LABEL "Recalc Box Design" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tteb SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      tteb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      tteb.lSelectable COLUMN-LABEL "Select" VIEW-AS TOGGLE-BOX 
      tteb.Form-No COLUMN-LABEL "Form" FORMAT ">9"
      tteb.Blank-No COLUMN-LABEL "Blank" FORMAT ">9"
      tteb.Part-No COLUMN-LABEL "Part" FORMAT "X(15)"
      tteb.Part-Dscr1 COLUMN-LABEL "Description" FORMAT "X(30)"
      ENABLE tteb.lSelectable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 6.91 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _FREEFORM
  QUERY BROWSE-3 NO-LOCK DISPLAY
      tteb.lSelectable COLUMN-LABEL "Select" VIEW-AS TOGGLE-BOX 
      tteb.Form-No COLUMN-LABEL "Form" FORMAT ">9"
      tteb.Blank-No COLUMN-LABEL "Blank" FORMAT ">9"
      tteb.Part-No COLUMN-LABEL "Part" FORMAT "X(15)"
      tteb.Part-Dscr1 COLUMN-LABEL "Description" FORMAT "X(30)"
      ENABLE tteb.lSelectable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 6.91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 2.91 COL 4 WIDGET-ID 200
     BROWSE-3 AT ROW 2.91 COL 84 WIDGET-ID 300
     TOGGLE-DIE AT ROW 10.29 COL 71 WIDGET-ID 6
     TOGGLE-CAD AT ROW 11.24 COL 71 WIDGET-ID 8
     TOGGLE-Oth-Attributes AT ROW 12.29 COL 71 WIDGET-ID 10
     TOGGLE-RecalcBoxDesign AT ROW 12.29 COL 99.4 WIDGET-ID 12
     Btn_OK AT ROW 13.38 COL 56
     Btn_Cancel AT ROW 13.38 COL 85
     "Copy From Item" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.95 COL 4.6 WIDGET-ID 2
     "Copy To Item(s)" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.95 COL 84 WIDGET-ID 4
     SPACE(51.79) SKIP(12.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Item Copy"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
/* BROWSE-TAB BROWSE-2 TEXT-2 Dialog-Frame */
/* BROWSE-TAB BROWSE-3 BROWSE-2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH eb
      WHERE est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "est-no EQ ipcEstimateNo"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH eb
      WHERE est-no EQ ipcEstimateNo NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "est-no EQ ipcEstimateNo"
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Item Copy */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 Dialog-Frame
ON VALUE-CHANGED OF lSelectable IN BROWSE BROWSE-2
DO:  
    DEFINE BUFFER buf-tteb FOR tteb.
    ASSIGN lCopyFromSelected = YES.
    DO iCounter = 1 TO ghBrowse2:NUM-COLUMNS:
        ghColumn = ghBrowse2:GET-BROWSE-COLUMN(iCounter).
        IF ghColumn:NAME = "lSelectable" AND ghColumn:SCREEN-VALUE = "YES"  THEN 
            ghColumn:READ-ONLY = TRUE.
        ELSE IF ghColumn:NAME = "Form-No" THEN 
            ASSIGN giFormCopyFrom = INTEGER (ghColumn:SCREEN-VALUE). 
        ELSE IF ghColumn:NAME = "Blank-No" THEN 
            ASSIGN giBlankCopyFrom = INTEGER (ghColumn:SCREEN-VALUE).   
        ELSE NEXT.
    END.
    
    FIND FIRST buf-tteb NO-LOCK
        WHERE buf-tteb.form-no EQ giFormCopyFrom
        AND buf-tteb.blank-no EQ giBlankCopyFrom NO-ERROR.
        
    IF AVAILABLE buf-tteb THEN 
        OPEN QUERY Browse-3 
        FOR EACH tteb WHERE ROWID(tteb) NE  ROWID (buf-tteb)
        NO-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 Dialog-Frame
ON VALUE-CHANGED OF lSelectable IN BROWSE BROWSE-3
DO:
    DO iCounter = 1 TO ghBrowse3:NUM-COLUMNS:
        ghColumn2 = ghBrowse3:GET-BROWSE-COLUMN(iCounter).
        IF ghColumn2:NAME = "Form-No" THEN 
            ASSIGN gcFormCopyTo = gcFormCopyTo + "," + ghColumn2:SCREEN-VALUE. 
        IF ghColumn2:NAME = "Blank-No" THEN 
            ASSIGN gcBlankCopyTo = gcBlankCopyTo + "," + ghColumn2:SCREEN-VALUE.   
    END.
    ASSIGN gcFormCopyTo = TRIM(gcFormCopyTo, ",")
           gcBlankCopyTo =  TRIM(gcBlankCopyTo, ",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Copy */
DO: 
    DEFINE VARIABLE iBlankWidLenIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE lStyleSame        AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cBoxDesign        AS CHARACTER NO-UNDO.
    IF lCopyFromSelected EQ NO THEN 
    DO:
        MESSAGE "Please select Form or Blank from CopyFromItem"
        VIEW-AS ALERT-BOX WARNING. 
        
        RETURN NO-APPLY.      
    END.
    
    IF NUM-ENTRIES(gcFormCopyTo) EQ 0 THEN 
    DO:
        MESSAGE "Please select Form or Blank from CopyToItem(s)"
        VIEW-AS ALERT-BOX WARNING. 
        
        RETURN NO-APPLY.      
    END. 
    
    IF  TOGGLE-DIE:SCREEN-VALUE EQ "NO" 
    AND TOGGLE-CAD:SCREEN-VALUE EQ "NO" THEN 
    DO: 
        IF TOGGLE-Oth-Attributes:SCREEN-VALUE EQ "YES" THEN 
        MESSAGE "Other Attributes alone should not be selected, please select CAD and DIE as well."
        VIEW-AS ALERT-BOX WARNING.
        ELSE 
        MESSAGE "Please select at least one DIE or CAD to copy"
        VIEW-AS ALERT-BOX WARNING.
        
        RETURN NO-APPLY.
    END.
    
    MESSAGE "Are you sure you want to Copy Blank Info? " VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE lCopyBlankInfo AS LOG.        
    IF lCopyBlankInfo THEN 
    DO: 
        FIND FIRST buf-eb NO-LOCK  
            WHERE  buf-eb.est-no EQ ipcEstimateNo
            AND  buf-eb.form-no EQ giFormCopyFrom
            AND  buf-eb.blank-no EQ giBlankCopyFrom NO-ERROR.
          
        FIND FIRST buf-ef NO-LOCK
            WHERE buf-ef.est-no EQ ipcEstimateNo
            AND buf-ef.form-no EQ giFormCopyFrom NO-ERROR. 
                 
        IF AVAILABLE buf-ef AND AVAILABLE buf-eb THEN 
        DO:       
            REPEAT iCountBlank = 1 TO NUM-ENTRIES(gcFormCopyTo):
                FIND FIRST buf2-eb EXCLUSIVE-LOCK 
                    WHERE  buf2-eb.est-no EQ ipcEstimateNo
                    AND  buf2-eb.form-no EQ INTEGER (ENTRY (iCountBlank, gcFormCopyTo))
                    AND  buf2-eb.blank-no EQ INTEGER (ENTRY (iCountBlank, gcBlankCopyTo)) NO-ERROR.
                
                FIND FIRST buf2-ef EXCLUSIVE-LOCK 
                    WHERE  buf2-ef.est-no EQ ipcEstimateNo
                    AND  buf2-ef.form-no EQ INTEGER (ENTRY (iCountBlank, gcFormCopyTo)) NO-ERROR.
                    
                IF AVAILABLE buf2-ef AND AVAILABLE buf2-eb  THEN 
                DO:
                    IF TOGGLE-DIE:SCREEN-VALUE EQ "YES" THEN
                        ASSIGN buf2-eb.die-no = buf-eb.die-no.
                
                    IF TOGGLE-CAD:SCREEN-VALUE EQ "YES" THEN
                        ASSIGN buf2-eb.cad-no = buf-eb.cad-no.
                
                    IF TOGGLE-Oth-Attributes:SCREEN-VALUE EQ "YES" THEN
                    DO:
                        ASSIGN cBoxDesign        = "S"
                               lStyleSame        = buf2-eb.style EQ buf-eb.style
                               buf2-eb.spc-no    = buf-eb.spc-no
                               buf2-eb.upc-no    = buf-eb.upc-no
                               buf2-eb.style     = buf-eb.style
                               buf2-eb.len       = buf-eb.len
                               buf2-eb.wid       = buf-eb.wid
                               buf2-eb.dep       = buf-eb.dep
                               buf2-eb.adhesive  = buf-eb.adhesive
                               buf2-eb.dust      = buf-eb.dust
                               buf2-eb.fpanel    = buf-eb.fpanel
                               buf2-eb.lock      = buf-eb.lock
                               buf2-eb.gluelap   = buf-eb.gluelap
                               buf2-eb.k-len     = buf-eb.k-len
                               buf2-eb.k-wid     = buf-eb.k-wid
                               buf2-eb.tuck      = buf-eb.tuck
                               buf2-eb.lin-in    = buf-eb.lin-in
                               buf2-eb.t-wid     = buf-eb.t-wid
                               buf2-eb.t-len     = buf-eb.t-len
                               buf2-eb.t-sqin    = buf-eb.t-sqin
                               buf2-ef.cad-image = buf-ef.cad-image.  
                            
                        DO iBlankWidLenIndex = 1 TO 20:
                            ASSIGN buf2-eb.k-wid-array[iBlankWidLenIndex] = buf-eb.k-wid-array[iBlankWidLenIndex]
                                   buf2-eb.k-wid-scr-type[iBlankWidLenIndex] = buf-eb.k-wid-scr-type[iBlankWidLenIndex]
                                   buf2-eb.k-len-array[iBlankWidLenIndex] = buf-eb.k-len-array[iBlankWidLenIndex]
                                   buf2-eb.k-len-scr-type[iBlankWidLenIndex] = buf-eb.k-len-scr-type[iBlankWidLenIndex].  
                        END.  
                        RUN est/u2kinc1.p (RECID(buf2-eb)).
                        RUN est/u2kinc2.p (RECID(buf2-eb)). 
                                                                   
                        IF  NOT lStyleSame AND cestyle-log THEN  
                        DO:
                            IF TOGGLE-RecalcBoxDesign:SCREEN-VALUE EQ "YES" THEN 
                                ASSIGN cBoxDesign = "B".
                            ELSE 
                                ASSIGN cBoxDesign = "N".
                        END. 
                        RUN pBuildBox (cBoxDesign).            
                    END.                
                END. 
            END. 
        END.
    MESSAGE "Blank Information Copied."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.          
    RELEASE buf-eb.
    RELEASE buf2-eb.
    RELEASE buf-ef.
    RELEASE buf2-ef.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Oth-Attributes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Oth-Attributes Dialog-Frame
ON VALUE-CHANGED OF TOGGLE-Oth-Attributes IN FRAME Dialog-Frame /* Copy Other Attributes */
DO:
    IF TOGGLE-Oth-Attributes:SCREEN-VALUE EQ "YES" THEN 
       ASSIGN TOGGLE-RecalcBoxDesign:SENSITIVE = TRUE. 
    ELSE 
       ASSIGN TOGGLE-RecalcBoxDesign:SENSITIVE = FALSE
              TOGGLE-RecalcBoxDesign:SCREEN-VALUE = "NO".     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   ghBrowse2 = BROWSE Browse-2:HANDLE. 
   ghBrowse3 = BROWSE Browse-3:HANDLE. 
   FOR EACH eb NO-LOCK 
      WHERE eb.est-no = ipcEstimateNo:
          CREATE tteb.
          BUFFER-COPY eb TO tteb. 
   END.
   RUN enable_UI.
   TOGGLE-RecalcBoxDesign:SENSITIVE = FALSE.
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
  DISPLAY TOGGLE-DIE TOGGLE-CAD TOGGLE-Oth-Attributes TOGGLE-RecalcBoxDesign 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-2 BROWSE-3 TOGGLE-DIE TOGGLE-CAD TOGGLE-Oth-Attributes 
         TOGGLE-RecalcBoxDesign Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildBox Dialog-Frame
PROCEDURE pBuildBox:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcBoxDesign AS CHARACTER NO-UNDO.

DEFINE  BUFFER buf-box-design-hdr  FOR box-design-hdr.
DEFINE  BUFFER buf-box-design-line FOR box-design-line. 
  
FIND FIRST buf3-eb NO-LOCK
    WHERE RECID(buf3-eb) EQ RECID(buf2-eb) NO-ERROR.
   
IF AVAILABLE buf3-eb THEN 
DO:
    FIND FIRST buf-est NO-LOCK 
        WHERE buf-est.company EQ buf3-eb.company 
        AND buf-est.est-no EQ buf3-eb.est-no NO-ERROR.

    FIND FIRST buf3-ef NO-LOCK 
        WHERE buf3-ef.company EQ buf3-eb.company 
        AND buf3-ef.est-no  EQ buf3-eb.est-no
        AND buf3-ef.form-no EQ buf3-eb.form-no NO-ERROR. 
END.

EMPTY TEMP-TABLE ttBoxDesignHdr.
EMPTY TEMP-TABLE ttBoxDesignLine.

FOR EACH box-design-hdr NO-LOCK 
    WHERE box-design-hdr.design-no EQ 0 
      AND box-design-hdr.company   EQ buf3-eb.company
      AND box-design-hdr.est-no    EQ buf3-eb.est-no 
      AND box-design-hdr.form-no   EQ buf3-eb.form-no
      AND box-design-hdr.blank-no  EQ buf3-eb.blank-no :

    CREATE ttBoxDesignHdr.
    BUFFER-COPY box-design-hdr TO ttBoxDesignHdr.

    FOR EACH box-design-line OF box-design-hdr NO-LOCK:
        CREATE ttBoxDesignLine.
        BUFFER-COPY box-design-line TO ttBoxDesignLine.
    END.
END. 

IF ipcBoxDesign NE "N" THEN
DO:
    FOR EACH box-design-hdr EXCLUSIVE-LOCK 
        WHERE box-design-hdr.design-no EQ 0 
          AND box-design-hdr.company   EQ buf3-eb.company 
          AND box-design-hdr.est-no    EQ buf3-eb.est-no
          AND box-design-hdr.form-no   EQ buf3-eb.form-no
          AND box-design-hdr.blank-no  EQ buf3-eb.blank-no :
              
        FOR EACH box-design-line OF box-design-hdr EXCLUSIVE-LOCK :
            DELETE box-design-line.
        END.
        DELETE box-design-hdr.
    END.
END.

FIND FIRST style NO-LOCK 
    WHERE style.company EQ buf3-eb.company
      AND style.style   EQ buf3-eb.style  NO-ERROR. 

IF AVAILABLE  style THEN 
    FIND FIRST buf-box-design-hdr NO-LOCK  
        WHERE buf-box-design-hdr.design-no EQ  style.design-no 
          AND buf-box-design-hdr.company   EQ  style.company  
          AND buf-box-design-hdr.est-no    EQ  "" NO-ERROR.

    IF AVAILABLE buf-box-design-hdr THEN 
    DO:
        IF ipcBoxDesign NE "N" THEN
        DO:
            RUN cec/descalc.p (RECID(buf-est), RECID(buf3-eb)).
            CREATE  box-design-hdr.
            ASSIGN   
                box-design-hdr.design-no    = 0
                box-design-hdr.company      = buf3-eb.company
                box-design-hdr.est-no       = buf3-eb.est-no
                box-design-hdr.form-no      = buf3-eb.form-no
                box-design-hdr.blank-no     = buf3-eb.blank-no
                box-design-hdr.description  = IF AVAILABLE  buf-box-design-hdr 
                                              THEN buf-box-design-hdr.description ELSE  ""
                box-design-hdr.lscore       = v-lscore-c
                box-design-hdr.lcum-score   = v-lcum-score-c
                box-design-hdr.wscore       = buf-box-design-hdr.wscore
                box-design-hdr.wcum-score   = buf-box-design-hdr.wcum-score
                box-design-hdr.box-text     = buf-box-design-hdr.box-text
                box-design-hdr.box-image    = buf-box-design-hdr.box-image
                box-design-hdr.box-3d-image = buf-box-design-hdr.box-3d-image.
          
            FOR EACH buf-box-design-line of buf-box-design-hdr NO-LOCK:
                CREATE box-design-line.
                ASSIGN  
                    box-design-line.design-no = box-design-hdr.design-no
                    box-design-line.company   = box-design-hdr.company
                    box-design-line.est-no    = box-design-hdr.est-no
                    box-design-line.form-no   = box-design-hdr.form-no
                    box-design-line.blank-no  = box-design-hdr.blank-no
                    box-design-line.line-no   = buf-box-design-line.line-no
                    box-design-line.line-text = buf-box-design-line.line-text.
    
                FIND FIRST w-box-design-line
                    WHERE w-box-design-line.line-no EQ box-design-line.line-no NO-ERROR.
    
                IF AVAILABLE w-box-design-line THEN
                    ASSIGN  box-design-line.wscore     = w-box-design-line.wscore-c
                            box-design-line.wcum-score = w-box-design-line.wcum-score-c.
            END.
        END. /*if ipcBoxDesign ne "N"*/
 /*
        IF ipcBoxDesign NE "B" AND ipcBoxDesign NE "N" THEN 
        DO: 
            FIND FIRST ttBoxDesignHdr NO-ERROR.
 
            IF AVAIL ttBoxDesignHdr THEN
            DO:
                IF ipcBoxDesign EQ "S" THEN
                    ASSIGN box-design-hdr.description  = ttBoxDesignHdr.description
                           box-design-hdr.box-image    = ttBoxDesignHdr.box-image
                           box-design-hdr.box-3d-image = ttBoxDesignHdr.box-3d-image.
                ELSE
                    ASSIGN box-design-hdr.lscore     = ttBoxDesignHdr.lscore
                           box-design-hdr.lcum-score = ttBoxDesignHdr.lcum-score
                           box-design-hdr.wscore     = ttBoxDesignHdr.wscore
                           box-design-hdr.wcum-score = ttBoxDesignHdr.wcum-score.
       
                FOR EACH ttBoxDesignLine OF box-design-hdr,
                    FIRST box-design-line OF ttBoxDesignHdr:
        
                    IF ipcBoxDesign EQ "S" THEN
                        ASSIGN box-design-line.line-no   = ttBoxDesignLine.line-no
                               box-design-line.line-text = ttBoxDesignLine.line-text.
                    ELSE
                        ASSIGN box-design-line.wscore     = ttBoxDesignLine.wscore
                               box-design-line.wcum-score = ttBoxDesignLine.wcum-score.
                END.
            END. 
        END. */
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCeStyle Dialog-Frame
PROCEDURE pGetCeStyle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO TRANSACTION:
    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CESTYLEF" NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN 
    DO:    
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "CESTYLEF" 
            sys-ctrl.log-fld = YES
            sys-ctrl.descrip = "When changing Style, Prompt to Update Box Design for " +
                      "Folding" + " Estimates".
    END.

    ASSIGN
        cestyle-log = sys-ctrl.log-fld
        cestyle-chr = sys-ctrl.char-fld
        cestyle-int = sys-ctrl.int-fld
        cestyle-dec = sys-ctrl.dec-fld.
END. 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

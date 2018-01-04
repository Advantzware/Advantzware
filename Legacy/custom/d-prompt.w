&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
/***              Example of use                    ******/
/*
DEF VAR ip-view-type AS CHAR NO-UNDO.
DEF VAR ip-parms     AS CHAR NO-UNDO.
DEF VAR op-values    AS CHAR NO-UNDO.

ip-parms = 
    "type=literal,name=fi1,row=3,col=15,enable=false,width=60,FORMAT=X(60),scrval=This will create an Actual release from this Planned release. " 
    + "|type=literal,name=fi4,row=4,col=15,enable=false,width=30,scrval=Would you like to continue?,FORMAT=X(30)"
    + "|type=toggle,name=tg1,row=6,col=16,enable=true,scrval=no,label=test this label,width=3"
    + "|type=literal,name=fi5,row=6,col=20,enable=false,scrval=Prompt to merge each item?,width=28,format=x(28)" 
    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=2,enable=true " 
    + "|type=win,name=fi3,enable=true,label=Question,scrollbars=no ".
*/    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-view-type AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-parms     AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-pproc     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-values    AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEF VAR ll-secure AS LOGICAL NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.
DEF VAR hImage AS HANDLE NO-UNDO.
DEF VAR cImageFile AS CHAR NO-UNDO.
DEF VAR cFieldTypes AS CHAR NO-UNDO INIT "fill-in,toggle".
DEF VAR lhFirstWid AS HANDLE NO-UNDO.
DEF VAR gvcLookupList AS CHAR NO-UNDO INIT "custShipTo,locField,fi_fromLoc".
DEFINE VARIABLE gvcLastEntryField AS CHARACTER   NO-UNDO.
DEF VAR h_super AS HANDLE.
IF ip-pproc GT "" THEN
  RUN VALUE(ip-pproc) PERSISTENT SET h_super.

IF ip-view-type = "" THEN
    ip-view-type = "ok-cancel".


DEF VAR hi AS INT NO-UNDO.
DEF VAR param-entry AS CHAR EXTENT 20 NO-UNDO .

DEF TEMP-TABLE tt-wid
    FIELD wid-hand AS HANDLE
    FIELD wid-type AS CHAR 
    FIELD wid-name AS CHAR
    FIELD wid-width AS INT
    FIELD wid-height AS INT
    FIELD wid-row AS DEC
    FIELD wid-col AS DEC
    FIELD is-enable AS LOG
    FIELD wid-label AS CHAR
    FIELD wid-format AS CHAR
    FIELD wid-scr-value AS CHAR
    FIELD wid-inp-value AS CHAR
    FIELD wid-chand AS CHAR
    FIELD wid-sl-hand AS CHAR
    FIELD wid-data-type AS CHAR
    FIELD wid-fgcol AS INT
    FIELD wid-bgcol AS INT
    FIELD wid-font AS INT   
    FIELD wid-init AS CHAR
    FIELD wid-image AS CHAR
    FIELD wid-list AS CHAR
    FIELD wid-default-from AS CHAR
    FIELD wid-depends-on AS CHAR
    FIELD wid-scrollbars AS LOG .

DEF VAR v-param AS CHAR.
DEF VAR v-attrib AS CHAR.
DEF VAR v-value AS CHAR.
DEF VAR h_wid1 AS WIDGET-HANDLE.
DEF VAR h_wid1-label AS HANDLE.

DEF VAR w-win AS HANDLE.

CREATE WINDOW w-win.
CURRENT-WINDOW = w-win.
w-win:RESIZE = TRUE.
w-win:X = 50.
w-win:Y = 40.
w-win:ALWAYS-ON-TOP = FALSE.
w-win:VISIBLE = FALSE.
w-win:HIDDEN  = TRUE.
w-win:NAME = "W-win".
w-win:SCROLL-BARS = NO.


DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-import 
     LABEL "&OK" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-no 
     LABEL "&NO" 
     SIZE 18 BY 1.14.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 8.1.

DEFINE FRAME xyz.
FRAME xyz:PARENT = w-win.
FRAME xyz:SCROLLABLE = NO.


FORM btn-import AT ROW 10 COL 31
     btn-no     AT ROW 10 COL 49.5
     btn-cancel AT ROW 10 COL 68
     SPACE(2.39) SKIP(0.46)
    WITH FRAME xyz SIZE-CHARS 90 BY 12
    VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE.
IF NOT ip-view-type = "yes-no-cancel" THEN
    ASSIGN btn-import:COL = 35 btn-cancel:COL = 55 btn-no:VISIBLE = NO.

ON CHOOSE OF btn-cancel IN FRAME xyz /* Cancel */
DO:
    RUN assignButtonDefaults.
    FOR EACH tt-wid WHERE tt-wid.is-enable = TRUE AND tt-wid.wid-type NE "WiN".
        tt-wid.wid-scr-value = tt-wid.wid-hand:SCREEN-VALUE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Error - widget screen value " tt-wid.wid-name
            VIEW-AS ALERT-BOX.
        IF tt-wid.wid-scr-value NE ? THEN
        op-values = op-values + "," + tt-wid.wid-name + "," + REPLACE(tt-wid.wid-scr-value, ",", "").
    END.
      IF ip-view-type EQ "yes-no" THEN
        op-values = op-values + "," + "DEFAULT" + "," + "NO".
      ELSE
        op-values = op-values + "," + "DEFAULT" + "," + "Cancel".
    op-values = TRIM(op-values, ",").

    IF VALID-HANDLE(h_super) THEN
      DELETE OBJECT h_super.
    RUN delete-objects.
    apply "close" TO CURRENT-WINDOW.
END.

ON CHOOSE OF btn-import OR CHOOSE OF btn-no IN FRAME xyz /* OK */
DO:
   RUN assignButtonDefaults.
   DEF VAR ta AS HANDLE.
   FOR EACH tt-wid WHERE tt-wid.is-enable = TRUE AND tt-wid.wid-type NE "WiN".
       tt-wid.wid-scr-value = tt-wid.wid-hand:SCREEN-VALUE NO-ERROR.
      
       ta = tt-wid.wid-hand.
       IF ERROR-STATUS:ERROR THEN
           MESSAGE "Error - widget screen value " tt-wid.wid-name
           VIEW-AS ALERT-BOX.
       IF tt-wid.wid-scr-value NE ? THEN DO:                      
           op-values = op-values + "," + tt-wid.wid-name + "," + REPLACE(tt-wid.wid-scr-value, ",", "").
       END.
       
   END.
  
   IF ip-view-type EQ "yes-no" THEN
     op-values = op-values + "," + "DEFAULT" + "," + "YES".
   ELSE
       IF ip-view-type EQ "yes-no-cancel" THEN
           op-values = op-values + "," + "DEFAULT" + "," + (IF INDEX(SELF:LABEL,"NO") GT 0 THEN "NO" ELSE "YES") .
   ELSE
     op-values = op-values + "," + "DEFAULT" + "," + "OK".
  
   op-values = TRIM(op-values, ",").
  
   IF VALID-HANDLE(h_super) THEN
      DELETE OBJECT h_super.
  
   APPLY 'GO' TO  FRAME xyz.
END.



ON 'alt-y':U OF FRAME xyz OR 'alt-n' OF FRAME xyz OR 'alt-o' OF FRAME xyz 
   OR 'alt-c' OF FRAME xyz                                                 
DO:
    IF KEYLABEL(LASTKEY) EQ "alt-y" OR KEYLABEL(LASTKEY) EQ "alt-o" THEN
        APPLY 'choose' TO btn-import IN FRAME xyz.
    IF KEYLABEL(LASTKEY) EQ "alt-n" OR KEYLABEL(LASTKEY) EQ "alt-c" THEN
        APPLY 'choose' TO btn-cancel IN FRAME xyz.

END.

ON 'leave':U ANYWHERE
DO:
    DEF VAR v-name AS CHAR.
    v-name = SELF:NAME.
    IF VALID-HANDLE(h_super) THEN
      RUN leave-trig IN h_super (INPUT v-name).
    
    DEF VAR v-scrval AS CHAR NO-UNDO.
    DEF BUFFER bf-tt-wid FOR tt-wid.
    v-name = SELF:NAME.
    v-scrval = SELF:SCREEN-VALUE NO-ERROR.
    FOR EACH tt-wid WHERE tt-wid.wid-name = v-name,

        EACH bf-tt-wid WHERE bf-tt-wid.wid-default-from EQ tt-wid.wid-name
                         AND bf-tt-wid.wid-default-from GT "":
        IF VALID-HANDLE(tt-wid.wid-hand) AND VALID-HANDLE(bf-tt-wid.wid-hand)
            AND tt-wid.wid-hand:SCREEN-VALUE NE "" AND tt-wid.wid-hand:SCREEN-VALUE NE ?  THEN
            bf-tt-wid.wid-hand:SCREEN-VALUE = tt-wid.wid-hand:SCREEN-VALUE.
    END.    
    RETURN.
END.

ON 'escape':U OF FRAME xyz
DO:    
    APPLY 'choose' TO btn-cancel IN FRAME xyz.
    RUN delete-objects.
    RETURN.
END.

CASE ip-view-type:
    WHEN "OK-Cancel" THEN
        ASSIGN btn-import:LABEL = "Ok" btn-cancel:LABEL = "Cancel".
    WHEN "YES-NO" THEN
        ASSIGN btn-import:LABEL = "&Yes" btn-cancel:LABEL = "&No".
    WHEN "YES-NO-CANCEL" THEN
        ASSIGN btn-import:LABEL = "&Yes" btn-cancel:LABEL = "&Cancel".
END CASE.

ON WINDOW-CLOSE OF FRAME xyz /* Select RunShip or Managed Inventory */
DO:
  IF VALID-HANDLE(h_super) THEN
    DELETE OBJECT h_super.
  APPLY 'choose' TO btn-cancel.

  /* APPLY "END-ERROR":U TO SELF. */
END.

APPLY 'entry' TO w-win.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-get-attrib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-attrib Procedure 
FUNCTION get-attrib RETURNS CHARACTER
  ( ipcWidType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lookup-exists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD lookup-exists Procedure 
FUNCTION lookup-exists RETURNS LOGICAL
  ( ipcField AS CHAR )  FORWARD.

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


/* ************************  Frame Definitions  *********************** */

DEF VAR vi AS INT NO-UNDO.
 DEF BUFFER bf-tt-wid FOR tt-wid.

ON 'help':U ANYWHERE
DO:
    DEF VAR v-name AS CHAR NO-UNDO.
    DEF VAR char-val AS CHAR NO-UNDO.
    v-name = SELF:NAME.

    IF lookup-exists(v-name) THEN DO:
        RUN field-lookup (INPUT v-name, INPUT self:screen-value, OUTPUT char-val).
    END.
    IF char-val NE "" THEN
      SELF:SCREEN-VALUE = char-val NO-ERROR.
        
END.

ON 'entry':U ANYWHERE
DO:


/*    tt-wid.wid-default-from = v-value.
             tt-wid.wid-depends-on = v-value. */
    RETURN.
END.

ON 'value-changed':U ANYWHERE
DO:
    DEF VAR v-name AS CHAR NO-UNDO.
    DEF VAR v-scrval AS CHAR NO-UNDO.
    DEF VAR v-datatype AS CHAR NO-UNDO.
    DEF VAR v-selected AS LOG NO-UNDO.

    ASSIGN
    v-name     = SELF:NAME
    v-scrval   = SELF:SCREEN-VALUE 
    v-datatype = SELF:DATA-TYPE NO-ERROR.

    IF v-datatype = "logical" AND v-scrval = "YES" THEN
        v-selected = YES.
    ELSE
        v-selected = NO.
   
    /* If dependent on a checkbox, check for that and set sensitive */
    IF v-datatype = "logical" THEN DO:
      FOR EACH tt-wid WHERE tt-wid.wid-name = v-name,
            EACH bf-tt-wid WHERE bf-tt-wid.wid-depends-on = tt-wid.wid-name .

        IF AVAIL bf-tt-wid AND VALID-HANDLE(bf-tt-wid.wid-hand) THEN
          bf-tt-wid.wid-hand:SENSITIVE = v-selected.
      END.
    END.

    RETURN.
END.

DO hi = 1 TO NUM-ENTRIES(ip-parms, "|"):
   param-entry[hi] = ENTRY(hi, ip-parms, "|").
   RUN parse-params (INPUT param-entry[hi]).
END.

CURRENT-WINDOW:WIDTH-CHARS = 10.
CURRENT-WINDOW:HEIGHT-CHARS = 11.
CURRENT-WINDOW:TITLE = "Code Entry".
CURRENT-WINDOW:ROW  = 10.
CURRENT-WINDOW:COLUMN = 100.
RUN set-window-attri.


APPLY 'entry' TO w-win.

DEF VAR az AS HANDLE.
VIEW FRAME xyz.
az = FRAME xyz:PARENT.
RUN assignButtonDefaults.
RUN create-field-widgets.


/* CASE ip-view-type:                                                     */
/*     WHEN "OK-Cancel" THEN                                              */
/*         ASSIGN btn-import:LABEL = "Ok" btn-cancel:LABEL = "Cancel".    */
/*     WHEN "YES-NO" THEN                                                 */
/*         ASSIGN btn-import:LABEL = "&Yes" btn-cancel:LABEL = "&No".     */
/*     WHEN "YES-NO-CANCEL" THEN                                          */
/*         ASSIGN btn-import:LABEL = "&Yes" btn-cancel:LABEL = "&Cancel". */
/*                                                                        */
/* END CASE.                                                              */

APPLY 'entry' TO w-win.
ASSIGN FRAME xyz:SCROLLABLE = FALSE.
VIEW FRAME xyz.

IF VALID-HANDLE(hImage) THEN
  hImage:LOAD-IMAGE(SEARCH(cImageFile)).

IF ip-view-type EQ "YES-NO-CANCEL" THEN
ENABLE btn-import btn-no btn-cancel WITH FRAME xyz.
ELSE
ENABLE btn-import btn-cancel WITH FRAME xyz.
IF lhFirstWid NE ? AND VALID-HANDLE(lhFirstWid) THEN
    APPLY 'entry' TO lhFirstWid.
FOR EACH bf-tt-wid WHERE bf-tt-wid.wid-depends-on GT "" 
            AND bf-tt-wid.wid-depends-on NE  "tb_addinv":
    IF VALID-HANDLE(bf-tt-wid.wid-hand) THEN
        bf-tt-wid.wid-hand:SENSITIVE = FALSE.
END.
WAIT-FOR GO OF FRAME xyz.

RUN delete-objects.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-assignButtonDefaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignButtonDefaults Procedure 
PROCEDURE assignButtonDefaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME xyz:
  CASE ip-view-type:
      WHEN "OK-Cancel" THEN
          ASSIGN btn-import:LABEL = "Ok" btn-cancel:LABEL = "Cancel".
      WHEN "YES-NO" THEN
          ASSIGN btn-import:LABEL = "&Yes" btn-cancel:LABEL = "&No".
      WHEN "YES-NO-CANCEL" THEN
          ASSIGN btn-import:LABEL = "&Yes" btn-no:LABEL = "&No"
                 btn-cancel:LABEL = "&Cancel".
  
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-field-widgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-field-widgets Procedure 
PROCEDURE create-field-widgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR vj AS INT NO-UNDO.
FOR EACH tt-wid vj = 1 TO 50:
    IF tt-wid.wid-type EQ "WIN" THEN NEXT.    
    RUN set-wid (INPUT-OUTPUT h_wid1).   
    tt-wid.wid-hand = h_wid1.    
    IF LOOKUP(tt-wid.wid-type, cFieldTypes) GT 0 AND lhFirstWid EQ ? THEN
      lhFirstWid = h_wid1.
  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-objects) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-objects Procedure 
PROCEDURE delete-objects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(w-win) THEN
        DELETE WIDGET w-win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-field-lookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE field-lookup Procedure 
PROCEDURE field-lookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipcFieldName AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipcScrnVal AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opcCharVal AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.
DEF VAR rec-val AS RECID NO-UNDO.

/* Define an entry for each gvcLookupList here */
char-val = ipcScrnVal.
CASE ipcFieldName:
    
    WHEN "custShipTo" THEN DO:

        IF GET-attrib("company") GT ""             
            AND get-attrib("cust-no") GT "" THEN
        RUN windows/l-shipt2.w (get-attrib("company"), 
                                "" /* loc */, 
                                get-attrib("cust-no"), 
                                ipcScrnVal,
                                OUTPUT char-val, 
                                OUTPUT rec-val).
        char-val = ENTRY(1, char-val).

        opcCharVal = char-val.

        
    END.
    WHEN "locField" OR WHEN "fi_fromLoc" THEN DO:

       IF GET-attrib("company") GT "" THEN
         RUN windows/l-loc.w (get-attrib("company"), ipcScrnVal, OUTPUT char-val).
       IF char-val NE "" THEN DO:
         char-val = ENTRY(1, char-val).
         opcCharVal = char-val.
       END.

    END.
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parse-params) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parse-params Procedure 
PROCEDURE parse-params :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER var1 AS CHAR NO-UNDO.
    CREATE tt-wid.
    DO vi = 1 TO NUM-ENTRIES(var1):
        v-param = ENTRY(vi, var1).
        v-attrib = ENTRY(1, v-param, "=").
        v-value  = ENTRY(2, v-param, "=").
        CASE v-attrib:
            WHEN "type" THEN tt-wid.wid-type = v-value.
            WHEN "name" THEN tt-wid.wid-name = v-value.
            WHEN "row" THEN tt-wid.wid-row = DECIMAL(v-value).
            WHEN "col" THEN tt-wid.wid-col = DECIMAL(v-value).
            WHEN "enable" THEN tt-wid.is-enable = logical(v-value).
            WHEN "label" THEN tt-wid.wid-label = v-value.
            WHEN "format" THEN tt-wid.wid-format = v-value.
            WHEN "width" THEN tt-wid.wid-width   = integer(v-value).
            WHEN "height" THEN tt-wid.wid-height   = integer(v-value).
            WHEN "scrval" THEN tt-wid.wid-scr-value = v-value.
            WHEN "image" THEN tt-wid.wid-image = v-value.
            WHEN "scrollbars" THEN tt-wid.wid-scrollbars = logical(v-value).
            WHEN "data-type" THEN tt-wid.wid-data-type = v-value.
            WHEN "side-label-handle" THEN tt-wid.wid-sl-hand = v-value.
            WHEN "handle" THEN tt-wid.wid-chand = v-value.
            WHEN "inpval" THEN tt-wid.wid-inp-value = v-value.
            WHEN "fgcol" THEN tt-wid.wid-fgcol = INT(v-value).
            WHEN "bgcol" THEN tt-wid.wid-bgcol = INT(v-value).
            WHEN "font" THEN tt-wid.wid-font = INT(v-value).
            WHEN "initial" THEN tt-wid.wid-init = v-value.
            WHEN "deffield" THEN tt-wid.wid-default-from = v-value.
            WHEN "depfield" THEN tt-wid.wid-depends-on = v-value.
            WHEN "list" THEN tt-wid.wid-list = REPLACE(v-value, "#", ",").            
        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-position) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-position Procedure 
PROCEDURE set-position :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipRow AS INT NO-UNDO.
    DEF INPUT PARAMETER ipCol AS INT NO-UNDO.
    IF VALID-HANDLE(w-win) THEN DO:
  
        ASSIGN FRAME xyz:ROW = ipRow
               FRAME xyz:COL = ipCol.
  
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-wid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-wid Procedure 
PROCEDURE set-wid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER h_wid1 AS HANDLE NO-UNDO.

    CASE tt-wid.wid-type:
        WHEN "fill-in" THEN do: 
            CREATE FILL-IN h_wid1 ASSIGN FORMAT = "x(256)"
                WIDTH-CHARS = 20.

            CREATE TEXT h_wid1-label.
            h_wid1-label:FRAME = FRAME xyz:HANDLE.
        END.
        WHEN "literal" THEN do: 
            CREATE TEXT h_wid1 .     
            h_wid1:FRAME = FRAME xyz:HANDLE.
        END.
        WHEN "rectangle" THEN do: 
            CREATE RECTANGLE h_wid1 .
            ASSIGN tt-wid.wid-row = tt-wid.wid-row - .2
                tt-wid.wid-col = tt-wid.wid-col +  .1.
        END.
        WHEN "toggle" THEN do: 
           CREATE TOGGLE-BOX h_wid1 .
           
        END.
        WHEN "combo-box" THEN do: 
           CREATE COMBO-BOX h_wid1 .           
        END.
        WHEN "selection-list" THEN do: 
           CREATE SELECTION-LIST h_wid1 .  
           h_wid1:FONT = IF tt-wid.wid-font GT 0 THEN tt-wid.wid-font ELSE 8.
           h_wid1:HEIGHT-PIXELS = IF tt-wid.wid-height GT 0 THEN tt-wid.wid-height ELSE 40.
        END.
        WHEN "Image" THEN DO:
            CREATE IMAGE h_wid1 .
            h_wid1:STRETCH-TO-FIT = TRUE.
            h_wid1:WIDTH-PIXELS = IF tt-wid.wid-width GT 0 THEN tt-wid.wid-width ELSE 40.
            h_wid1:HEIGHT-PIXELS = IF tt-wid.wid-height GT 0 THEN tt-wid.wid-height ELSE 40.
            hImage = h_wid1.
            cImageFile = tt-wid.wid-image.
            /*
            IF tt-wid.wid-width GT 0 THEN
                h_wid1:WIDTH-PIXELS = tt-wid.wid-width.
            IF tt-wid.wid-height GT 0 THEN
                 h_wid1:HEIGHT-PIXELS = tt-wid.wid-height. */
        END.
        WHEN "ButtonLabel" THEN DO:
          CASE ip-view-type:
              WHEN "OK-Cancel" THEN DO:
                IF tt-wid.wid-name = "OK" THEN
                   ASSIGN btn-import:LABEL = tt-wid.wid-label /* "Ok" */.
                IF tt-wid.wid-name = "Cancel" THEN
                   ASSIGN btn-cancel:LABEL = tt-wid.wid-label /* "Cancel" */.
              END.                  
              WHEN "YES-NO" THEN DO:
                IF tt-wid.wid-name = "YES" THEN
                  ASSIGN btn-import:LABEL = tt-wid.wid-label /* "&Yes"  */.
                IF tt-wid.wid-name = "NO" THEN
                  ASSIGN btn-cancel:LABEL = tt-wid.wid-label /* "&No" */.
              END.                 
              WHEN "YES-NO-CANCEL" THEN DO:
                IF tt-wid.wid-name = "OK" THEN
                  ASSIGN btn-no:LABEL = tt-wid.wid-label /* "OK" */.
                IF tt-wid.wid-name = "YES" THEN
                  ASSIGN btn-import:LABEL = tt-wid.wid-label /* "&Yes" */.
                IF tt-wid.wid-name = "Cancel" THEN
                  ASSIGN btn-cancel:LABEL = tt-wid.wid-label /* "&Cancel" */.
              END.                            
          END CASE.
          RETURN.
        END.
    END CASE.
    
    /* Must be assigned before assigned to frame */
    IF tt-wid.wid-type NE "literal" AND tt-wid.wid-type NE "attrib" THEN DO:  
        IF tt-wid.wid-data-type GT "" THEN
            h_wid1:DATA-TYPE = tt-wid.wid-data-type NO-ERROR.
    end.
    IF tt-wid.wid-width GT 0  THEN
        ASSIGN h_wid1:WIDTH-CHARS = tt-wid.wid-width.
    IF tt-wid.wid-format GT "" AND tt-wid.wid-type NE "toggle" THEN
        h_wid1:FORMAT = tt-wid.wid-format.

    /* h_wid1:SIDE-LABEL-HANDLE = h_wid1-label  NO-ERROR. */
    IF tt-wid.wid-label GT "" THEN
        h_wid1:LABEL = tt-wid.wid-label.
    IF tt-wid.wid-type NE "literal" AND tt-wid.wid-type NE "attrib" THEN
    h_wid1:FRAME = FRAME xyz:HANDLE.

    IF tt-wid.wid-image NE "" THEN
        h_wid1:LOAD-IMAGE(search(tt-wid.wid-image))
         .
    IF tt-wid.wid-type NE "literal"  THEN DO:
     /* error if try to assign to a literal */
      h_wid1:SIDE-LABEL-HANDLE = h_wid1-label  NO-ERROR.
    END.

    IF tt-wid.wid-type EQ "rectangle" THEN
      h_wid1:FILLED = NO.

    IF tt-wid.wid-ROW GT 0 THEN
        h_wid1:ROW = tt-wid.wid-row.
    IF tt-wid.wid-col GT 0 THEN
        h_wid1:COL = tt-wid.wid-col.

        
    IF   tt-wid.wid-scr-value GT "" THEN
    h_wid1:SCREEN-VALUE = tt-wid.wid-scr-value.

    IF  tt-wid.is-enable THEN
        ASSIGN h_wid1:SENSITIVE = TRUE.
    
    IF tt-wid.wid-width GT 0 THEN
        h_wid1:WIDTH-CHARS = tt-wid.wid-width.

    IF tt-wid.wid-height GT 0 THEN
        h_wid1:HEIGHT-CHARS = tt-wid.wid-height.

    IF tt-wid.wid-name GT "" THEN
      h_wid1:NAME = tt-wid.wid-name.
    
    IF tt-wid.wid-type NE "toggle" AND tt-wid.wid-format GT "" THEN
        h_wid1:FORMAT = tt-wid.wid-format.
    
    IF tt-wid.wid-type EQ "literal" THEN
        h_wid1:AUTO-RESIZE = TRUE.

    IF tt-wid.wid-type EQ "Combo-box" AND tt-wid.wid-list GT "" THEN DO:    
        h_wid1:ADD-LAST(tt-wid.wid-list).
        h_wid1:SCREEN-VALUE = ENTRY(1, tt-wid.wid-list).
        IF NUM-ENTRIES(tt-wid.wid-list) LT 3 THEN
            h_wid1:INNER-LINES = 3.
        ELSE
            h_wid1:INNER-LINES = NUM-ENTRIES(tt-wid.wid-list).
    END.

    IF tt-wid.wid-type EQ "selection-list" AND tt-wid.wid-list GT "" THEN DO:   
        h_wid1:AUTO-RESIZE = TRUE.
        h_wid1:ADD-LAST(tt-wid.wid-list).
        h_wid1:SCREEN-VALUE = ENTRY(1, tt-wid.wid-list).
        IF NUM-ENTRIES(tt-wid.wid-list) LT 3 THEN
            h_wid1:INNER-LINES = 3.
        ELSE
            h_wid1:INNER-LINES = NUM-ENTRIES(tt-wid.wid-list).
    END.

    IF tt-wid.wid-type NE "literal" AND tt-wid.wid-type NE "attrib" THEN DO:
       IF /* necessary for toggle and literal */ tt-wid.wid-type NE "toggle" AND   tt-wid.wid-label GT "" THEN
           h_wid1-label:SCREEN-VALUE = tt-wid.wid-label.
    
        IF tt-wid.wid-data-type GT "" THEN
            h_wid1:DATA-TYPE = tt-wid.wid-data-type NO-ERROR.
        IF tt-wid.wid-init GT "" THEN
            h_wid1:SCREEN-VALUE = tt-wid.wid-init.
    END.
    IF tt-wid.wid-fgcol GT 0 THEN
       h_wid1:FGCOLOR = tt-wid.wid-fgcol NO-ERROR.
    IF tt-wid.wid-bgcol GT 0 THEN
       h_wid1:BGCOLOR = tt-wid.wid-bgcol NO-ERROR.
    IF tt-wid.wid-font GT 0 THEN
       h_wid1:FONT = tt-wid.wid-font NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-window-attri) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-window-attri Procedure 
PROCEDURE set-window-attri :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST tt-wid WHERE tt-wid.wid-type = "WIN" NO-LOCK NO-ERROR.
IF AVAIL tt-wid THEN DO:
    h_wid1 = tt-wid.wid-hand.
   IF tt-wid.wid-row GT 0 THEN
        RUN set-position (INPUT tt-wid.wid-row, INPUT tt-wid.wid-col). 
    IF tt-wid.wid-label GT "" THEN
        FRAME xyz:TITLE = tt-wid.wid-label. 
    IF tt-wid.wid-scrollbars THEN
        h_wid1:SCROLL-BARS = YES.

    IF tt-wid.wid-width GT 0 THEN
        ASSIGN CURRENT-WINDOW:WIDTH-CHARS = tt-wid.wid-width
               FRAME xyz:WIDTH-CHARS = tt-wid.wid-width - .5.
    IF tt-wid.wid-height GT 0 THEN
        ASSIGN CURRENT-WINDOW:HEIGHT-CHARS = tt-wid.wid-height
               FRAME xyz:HEIGHT-CHARS = tt-wid.wid-height - .5
        btn-no:ROW = tt-wid.wid-height - 3
        btn-import:ROW = tt-wid.wid-height - 3
        btn-cancel:ROW = tt-wid.wid-height - 3.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-get-attrib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-attrib Procedure 
FUNCTION get-attrib RETURNS CHARACTER
  ( ipcWidType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER bf-wid FOR tt-wid.
  FIND FIRST bf-wid WHERE bf-wid.wid-TYPE = "attrib"
       AND bf-wid.wid-name = ipcWidType NO-LOCK NO-ERROR.
  IF AVAIL bf-wid THEN
    RETURN bf-wid.wid-inp-val.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lookup-exists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION lookup-exists Procedure 
FUNCTION lookup-exists RETURNS LOGICAL
  ( ipcField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF LOOKUP(ipcField, gvcLookupList) GT 0 THEN
    RETURN TRUE.
  ELSE
    RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


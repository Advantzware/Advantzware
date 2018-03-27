&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : touch/translations.i
    Purpose     : translate object labels using multiple languages

    Syntax      : {touch/translations.i}

    Description :

    Author(s)   : Ron Stark
    Created     : 6.28.2005
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ************************************************************************ */
/* multiple languages - see procedure Label_Language and function Translate */
/* include touch/localview.i added to proc local-view in all smart objects  */
/* to override table entries, add comma list of translations to object's    */
/* private-data property                                                    */
/* ************************************************************************ */

DEFINE VARIABLE languageList AS CHARACTER NO-UNDO.
DEFINE VARIABLE flagList AS CHARACTER NO-UNDO.
DEFINE VARIABLE transString AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE translations NO-UNDO
  FIELD idxString AS CHARACTER
  FIELD translations AS CHARACTER
    INDEX translations IS PRIMARY UNIQUE idxString.

INPUT FROM VALUE(search('touch/translations.txt')) NO-ECHO.
IMPORT UNFORMATTED languageList. /* first row is list of avail languages */
IMPORT UNFORMATTED flagList. /* second row is list of language flag images */
REPEAT:
  IMPORT UNFORMATTED transString.
  IF CAN-FIND(translations WHERE translations.idxString EQ ENTRY(1,transString)) THEN
  NEXT. /* duplicate entry */
  CREATE translations.
  ASSIGN
    translations.idxString = ENTRY(1,transString) /* make englist work key */
    translations.translations = transString.
END.
INPUT CLOSE.

/* make list avail to other smart objects */
RUN Set_Value ('language_list',languageList) NO-ERROR.
RUN Set_Value ('flag_list',flagList) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD translate Include 
FUNCTION translate RETURNS CHARACTER
  (ipString AS CHARACTER,ipCaps AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Label_Language Include 
PROCEDURE Label_Language :
/*------------------------------------------------------------------------------
  Purpose:     change labels to selected language by walking widget tree
               looking for values in private-data
  Parameters:  frame handle of smart object
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER current-frame AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  
  ASSIGN
    current-widget = current-frame
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA NE ? THEN
    DO:
      /* this object uses an image, not text */
      IF INDEX(current-widget:PRIVATE-DATA,'images') NE 0 THEN
      ldummy = current-widget:LOAD-IMAGE(translate(current-widget:PRIVATE-DATA,NO)).
      ELSE
        IF current-widget:TYPE EQ "EDITOR" THEN
        current-widget:SCREEN-VALUE = translate(current-widget:PRIVATE-DATA,NO).
        ELSE
        current-widget:LABEL = translate(current-widget:PRIVATE-DATA,NO /*
                                         current-widget:TYPE EQ 'Button'*/ ).
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION translate Include 
FUNCTION translate RETURNS CHARACTER
  (ipString AS CHARACTER,ipCaps AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  translate input word(s) into desire language
    Notes:  languageList populated in definitions section
------------------------------------------------------------------------------*/
  DEFINE VARIABLE labelLanguage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE rtnString AS CHARACTER NO-UNDO.

  RUN Get_Value ('label_language':U,OUTPUT labelLanguage). /* language */
  idx = LOOKUP(labelLanguage,languageList). /* list of languages */
  IF idx EQ 0 THEN idx = 1. /* didn't find it, so assume english */
  
  /* multiple entries in object:private-data, use them instead of table */
  IF NUM-ENTRIES(ipString) GT 1 THEN rtnString = ENTRY(idx,ipString).
  ELSE /* already comes in as english, simply return it, save processing */
  IF idx EQ 1 THEN rtnString = ipString.
  ELSE DO: /* find translation in temp table loaded from translations.txt */
    FIND translations NO-LOCK WHERE translations.idxString EQ ipString NO-ERROR.
    rtnString = IF AVAILABLE translations AND
                NUM-ENTRIES(translations.translations) GE idx THEN
                ENTRY(idx,translations.translations)
                /* no translation for image file */
           ELSE IF INDEX(ipString,'images') NE 0 THEN ipString
           ELSE '*' + ipString + '*'. /* no translation exists */
  END.
  /* only english returns in caps, otherlanguages do not fit in objects */
  RETURN IF ipCaps AND idx EQ 1 THEN CAPS(rtnString) ELSE rtnString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


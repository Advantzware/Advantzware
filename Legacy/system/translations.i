&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : system/translations.i
    Purpose     : translate object labels using multiple languages

    Syntax      : {system/translations.i}

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

DEFINE VARIABLE cLabelLanguage AS CHARACTER NO-UNDO INITIAL "English".
DEFINE VARIABLE cLanguageList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFlagList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTransString   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTranslations NO-UNDO
  FIELD idxString    AS CHARACTER
  FIELD translations AS CHARACTER
    INDEX translations IS PRIMARY UNIQUE idxString.

INPUT FROM VALUE(SEARCH("menuTrans.dat")) NO-ECHO.
IMPORT UNFORMATTED cLanguageList. /* first row is list of avail languages */
IMPORT UNFORMATTED cFlagList. /* second row is list of language flag images */
REPEAT:
  IMPORT UNFORMATTED cTransString.
  IF CAN-FIND(ttTranslations WHERE ttTranslations.idxString EQ ENTRY(1,cTransString)) THEN
  NEXT. /* duplicate entry */
  CREATE ttTranslations.
  ASSIGN
    ttTranslations.idxString    = ENTRY(1,cTransString) /* make englist work key */
    ttTranslations.translations = cTransString
    .
END.
INPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD translate Include 
FUNCTION fTranslate RETURNS CHARACTER
  (ipcString AS CHARACTER,iplCaps AS LOGICAL)  FORWARD.

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
PROCEDURE pChangeLanguage :
/*------------------------------------------------------------------------------
  Purpose:     change labels to selected language by walking widget tree
               looking for values in private-data
  Parameters:  frame handle of smart object
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iphCurrentFrame AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE hWidget AS WIDGET-HANDLE NO-UNDO.
  
  ASSIGN
    hWidget = iphCurrentFrame
    hWidget = hWidget:FIRST-CHILD
    hWidget = hWidget:FIRST-CHILD.
  DO WHILE hWidget NE ?:
    IF hWidget:PRIVATE-DATA NE ? THEN DO:
      /* this object uses an image, not text */
      IF INDEX(hWidget:PRIVATE-DATA,'images') NE 0 THEN
      hWidget:LOAD-IMAGE(fTranslate(hWidget:PRIVATE-DATA,NO)).
      ELSE
        IF hWidget:TYPE EQ "EDITOR" THEN
        hWidget:SCREEN-VALUE = fTranslate(hWidget:PRIVATE-DATA,NO).
        ELSE
        hWidget:LABEL = fTranslate(hWidget:PRIVATE-DATA,NO).
    END.
    hWidget = hWidget:NEXT-SIBLING.
  END.
  &IF DEFINED(VDC) NE 0 &THEN
  iphCurrentFrame:TITLE = fTranslate(iphCurrentFrame:PRIVATE-DATA,NO).
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION translate Include 
FUNCTION fTranslate RETURNS CHARACTER
  (ipcString AS CHARACTER,iplCaps AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  translate input word(s) into desire language
    Notes:  cLanguageList populated in definitions section
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLanguage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx       AS INTEGER NO-UNDO.
  DEFINE VARIABLE cReturn   AS CHARACTER NO-UNDO.

  cLanguage = cLabelLanguage.
  idx = LOOKUP(cLanguage,cLanguageList). /* list of languages */
  IF idx EQ 0 THEN idx = 1. /* didn't find it, so assume english */
  
  /* multiple entries in object:private-data, use them instead of table */
  IF NUM-ENTRIES(ipcString) GT 1 THEN cReturn = ENTRY(idx,ipcString).
  ELSE /* already comes in as english, simply return it, save processing */
  IF idx EQ 1 THEN cReturn = ipcString.
  ELSE DO: /* find translation in temp table loaded from translations.txt */
    FIND ttTranslations WHERE ttTranslations.idxString EQ ipcString NO-ERROR.
    cReturn = IF AVAILABLE ttTranslations AND
                NUM-ENTRIES(ttTranslations.translations) GE idx THEN
                ENTRY(idx,ttTranslations.translations)
                /* no translation for image file */
           ELSE IF INDEX(ipcString,'images') NE 0 THEN ipcString
           ELSE '*' + ipcString + '*'. /* no translation exists */
  END.
  /* only english returns in caps, otherlanguages do not fit in objects */
  RETURN IF iplCaps AND idx EQ 1 THEN CAPS(cReturn) ELSE cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


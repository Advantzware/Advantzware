&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*-------------------------------------------------------------------------
    File        : viewer.i  
    Purpose     : Basic SmartViewer methods for the ADM
  
    Syntax      : {src/adm/method/viewer.i}

    Description :
  
    Author(s)   :
    Created     :
    Notes       :
    HISTORY: 
-------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(adm-viewer) = 0 &THEN
&GLOBAL adm-viewer yes
/* Dialog program to run to set runtime attributes - if not defined in master */
&IF DEFINED(adm-attribute-dlg) = 0 &THEN
&SCOP adm-attribute-dlg adm/support/viewerd.w
&ENDIF

/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */
&IF DEFINED(adm-attribute-list) = 0 &THEN
&SCOP adm-attribute-list Initial-Lock,Hide-on-Init,Disable-on-Init,Key-Name,~
Layout,Create-On-Add
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 6.88
         WIDTH              = 66.
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{src/adm/method/record.i}
{src/adm/method/tableio.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

  /* Initialize attributes for update processing objects. */
  RUN set-attribute-list ('FIELDS-ENABLED=no,ADM-NEW-RECORD=no':U).
 

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-viewer-identifier) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewer-identifier Method-Library 

  PROCEDURE viewer-identifier:
    /* Purpose: To make external programs identify this is a viewer using current procedure handle. */
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-position) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-position Method-Library 
PROCEDURE get-position :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified position.
  Parameters:  ROW and COLUMN 
  Notes:       
-------------------------------------------------------------*/
  
    DEFINE OUTPUT PARAMETER p-row    AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER p-col    AS DECIMAL NO-UNDO.
    
    IF VALID-HANDLE(adm-object-hdl) THEN         
     /* Get object's position */
      ASSIGN p-row = adm-object-hdl:ROW        
             p-col = adm-object-hdl:COLUMN
             NO-ERROR.         
    RETURN.
  &global-define EXCLUDE-get-position true  
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-size) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-size Method-Library 
PROCEDURE get-size :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified size.
  Parameters:  ROW and COLUMN 
  Notes:       
-------------------------------------------------------------*/
  
    DEFINE OUTPUT PARAMETER p-height   AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER p-width    AS DECIMAL NO-UNDO.     
      /* Get object's size */
      ASSIGN p-height = adm-object-hdl:HEIGHT-CHARS
             p-width = adm-object-hdl:WIDTH-CHARS
             NO-ERROR.    
    RETURN.
  &global-define EXCLUDE-get-size true  
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
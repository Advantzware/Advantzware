&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        :   pt/ipSetSecurity.i
    Purpose     :   Check security and label language by viewer widget
    Syntax      :
    Author(s)   :   MYT
    Created     :   07/14/16
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEF VAR h_frame AS HANDLE.
    DEF VAR h_field AS HANDLE.
    DEF VAR h_continue AS HANDLE.
  
    ASSIGN 
        h_frame = FRAME {&FRAME-NAME}:HANDLE.
    
    IF VALID-HANDLE (h_frame) THEN DO:
        ASSIGN
            h_field = h_frame:FIRST-CHILD  /* field-group */.
            h_field = h_field:FIRST-CHILD. /* first field */
        REPEAT:
            IF h_field:NAME <> ? THEN DO:
                IF CAN-DO(no-display, h_field:NAME) THEN ASSIGN 
                    h_field:HIDDEN = TRUE.
                IF CAN-DO(no-update, h_field:NAME) THEN ASSIGN 
                    h_field:READ-ONLY = TRUE.
                IF sv-userlang <> "ENG" THEN DO:
                    IF CAN-DO("fill-in,combo-box,toggle-box,button",h_field:TYPE)
                    AND h_field:LABEL <> "" THEN DO:
                        FIND FIRST z_translate WHERE
                            z_translate.langcode = sv-userlang AND
                            z_translate.sourceWord = h_field:LABEL
                            NO-LOCK NO-ERROR.
                        IF AVAIL z_translate THEN ASSIGN
                            h_field:LABEL = z_translate.targetWord.
                    END.
                END.
            END.  
            ASSIGN 
                h_field = h_field:NEXT-SIBLING.
            IF NOT VALID-HANDLE (h_field) THEN LEAVE.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
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



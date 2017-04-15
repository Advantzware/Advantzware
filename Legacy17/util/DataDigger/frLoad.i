&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getData Include 
FUNCTION getData returns character
  ( pcName as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setData Include 
FUNCTION setData returns logical
  ( pcName as character 
  , pcValue as character )  FORWARD.

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
         HEIGHT             = 13.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideFrame Include 
PROCEDURE hideFrame :
/*------------------------------------------------------------------------------
  Purpose:     hide the frame
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  frame {&frame-name}:hidden = yes.
  
end procedure. /* hideFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFrame Include 
PROCEDURE setFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define input  parameter piFrame-x as integer     no-undo.
  define input  parameter piFrame-y as integer     no-undo.
  define input  parameter piFrame-w as integer     no-undo.
  define input  parameter piFrame-h as integer     no-undo.

  if piFrame-x <> ? then frame {&frame-name}:x = piFrame-x.

  if piFrame-y <> ? then frame {&frame-name}:y = piFrame-y.

  if piFrame-w <> ? then 
  do:
    frame {&frame-name}:width-pixels  = piFrame-w.
    frame {&frame-name}:virtual-width-pixels  = piFrame-w.
  end.

  if piFrame-h <> ? then 
  do:
    frame {&frame-name}:height-pixels = piFrame-h.
    frame {&frame-name}:virtual-height-pixels = piFrame-h.
  end.

END PROCEDURE. /* setFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewFrame Include 
PROCEDURE viewFrame :
/*------------------------------------------------------------------------------
  Purpose:     hide the frame
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  frame {&frame-name}:hidden = no.
  apply 'entry' to frame {&frame-name}.

end procedure. /* viewFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getData Include 
FUNCTION getData returns character
  ( pcName as character ) :

  return dynamic-function('getData' in phParent, pcName).

end function. /* getData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setData Include 
FUNCTION setData returns logical
  ( pcName as character 
  , pcValue as character ) :

  return dynamic-function('setData' in phParent, pcName, pcValue).

end function. /* setData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


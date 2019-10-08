&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : util/TextWrap.i
    Purpose     : Modify a paragraph to wrap the text.

    Syntax      : {util/TextWrap.i}

    Description : A set of procedures and functions that accepts 
                  a paragraph and inserts carriage returns in the
                  appropriate places in order to wrap the text to
                  fit into a predetermined column width.

    Author(s)   : Stacey Brooks 
    Created     : 02/07/2007
    
    Notes       : Include this file in your definitions.
                  When you need to format a paragraph, just
                  send it to procedure "FormatParagraph"
                  and it will be returned properly formatted.
                  
                  Syntax: 
                  RUN FormatParagraph (INPUT-OUTPUT pcText,
                                       INPUT 80).
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addCarriageReturn Include 
FUNCTION addCarriageReturn RETURNS CHARACTER
  ( INPUT pcParagraph AS CHAR,
    INPUT pcLine AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addText Include 
FUNCTION addText RETURNS CHARACTER
  ( INPUT pcLine  AS CHAR,
    INPUT pcText  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TextExceedsWidth Include 
FUNCTION TextExceedsWidth RETURNS LOGICAL
  ( INPUT pcNextWord   AS CHAR,
    INPUT pcInputLine  AS CHAR,
    INPUT piColWidth   AS INT )  FORWARD.

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
         HEIGHT             = 9.67
         WIDTH              = 53.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FormatParagraph Include 
PROCEDURE FormatParagraph :
/*------------------------------------------------------------------------------
  Purpose:     Check the paragraph and insert all necessary carriage returns
               where they are needed to ensure the text wraps within the
               specified width.
  Parameters:  <none>
  Notes:       This procedure should be executed only when the entire
               paragraph exceeds the specified width (regardless of whether
               it contains carriage returns or not).  Even if the paragraph
               contains carriage returns, the individual lines could still
               exceed the specified column width.
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER pcParagraph  AS CHAR NO-UNDO.
    DEFINE INPUT        PARAMETER piColWidth   AS INT NO-UNDO.

    DEF VAR vcTempLine     AS CHAR NO-UNDO INIT "".
    DEF VAR vcNewParagraph AS CHAR NO-UNDO INIT "".
    DEF VAR viNumLines     AS INT  NO-UNDO INIT 0.
    DEF VAR viCount        AS INT  NO-UNDO INIT 0.


    /* If the paragraph does not exceed the specified
       column width, then do nothing and return. */
    IF LENGTH(pcParagraph) <= piColWidth THEN RETURN.


    /* If there are no carriage returns, then insert them
       in all the right places and return it back. */
    IF NUM-ENTRIES(pcParagraph,CHR(10)) = 0 THEN DO:
        RUN InsertCarriageReturns(INPUT-OUTPUT pcParagraph,
                                  INPUT piColWidth).
        RETURN.
    END.


    /* Else if there are pre-existing carriage returns, then check each 
       line to see if more carriage returns are needed to further wrap 
       the text. */
    ELSE DO:

        /* Determine how many lines are in the Paragraph based on 
           how many pre-existing carriage returns are embedded. */
        ASSIGN viNumLines = NUM-ENTRIES(pcParagraph,CHR(10)).


        /* Check each line to see if it exceeds the specified width. */
        DO viCount = 1 TO viNumLines:
            
            /* Get the current line up to the carriage return. */
            ASSIGN vcTempLine = ENTRY(viCount,pcParagraph,CHR(10)).

            /* If the current line exceeds the specified width... */
            IF LENGTH(vcTempLine) > piColWidth THEN DO:

                /* Insert carriage returns into this line. */
                RUN InsertCarriageReturns (INPUT-OUTPUT vcTempLine,
                                           INPUT piColWidth).

                /* Reattach the original carriage return. */
                ASSIGN vcTempLine = vcTempLine + CHR(10).
            END.
            /* Else if the current line does not exceed the width... */
            ELSE
                /* Just add the carriage return back to it. */
                ASSIGN vcTempLine = vcTempLine + CHR(10).
                
            /* Attach this line to the new Paragraph
               (reformatted or not). */
            ASSIGN vcNewParagraph = addText(vcNewParagraph,vcTempLine).
        END.

        /* Assign the new paragraph to be returned. */
        ASSIGN pcParagraph = vcNewParagraph.
    END.


    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InsertCarriageReturns Include 
PROCEDURE InsertCarriageReturns PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Insert carriage returns into a paragraph in order to wrap the
               text to fit into a column of a specific column width.
               
  Parameters:  character - paragraph to edit
               integer   - column width (num characters).
               
  Notes:       This is a private procedure.  It should only be executed by
               procedure "FormatParagraph".
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER pcParagraph  AS CHAR NO-UNDO.
    DEFINE INPUT        PARAMETER piColWidth   AS INT NO-UNDO.

    DEF VAR vcTempLine       AS CHAR NO-UNDO INIT "".
    DEF VAR vcNewParagraph   AS CHAR NO-UNDO INIT "".
    DEF VAR viNumWords       AS INT  NO-UNDO INIT 0.
    DEF VAR viCount          AS INT  NO-UNDO INIT 0.
    



    /* Get the number of words in the input paragraph. */
    ASSIGN viNumWords = NUM-ENTRIES(pcParagraph," ").


    /* Build a new paragraph by processing 
       each word in the input paragraph. */
    DO viCount = 1 TO viNumWords:

        /* If the current word forces the temporary line to exceed the specified column width... */
        IF TextExceedsWidth(ENTRY(viCount,pcParagraph," "),vcTempLine,piColWidth) THEN
        DO:
            /* Add the current line to the new paragraph with a carriage return. */
            ASSIGN vcNewParagraph = addCarriageReturn(vcNewParagraph,vcTempLine).
            /* Clear the temporary line. */
            ASSIGN vcTempLine = "".
            /* Add the current word to the temporary line. */
            ASSIGN vcTempLine = addText(vcTempLine,ENTRY(viCount,pcParagraph," ")).
        END.
        /* Else if the current word will not exceed the width... */
        ELSE
            /* Just add the current word to the temporary line. */
            ASSIGN vcTempLine = addText(vcTempLine,ENTRY(viCount,pcParagraph," ")).
    END.

    /* If the temporary line still contains a partial sentence 
      (end of paragraph did not reach end of line)... */
    IF vcTempLine <> "" THEN
       /* Add the remaining text to the new paragraph. */
        ASSIGN vcNewParagraph = addText(vcNewParagraph,vcTempLine).

    /* Assign the new paragraph to be returned. */
    ASSIGN pcParagraph = vcNewParagraph.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addCarriageReturn Include 
FUNCTION addCarriageReturn RETURNS CHARACTER
  ( INPUT pcParagraph AS CHAR,
    INPUT pcLine AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Add a line and a carriage return to the end of a paragraph
    Notes:  
------------------------------------------------------------------------------*/
  
  ASSIGN pcParagraph = pcParagraph + pcLine + CHR(10).

  RETURN pcParagraph.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addText Include 
FUNCTION addText RETURNS CHARACTER
  ( INPUT pcLine  AS CHAR,
    INPUT pcText  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Add a space and a line of text to an existing line.
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN pcLine = pcLine + " " + pcText.

  RETURN pcLine.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TextExceedsWidth Include 
FUNCTION TextExceedsWidth RETURNS LOGICAL
  ( INPUT pcNextWord   AS CHAR,
    INPUT pcInputLine  AS CHAR,
    INPUT piColWidth   AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Determine if a block of text will make the line exceed 
            the specified column width.
    Notes: 
    Usage: IF TextExceedsWidth(vcWord,vcLine,viColWidth) THEN. 
------------------------------------------------------------------------------*/

    IF LENGTH(pcInputLine + " " + pcNextWord) > piColWidth THEN
        RETURN TRUE.
    ELSE
        RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


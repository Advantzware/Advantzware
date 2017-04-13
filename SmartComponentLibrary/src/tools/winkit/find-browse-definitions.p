/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : find-browse-definitions.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Mar 09 09:42:19 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH . 

DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcFile       AS LONGCHAR      NO-UNDO.
DEFINE VARIABLE oBrowseNames AS CharacterList NO-UNDO . 

DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLines AS INTEGER   NO-UNDO.

DEFINE VARIABLE iStart  AS INTEGER NO-UNDO.
DEFINE VARIABLE iEnd    AS INTEGER NO-UNDO.
DEFINE VARIABLE lcBlock AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

/* ***************************  Main Block  *************************** */

COPY-LOB FROM FILE pcFileName TO lcFile . 

ASSIGN oBrowseNames = NEW CharacterList () 
       iLines       = NUM-ENTRIES (lcFile, System.Environment:NewLine) .

DO i = 1 TO iLines:
    ASSIGN cLine = ENTRY (i, lcFile, System.Environment:NewLine) .
    
    IF cLine MATCHES "*_UIB-PREPROCESSOR-BLOCK-END*":U THEN LEAVE . 
    
    IF cLine MATCHES "*Definitions for BROWSE*":U THEN 
        oBrowseNames:Add (ENTRY (5, cLine, " ":U)) .
END.

ASSIGN lChanged = FALSE . 

{Consultingwerk/foreachPrimitiveList.i CHARACTER cBrowseName in oBrowseNames}
    
    /* When we cannot find an include reference for display-fields-in-browse.i for this browse,
       add one to the main block */
    
    /* When WinKitFormType not yet set, add it */
    IF INDEX (lcFile, "display-fields-in-browse.i " + cBrowseName) = 0 THEN DO:
        
        ASSIGN lChanged = TRUE 
        
               iStart = INDEX (lcFile, "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK":U) 
               iEnd   = INDEX (lcFile, "/* _UIB-CODE-BLOCK-END */":U, iStart) 
        
               lcBlock = SUBSTRING (lcFile, iStart, iEnd - iStart) .
        
        lcBlock = lcBlock + 
                  System.Environment:NewLine +
                  SUBSTITUTE ("~{ src/winkit/display-fields-in-browse.i &1 ~}", cBrowseName)  + 
                  System.Environment:NewLine + System.Environment:NewLine .  
                  
        SUBSTRING (lcFile, iStart, iEnd - iStart) = lcBlock .
    END.    
END.

IF lChanged = TRUE THEN 
    COPY-LOB FROM lcFile TO FILE pcFileName.

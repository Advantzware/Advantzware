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
    File        : refactor-nonblock-trigger.p
    Purpose     : Refactors all trigger blocks located by find-nonblock-triggers.p
                  into triggers defined using a DO: ... END. block and not
                  just a single statement

    Syntax      :

    Description : This file is part of the WinKit MTK

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 30 15:03:00 CEST 2011
    Notes       : Please ensure, that no changes are made to the source files 
                  after the find-nonblock-triggers.p has been executed
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{tools/winkit/ttTrigger.i}

DEFINE VARIABLE lcFile  AS LONGCHAR NO-UNDO .

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE VARIABLE cTriggerCode AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cZeile       AS CHARACTER NO-UNDO.

&GLOBAL-DEFINE dlm CHR(10)

/* ***************************  Main Block  *************************** */

TEMP-TABLE ttTrigger:READ-XML ("FILE":U, 
                               "c:\temp\non-block-trigger.xml":U, 
                               "EMPTY":U,
                               ?,
                               ?) .
                               
FOR EACH ttTrigger 
  WHERE ttTrigger.ProcedureName = "":U 
     OR ttTrigger.ProcedureName = "disable_UI":U  

    BREAK BY ttTrigger.FileName
          BY ttTrigger.LineAnfang DESCENDING:
  
  IF FIRST-OF (ttTrigger.FileName) THEN DO: 
    COPY-LOB FROM FILE ttTrigger.FileName TO lcFile .
    
    DISPLAY ttTrigger.FileName FORMAT "x(70)":U WITH DOWN .
    DOWN .
    PAUSE 0 BEFORE-HIDE . 
    PROCESS EVENTS . 
  END.

  /* handle beginning line */
  ASSIGN cZeile = ENTRY (ttTrigger.EnclosedStartLine, lcFile, {&dlm})   
         cTriggerCode = FILL (" ":U, ttTrigger.ColumnAnfang - 1) + "DO:":U + {&dlm} + FILL (" ":U, ttTrigger.ColumnAnfang + 1) + SUBSTRING (cZeile, ttTrigger.EnclosedStartColumn) . 
         
   IF ttTrigger.EnclosedStartColumn > 1 THEN 
      cZeile = SUBSTRING (cZeile, 1, ttTrigger.EnclosedStartColumn - 1).
   ELSE cZeile = "":U . 
  
  ENTRY (ttTrigger.EnclosedStartLine, lcFile, {&dlm}) = cZeile.

  IF ttTrigger.EnclosedEndLine > ttTrigger.EnclosedStartLine THEN DO: 
      DO i = ttTrigger.EnclosedStartLine + 1 TO ttTrigger.EnclosedEndLine - 1:
          ASSIGN cTriggerCode = cTriggerCode + {&dlm} + FILL (" ":U, ttTrigger.ColumnAnfang + 2) + ENTRY (i, lcFile, {&dlm}) .
      END.
  
      ASSIGN cZeile = ENTRY (ttTrigger.EnclosedEndLine, lcFile, {&dlm}) .
             cTriggerCode = cTriggerCode + {&dlm} + 
                            SUBSTRING (cZeile, 1, ttTrigger.EnclosedEndColumn) .
  END.

  cTriggerCode = cTriggerCode + {&dlm} + FILL (" ":U, ttTrigger.ColumnAnfang - 1) + "END. ":U + {&dlm}.

  /* handle ending line */
  IF ttTrigger.EnclosedEndLine > ttTrigger.EnclosedStartLine THEN DO:
    ASSIGN cZeile = ENTRY (ttTrigger.EnclosedEndLine, lcFile, {&dlm})
           cZeile = FILL (" ":U, ttTrigger.ColumnAnfang) + SUBSTRING (cZeile, ttTrigger.EnclosedEndColumn + 1).

    ENTRY (ttTrigger.EnclosedEndLine, lcFile, {&dlm}) = cZeile .
  END.

  /* remove lines between start and end */
  IF ttTrigger.EnclosedEndLine > ttTrigger.EnclosedStartLine + 1 THEN
  DO i = ttTrigger.EnclosedEndLine TO ttTrigger.EnclosedStartLine + 1 BY -1:
      lcFile = Consultingwerk.Util.ListHelper:RemoveEntry (lcFile, i , {&dlm}) .
  END.

  /* insert Triggercode */
  lcFile = Consultingwerk.Util.ListHelper:InsertEntry (lcFile, ttTrigger.EnclosedStartLine + 1, STRING (cTriggerCode), {&dlm}) .

  /* Line before Trigger empty? */
  IF TRIM (REPLACE (ENTRY (ttTrigger.EnclosedStartLine, lcFile, {&dlm}), CHR(9), "":U)) = "":U 
  THEN DO: 
    lcFile = Consultingwerk.Util.ListHelper:RemoveEntry (lcFile, ttTrigger.EnclosedStartLine, {&dlm}) .
  END.
  
  IF LAST-OF (ttTrigger.FileName) THEN
    COPY-LOB FROM lcFile TO FILE ttTrigger.FileName .

  CATCH err AS Progress.Lang.Error :
  		MESSAGE ttTrigger.FileName SKIP (2) 
  		        err:GetMessage (1)
      VIEW-AS ALERT-BOX.
  END CATCH.

END .

MESSAGE "done"
    VIEW-AS ALERT-BOX.

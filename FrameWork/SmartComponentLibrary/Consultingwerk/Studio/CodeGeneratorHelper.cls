/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CodeGeneratorHelper
    Purpose     : Contains supporting routines for code generators
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 03 11:28:45 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.Studio.*    FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.Studio.CodeGeneratorHelper:

    /**
     * Purpose: Constructor for the CodeGeneratorHelper class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED CodeGeneratorHelper ():

    END CONSTRUCTOR.

    /**
     * Purpose: Verifies if the source code has been modified outside of comments
     * Notes:
     * @param plcSourceCode The new source code
     * @param pcFileName The file name to verify against
     * @return Logical value indicating if the source contains changes outside of comments
     */
    METHOD PUBLIC STATIC LOGICAL HasNonCommentChanges (plcSourceCode AS LONGCHAR,
                                                       pcFileName AS CHARACTER):

        DEFINE VARIABLE lcFileCode   AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcSourceCode AS LONGCHAR NO-UNDO .

        IF NOT FileHelper:Exists (pcFileName) THEN
            RETURN TRUE .

        COPY-LOB FROM FILE pcFileName TO lcFileCode .

        ASSIGN lcSourceCode = CodeGeneratorHelper:SourceWithoutComments(plcSourceCode)
               lcFileCode   = CodeGeneratorHelper:SourceWithoutComments(lcFileCode) .

        RETURN COMPARE (lcSourceCode, "NE":U, lcFileCode, "CASE-SENSITIVE":U) .

    END METHOD .

    /**
     * Purpose: Verifies if the source code has been modified outside of comments
     * Notes:
     * @param plcSourceCode The new source code
     * @return The source code without any comment block
     */
    METHOD PUBLIC STATIC LONGCHAR SourceWithoutComments (plcSourceCode AS LONGCHAR):

        DEFINE VARIABLE iPos AS INTEGER NO-UNDO INIT 1.
        DEFINE VARIABLE iEnd AS INTEGER NO-UNDO INIT 1.

        DO WHILE iPos > 0:

            iPos = INDEX (plcSourceCode, "/*":U, iPos) .

            IF iPos > 0 THEN DO:

                iEnd = StringHelper:FindClosingComment (plcSourceCode, iPos) .

                IF iEnd > 0 THEN
                    /* SCL-1495: need to set to blank instead of spaces as we need to ignore
                     *  differences in length of comment (e.g. different author name) */
                    SUBSTRING (plcSourceCode, iPos, iEnd - iPos + 1, "CHARACTER":U) = "":U .
            END.
        END.

        ASSIGN plcSourceCode = REPLACE (plcSourceCode, CHR (13), "":U)
               plcSourceCode = REPLACE (plcSourceCode, CHR (9), "":U) .

        ASSIGN iPos = ? .

        DO WHILE LENGTH (plcSourceCode, "CHARACTER":U) <> iPos:
            ASSIGN plcSourceCode = REPLACE (plcSourceCode, "  ":U, " ":U)
                   iPos          = LENGTH (plcSourceCode, "CHARACTER":U) .
        END.

        RETURN plcSourceCode .

    END METHOD.

END CLASS.

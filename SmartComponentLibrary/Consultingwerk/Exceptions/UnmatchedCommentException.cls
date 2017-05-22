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
    File        : UnmatchedCommentException
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 29 12:40:14 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.UnmatchedCommentException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the Source String associated with the Exception
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SourceString AS LONGCHAR NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Returns the Starting Position associated with the Exception
     * Notes:
     */
    DEFINE PUBLIC PROPERTY StartingPosition AS INTEGER NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor for the UnmatchedCommentException class
     * Notes:
     * @param pcSourceString The source string that should contain a closing quote
     * @param piStartingPosition The starting position, which is the position of the opening quote
     */
    CONSTRUCTOR PUBLIC UnmatchedCommentException (pcSourceString AS LONGCHAR,
                                                  piStartingPosition AS INTEGER):

        DEFINE VARIABLE cMessageString AS CHARACTER NO-UNDO.

        SUPER () .

        IF piStartingPosition > 0 AND piStartingPosition < LENGTH (pcSourceString, "CHARACTER":U) THEN DO:
            ASSIGN cMessageString = SUBSTRING (pcSourceString, piStartingPosition, 40, "CHARACTER":U) .

            THIS-OBJECT:AddMessage (SUBSTITUTE ("No comment terminator for the comment starting with ~"&1~" at position &2"{&TRAN},
                                                cMessageString,
                                                piStartingPosition),
                                    0).
        END.
        ELSE
            THIS-OBJECT:AddMessage (SUBSTITUTE ("No comment terminator for the comment starting position &1"{&TRAN},
                                                piStartingPosition),
                                    0).


        ASSIGN THIS-OBJECT:SourceString     = pcSourceString
               THIS-OBJECT:StartingPosition = piStartingPosition .

    END CONSTRUCTOR.

END CLASS.

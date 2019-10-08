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
    File        : StopConditionException
    Purpose     : Exception thrown when a STOP condition has been catched
    Syntax      :
    Description :
    Author(s)   : Mike Fechner, Consultingwerk Ltd.
    Created     : Wed May 01 19:23:58 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*       FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.StopConditionException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor for the StopConditionException class
     * Notes:
     * @param pcErrorString The error message associated with this StopConditionException object
     * @param piMessageNumber The error message number associated with this StopConditionException object
     */
    CONSTRUCTOR PUBLIC StopConditionException (pcErrorString   AS CHARACTER,
                                               piMessageNumber AS INTEGER):

        SUPER (INPUT pcErrorString, INPUT piMessageNumber).

        THIS-OBJECT:AddRuntimeMessage() .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the StopConditionException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC StopConditionException ():
        SUPER ("A stop condition has occurred."{&TRAN}, 0 ).

        THIS-OBJECT:AddRuntimeMessage() .

    END CONSTRUCTOR.

    /**
     * Purpose: Adds the actual runtime message to the Exception
     * Notes:
     */
    METHOD PROTECTED VOID AddRuntimeMessage():

        DEFINE VARIABLE iMsgNum AS INTEGER NO-UNDO.

        ASSIGN iMsgNum = _msg(1) .

        IF iMsgNum > 0 THEN
            THIS-OBJECT:AddMessage (ErrorHelper:GetRuntimeErrorMessage (iMsgNum), iMsgNum) .

    END METHOD.

END CLASS.

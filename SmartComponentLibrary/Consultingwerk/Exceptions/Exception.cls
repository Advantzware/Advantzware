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
    File        : Exception
    Purpose     : Common base error class, implementing inner exception
                  handling / access, returning the Error instance
                  that caused the current exception.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 20 20:19:00 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.Exception
    INHERITS AppError
    IMPLEMENTS Consultingwerk.Exceptions.ISupportsInnerException, IProvidesSessionInfo
    {&SERIALIZABLE}:

    /**
     * Purpose: Returns the Error instance that caused the current exception.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY InnerException AS Error NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Returns the Information about the Session the error occurred in
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SessionInfo AS SessionInfo NO-UNDO
    GET.
    PRIVATE SET.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     */
    CONSTRUCTOR PUBLIC Exception ():

        SUPER ().

        IF SESSION:REMOTE THEN DO:
            THIS-OBJECT:SessionInfo = NEW SessionInfo () .
            THIS-OBJECT:SessionInfo:Initialize () .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC Exception (pcErrorString AS CHARACTER,
                                  piMessageNumber AS INTEGER):

        SUPER (pcErrorString, piMessageNumber).

        IF pcErrorString > "":U AND THIS-OBJECT:NumMessages = 0 THEN
            THIS-OBJECT:AddMessage (pcErrorString, piMessageNumber) .

        IF SESSION:REMOTE THEN DO:
            THIS-OBJECT:SessionInfo = NEW SessionInfo () .
            THIS-OBJECT:SessionInfo:Initialize () .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC Exception (pcErrorString AS CHARACTER):

        SUPER (pcErrorString).

        IF SESSION:REMOTE THEN DO:
            THIS-OBJECT:SessionInfo = NEW SessionInfo () .
            THIS-OBJECT:SessionInfo:Initialize () .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC Exception (poInnerException AS Error):

        SUPER ().

        THIS-OBJECT:InnerException = poInnerException .

        IF SESSION:REMOTE THEN DO:
            THIS-OBJECT:SessionInfo = NEW SessionInfo () .
            THIS-OBJECT:SessionInfo:Initialize () .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC Exception (poInnerException AS Error,
                                  pcErrorString AS CHARACTER,
                                  piMessageNumber AS INTEGER):

        THIS-OBJECT (pcErrorString, piMessageNumber).

        THIS-OBJECT:InnerException = poInnerException .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC Exception (poInnerException AS Error,
                                  pcErrorString AS CHARACTER):

        SUPER (pcErrorString).

        THIS-OBJECT:InnerException = poInnerException .

        IF SESSION:REMOTE THEN DO:
            THIS-OBJECT:SessionInfo = NEW SessionInfo () .
            THIS-OBJECT:SessionInfo:Initialize () .
        END.

    END CONSTRUCTOR.

    /**
     * Purpose: Adds error messages from another error to this Exception instance
     * Notes:
     * @param poError The error instance to add errors from
     */
    METHOD PUBLIC VOID AddMessagesFrom (poError AS Error):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO poError:NumMessages:
            THIS-OBJECT:AddMessage (poError:GetMessage (i), poError:GetMessageNum (i)) .
        END.

    END METHOD .

    /**
     * Purpose: Creates an Exception using the given error message and number and
     *          Existing Error
     * Notes:   The messages from the existing error will be added to the new error
     *          are well, useful for adding messages to ABL runtime errors
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     * @param poError The reference to the original error
     * @return The reference to the new Exception
     */
    METHOD PUBLIC STATIC Exception FromErrorMessageAndError (pcErrorString AS CHARACTER,
                                                             piMessageNumber AS INTEGER,
                                                             poError AS Error):

        DEFINE VARIABLE oException AS Exception NO-UNDO .
        DEFINE VARIABLE i          AS INTEGER   NO-UNDO .

        ASSIGN oException = NEW Exception (poError, pcErrorString, piMessageNumber) .

        DO i = 1 TO poError:NumMessages:
            oException:AddMessage (poError:GetMessage (i), poError:GetMessageNum (i)) .
        END.

        IF TYPE-OF (poError, AppError) THEN
            oException:ReturnValue = CAST (poError, AppError):ReturnValue .

        RETURN oException .

    END METHOD .

END CLASS.

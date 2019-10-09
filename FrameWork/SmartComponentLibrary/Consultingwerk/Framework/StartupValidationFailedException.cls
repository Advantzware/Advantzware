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
    File        : StartupValidationFailedException
    Purpose     : Exception that may be thrown by the IStartupValidationService
                  when a configuration issue is detected
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 28 08:20:47 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Consultingwerk.Framework.*  FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Framework.StartupValidationFailedException
    INHERITS Exception:

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException (poInnerException AS Progress.Lang.Error,
                                                         pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException (poInnerException AS Progress.Lang.Error,
                                                         pcErrorString AS CHARACTER,
                                                         piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException (pcErrorString AS CHARACTER,
                                                         piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     */
    CONSTRUCTOR PUBLIC StartupValidationFailedException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.

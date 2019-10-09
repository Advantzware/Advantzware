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
    File        : DoNotShowException
    Purpose     : Common base error class for errors that do not need
                  to be shown using the ErrorHelper class
    Syntax      :
    Description : Useful for errors where the actual error message might
                  already have been shown to the user and the error is just
                  thrown to cancel further processing
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 20 20:19:00 CET 2008
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Exceptions.DoNotShowException
    INHERITS Exception
    IMPLEMENTS ISupportsInnerException, IDoNotShowException
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC DoNotShowException ():

        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC DoNotShowException (pcErrorString AS CHARACTER,
                                           piMessageNumber AS INTEGER):

        SUPER (pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC DoNotShowException (pcErrorString AS CHARACTER):

        SUPER (pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC DoNotShowException (poInnerException AS Error):

        SUPER (poInnerException).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC DoNotShowException (poInnerException AS Error,
                                           pcErrorString AS CHARACTER,
                                           piMessageNumber AS INTEGER):

        THIS-OBJECT (poInnerException, pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the DoNotShowException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC DoNotShowException (poInnerException AS Error,
                                           pcErrorString AS CHARACTER):

        SUPER (poInnerException, pcErrorString).

    END CONSTRUCTOR.

END CLASS.

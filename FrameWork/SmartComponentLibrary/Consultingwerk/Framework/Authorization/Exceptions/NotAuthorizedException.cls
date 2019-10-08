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
    File        : NotAuthorizedException
    Purpose     : Common base error class for authorization exceptions
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 24 20:05:53 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

CLASS Consultingwerk.Framework.Authorization.Exceptions.NotAuthorizedException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException (poInnerException AS Progress.Lang.Error,
                                               pcErrorString AS CHARACTER ):

        SUPER (poInnerException, pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorMessage The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException (poInnerException AS Progress.Lang.Error,
                                               pcErrorMessage AS CHARACTER,
                                               piMessageNumber AS INTEGER ):

        SUPER (poInnerException, pcErrorMessage, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException (poInnerException AS Progress.Lang.Error):

        SUPER (poInnerException).

    END CONSTRUCTOR.


    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException (pcErrorString AS CHARACTER):

        SUPER (pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     * @param pcErrorMessage The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException (pcErrorMessage AS CHARACTER,
                                               piMessageNumber AS INTEGER ):

        SUPER (pcErrorMessage, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the NotAuthorizedException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC NotAuthorizedException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.

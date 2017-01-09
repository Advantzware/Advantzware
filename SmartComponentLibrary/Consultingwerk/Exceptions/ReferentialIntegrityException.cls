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
    File        : ReferentialIntegrityException
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 27 20:09:07 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.Exception.


USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.ReferentialIntegrityException
     INHERITS Exception
     {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException (poInnerException AS Progress.Lang.Error, pcErrorString AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException (poInnerException AS Progress.Lang.Error, pcErrorString AS CHARACTER, piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     * @param poInnerException The reference to the original error
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException (poInnerException AS Progress.Lang.Error):
        SUPER (poInnerException).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException (pcErrorString AS CHARACTER):
        SUPER (pcErrorString).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException (pcErrorString AS CHARACTER, piMessageNumber AS INTEGER):
        SUPER (pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ReferentialIntegrityException class
     * Notes:
     */
    CONSTRUCTOR PUBLIC ReferentialIntegrityException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.

/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : JsonSerializationException
    Purpose     : Exception thrown by Json-Serialization and Deserialization
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 09 17:13:10 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.JsonSerializationException
    INHERITS Exception
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC JsonSerializationException (poInnerException AS Progress.Lang.Error,
                                                   pcErrorString AS CHARACTER,
                                                   piMessageNumber AS INTEGER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     * @param pcErrorString The error message associated with this Exception object
     * @param piMessageNumber The error message number associated with this Exception object
     */
    CONSTRUCTOR PUBLIC JsonSerializationException (pcErrorString AS CHARACTER,
                                                   piMessageNumber AS INTEGER):
        SUPER (INPUT pcErrorString, INPUT piMessageNumber).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the Exception class
     * Notes:
     */
    CONSTRUCTOR PUBLIC JsonSerializationException ():
        SUPER ().

    END CONSTRUCTOR.

END CLASS.

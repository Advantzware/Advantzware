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
    File        : ParameterNullValueException
    Purpose     : An error object thrown to signal invalid parameter null values
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 22 15:30:52 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING Consultingwerk.Exceptions.InvalidParameterValueException.

{Consultingwerk/products.i}

CLASS Consultingwerk.Exceptions.ParameterNullValueException
    INHERITS InvalidParameterValueException
    {&SERIALIZABLE}:

    /**
     * Purpose: Constructor of the ParameterNullValueException class
     * Notes:
     * @param poInnerException The reference to the original error
     * @param pcParameterName The name of the invalid parameter
     * @param pcClassName The name of the class that caused the InvalidParameterException
     */
    CONSTRUCTOR PUBLIC ParameterNullValueException (poInnerException AS Progress.Lang.ProError,
                                                    pcParameterName AS CHARACTER,
                                                    pcClassName AS CHARACTER ):
        SUPER (poInnerException,
               pcParameterName,
               ?,
               pcClassName).

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor of the ParameterNullValueException class
     * Notes:
     * @param pcParameterName The name of the invalid parameter
     * @param pcClassName The name of the class that caused the InvalidParameterException
     */
    CONSTRUCTOR PUBLIC ParameterNullValueException (pcParameterName AS CHARACTER,
                                                    pcClassName AS CHARACTER ):
        SUPER (pcParameterName,
               ?,
               pcClassName).

    END CONSTRUCTOR.

END CLASS.

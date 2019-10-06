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
    File        : InvalidObjectTypeException
    Purpose     : Error thrown, when an invalid object type is passed to
                  the Repository APIs
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 26 07:47:12 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.* FROM PROPATH .
USING Progress.Lang.*                            FROM PROPATH .

&SCOPED-DEFINE Title "Invalid object type"{&TRAN}

CLASS Consultingwerk.SmartFramework.Repository.InvalidObjectTypeException
    INHERITS RepositoryException
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the ObjectType associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectType AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectTypeException class
        Notes:
        @param pcObjectType The object type value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectTypeException (pcObjectType AS CHARACTER):

        SUPER (SUBSTITUTE ("Invalid object type ~"&1~"", pcObjectType), 0) .

        ASSIGN THIS-OBJECT:ObjectType = pcObjectType
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectTypeException class
        Notes:
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
        @param pcObjectType The object type value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectTypeException (pcErrorString AS CHARACTER,
                                                   piMessageNumber AS INTEGER,
                                                   pcObjectType AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ObjectType = pcObjectType
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectTypeException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcObjectType The object type value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectTypeException (poInnerException AS Error,
                                                   pcObjectType AS CHARACTER):

        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:ObjectType = pcObjectType
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectTypeException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcObjectType The object type value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectTypeException (poInnerException AS Error,
                                                   pcErrorString AS CHARACTER,
                                                   piMessageNumber AS INTEGER,
                                                   pcObjectType AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ObjectType = pcObjectType
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

END CLASS.

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
    File        : InvalidObjectMasterException
    Purpose     : Error thrown, when an invalid object master is passed to
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

&SCOPED-DEFINE Title "Invalid object master"{&TRAN}

CLASS Consultingwerk.SmartFramework.Repository.InvalidObjectMasterException
    INHERITS RepositoryException
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the ObjectMaster associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectMaster AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectMasterException class
        Notes:
        @param pcObjectMaster The object master value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectMasterException (pcObjectMaster AS CHARACTER):

        SUPER (SUBSTITUTE ("Invalid object master ~"&1~"", pcObjectMaster), 0) .

        ASSIGN THIS-OBJECT:ObjectMaster = pcObjectMaster
               THIS-OBJECT:Title        = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectMasterException class
        Notes:
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
        @param pcObjectMaster The object master value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectMasterException (pcErrorString AS CHARACTER,
                                                     piMessageNumber AS INTEGER,
                                                     pcObjectMaster AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ObjectMaster = pcObjectMaster
               THIS-OBJECT:Title        = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectMasterException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcObjectMaster The object master value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectMasterException (poInnerException AS Error,
                                                     pcObjectMaster AS CHARACTER):

        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:ObjectMaster = pcObjectMaster
               THIS-OBJECT:Title        = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidObjectMasterException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcObjectMaster The object master value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidObjectMasterException (poInnerException AS Error,
                                                     pcErrorString AS CHARACTER,
                                                     piMessageNumber AS INTEGER,
                                                     pcObjectMaster AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ObjectMaster = pcObjectMaster
               THIS-OBJECT:Title        = {&Title} .

    END CONSTRUCTOR.

END CLASS.

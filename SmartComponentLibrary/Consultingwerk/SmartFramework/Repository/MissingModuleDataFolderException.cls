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

&SCOPED-DEFINE Title "Invalid product module configuration"{&TRAN}

CLASS Consultingwerk.SmartFramework.Repository.MissingModuleDataFolderException
    INHERITS RepositoryException
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the ModuleName associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ModuleName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MissingModuleDataFolderException class
        Notes:
        @param pcModuleName The product module name value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MissingModuleDataFolderException (pcModuleName AS CHARACTER):

        SUPER (SUBSTITUTE ("Module Data Folder not set for Module ~"&1~"", pcModuleName), 0) .

        ASSIGN THIS-OBJECT:ModuleName = pcModuleName
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MissingModuleDataFolderException class
        Notes:
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
        @param pcModuleName The product module name value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MissingModuleDataFolderException (pcErrorString AS CHARACTER,
                                                         piMessageNumber AS INTEGER,
                                                         pcModuleName AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ModuleName = pcModuleName
               THIS-OBJECT:Title      = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MissingModuleDataFolderException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcModuleName The product module name value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MissingModuleDataFolderException (poInnerException AS Error,
                                                         pcModuleName AS CHARACTER):

        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:ModuleName = pcModuleName
               THIS-OBJECT:Title       = {&Title} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MissingModuleDataFolderException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcModuleName The product module name value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MissingModuleDataFolderException (poInnerException AS Error,
                                                         pcErrorString AS CHARACTER,
                                                         piMessageNumber AS INTEGER,
                                                         pcModuleName AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ModuleName = pcModuleName
               THIS-OBJECT:Title       = {&Title} .

    END CONSTRUCTOR.

END CLASS.

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
    File        : InvalidRepositoryDataTypeException
    Purpose     : Error thrown, when an invalid data type is stored in the
                  Repository
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

&SCOPED-DEFINE Title "Invalid data type"{&TRAN}

CLASS Consultingwerk.SmartFramework.Repository.InvalidRepositoryDataTypeException
    INHERITS RepositoryException
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the AttributeLabel associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AttributeLabel AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the DataType associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DataType AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ObjectType associated with the exception
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ObjectType AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidRepositoryDataTypeException class
        Notes:
        @param pcObjectType The object type value
        @param pcAttributeLabel The attribute label value
        @param pcDataType The data type value
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidRepositoryDataTypeException (pcObjectType AS CHARACTER,
                                                           pcAttributeLabel AS CHARACTER,
                                                           pcDataType AS CHARACTER):

        SUPER (SUBSTITUTE ("Invalid data type ~"&3~" for attribute ~"&2~" object type ~"&1~"",
                           pcObjectType,
                           pcAttributeLabel,
                           pcDataType), 0) .

        ASSIGN THIS-OBJECT:ObjectType     = pcObjectType
               THIS-OBJECT:AttributeLabel = pcAttributeLabel
               THIS-OBJECT:DataType       = pcDataType
               THIS-OBJECT:Title          = {&Title} .

    END CONSTRUCTOR.

END CLASS.

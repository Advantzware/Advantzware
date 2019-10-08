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
    File        : InvalidTablesParameterValueException
    Purpose     : Exception thrown by the DataAccess:FetchDataInternal method
                  when the FetchDataRequest parameter contains an invalid
                  table name
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 16 13:39:53 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.Exceptions.InvalidTablesParameterValueException
    INHERITS InvalidParameterValueException:

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the invalid table
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY InvalidTable AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of tables
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TablesList AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidTablesParameterValueException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcParameterName The name of the invalid parameter
        @param pcValue The invalid parameter name
        @param pcTablesList The list of tables
        @param pcInvalidTable The name of the invalid table
        @param pcClassName The name of the class that caused the InvalidParameterException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidTablesParameterValueException (poInnerException AS Progress.Lang.Error,
                                                             pcParameterName AS CHARACTER,
                                                             pcValue AS CHARACTER,
                                                             pcTablesList AS CHARACTER,
                                                             pcInvalidTable AS CHARACTER,
                                                             pcClassName AS CHARACTER):

        SUPER (poInnerException, pcParameterName, pcValue, pcClassName).

        ASSIGN THIS-OBJECT:TablesList   = pcTablesList
               THIS-OBJECT:InvalidTable = pcInvalidTable .

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Invalid table name: &1"{&TRAN}, pcInvalidTable), 0) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InvalidParameterException class
        Notes:
        @param pcParameterName The name of the invalid parameter
        @param pcValue The invalid parameter name
        @param pcTablesList The list of tables
        @param pcInvalidTable The name of the invalid table
        @param pcClassName The name of the class that caused the InvalidParameterException
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InvalidTablesParameterValueException (pcParameterName AS CHARACTER,
                                                             pcValue AS CHARACTER,
                                                             pcTablesList AS CHARACTER,
                                                             pcInvalidTable AS CHARACTER,
                                                             pcClassName AS CHARACTER):
        SUPER (pcParameterName, pcValue, pcClassName).

        ASSIGN THIS-OBJECT:TablesList   = pcTablesList
               THIS-OBJECT:InvalidTable = pcInvalidTable .

        THIS-OBJECT:AddMessage (SUBSTITUTE ("Invalid table name: &1"{&TRAN}, pcInvalidTable), 0) .

    END CONSTRUCTOR.

END CLASS.

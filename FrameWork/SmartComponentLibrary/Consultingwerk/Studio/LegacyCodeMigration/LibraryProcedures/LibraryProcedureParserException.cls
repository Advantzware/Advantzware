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
    File        : LibraryProcedureParserException
    Purpose     : Error class for assertions
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 25 14:48:55 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Studio.LegacyCodeMigration.LibraryProcedures.* FROM PROPATH .
USING Consultingwerk.Exceptions.*                                   FROM PROPATH .
USING Progress.Lang.*                                               FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.LegacyCodeMigration.LibraryProcedures.LibraryProcedureParserException
    INHERITS Exception
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the library procedure that caused the issue
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LibraryProcedureName AS CHARACTER NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (poInnerException AS Progress.Lang.Error,
                                                        pcErrorString AS CHARACTER,
                                                        pcProcedureName AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

        THIS-OBJECT:AddMessagesFrom(poInnerException) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (poInnerException AS Progress.Lang.Error,
                                                        pcErrorMessage AS CHARACTER,
                                                        piMessageNumber AS INTEGER,
                                                        pcProcedureName AS CHARACTER):
        SUPER (poInnerException, pcErrorMessage, piMessageNumber).

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

        THIS-OBJECT:AddMessagesFrom(poInnerException) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (poInnerException AS Progress.Lang.Error,
                                                        pcProcedureName AS CHARACTER):
        SUPER (poInnerException).

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

        THIS-OBJECT:AddMessagesFrom(poInnerException) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (pcErrorString AS CHARACTER,
                                                        pcProcedureName AS CHARACTER):
        SUPER (pcErrorString).

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (pcErrorMessage AS CHARACTER,
                                                        piMessageNumber AS INTEGER,
                                                        pcProcedureName AS CHARACTER):

        SUPER (pcErrorMessage, piMessageNumber).

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param pcProcedureName The name of the library procedure that caused this issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (pcProcedureName AS CHARACTER):
        SUPER ().

        THIS-OBJECT:LibraryProcedureName = pcProcedureName .

        THIS-OBJECT:AddMessage(SUBSTITUTE ("Error parsing: &1"{&TRAN}, pcProcedureName), 0).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (pcErrorMessage AS CHARACTER,
                                                        piMessageNumber AS INTEGER):

        SUPER (pcErrorMessage, piMessageNumber).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the LibraryProcedureParserException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorMessage The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParserException (poInnerException AS Progress.Lang.Error,
                                                        pcErrorMessage AS CHARACTER,
                                                        piMessageNumber AS INTEGER):

        SUPER (poInnerException, pcErrorMessage, piMessageNumber).

        THIS-OBJECT:AddMessagesFrom (poInnerException) .

    END CONSTRUCTOR.
END CLASS.

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
    File        : CatalogException
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 07 09:01:46 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.CatalogException
    INHERITS ServiceInterfaceException:

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException ():

        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param piMessageNumber The error message number associated with this ServiceInterfaceException object
        @param pcBusinessService The name of the business service that caused the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException (pcErrorString AS CHARACTER,
                                         piMessageNumber AS INTEGER,
                                         pcBusinessService AS CHARACTER):

        SUPER (pcErrorString, piMessageNumber).

        THIS-OBJECT:BusinessService = pcBusinessService .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param pcBusinessService The name of the business service that caused the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException (pcErrorString AS CHARACTER,
                                         pcBusinessService AS CHARACTER):

        SUPER (pcErrorString).

        THIS-OBJECT:BusinessService = pcBusinessService .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException (poInnerException AS Error):

        SUPER (poInnerException).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param piMessageNumber The error message number associated with this ServiceInterfaceException object
        @param pcBusinessService The name of the business service that caused the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException (poInnerException AS Error,
                                         pcErrorString AS CHARACTER,
                                         piMessageNumber AS INTEGER,
                                         pcBusinessService AS CHARACTER):

        SUPER (poInnerException, pcErrorString, piMessageNumber).

        THIS-OBJECT:BusinessService = pcBusinessService .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CatalogException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ServiceInterfaceException object
        @param pcBusinessService The name of the business service that caused the issue
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CatalogException (poInnerException AS Error,
                                         pcErrorString AS CHARACTER,
                                         pcBusinessService AS CHARACTER):

        SUPER (poInnerException,
               pcErrorString).

        THIS-OBJECT:BusinessService = pcBusinessService .

    END CONSTRUCTOR.

END CLASS.

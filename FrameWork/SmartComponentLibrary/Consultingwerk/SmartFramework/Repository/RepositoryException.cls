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
    File        : RepositoryException
    Purpose     : Common base error class for Exceptions thrown by the
                  Repository API
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

&SCOPED-DEFINE Title "Error retrieving Repository Data"{&TRAN}

CLASS Consultingwerk.SmartFramework.Repository.RepositoryException
    INHERITS ExceptionWithTitle
    {&SERIALIZABLE}:

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException ():

        SUPER ({&Title}).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this ExceptionWithTitle object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException (pcErrorString AS CHARACTER,
                                            piMessageNumber AS INTEGER):

        SUPER (pcErrorString, piMessageNumber, {&Title}).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
        @param pcErrorString The error message associated with this ExceptionWithTitle object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException (pcErrorString AS CHARACTER):

        SUPER (pcErrorString, {&Title}).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
        @param poInnerException The reference to the original error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException (poInnerException AS Error):

        SUPER (poInnerException, {&Title}).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this ExceptionWithTitle object
        @param piMessageNumber The error message number associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException (poInnerException AS Error,
                                            pcErrorString AS CHARACTER,
                                            piMessageNumber AS INTEGER):

        SUPER (poInnerException, pcErrorString, piMessageNumber, {&Title}).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RepositoryException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositoryException (poInnerException AS Error,
                                            pcErrorString AS CHARACTER):

        SUPER (poInnerException, pcErrorString, {&Title}).

    END CONSTRUCTOR.

END CLASS.

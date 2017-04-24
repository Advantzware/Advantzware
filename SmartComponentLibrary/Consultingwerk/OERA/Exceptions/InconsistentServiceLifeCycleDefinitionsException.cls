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
    File        : InconsistentServiceLifeCycleDefinitionsException
    Purpose     : Exception thrown when the service life cycle definitions
                  are not consistent.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 28 22:04:33 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*.
USING Progress.Lang.*.

CLASS Consultingwerk.OERA.Exceptions.InconsistentServiceLifeCycleDefinitionsException
    INHERITS Exception:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Service Name Pattern associated with the error
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ServiceNamePattern AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (poInnerException AS Progress.Lang.Error,
                                                                         pcErrorString AS CHARACTER,
                                                                         pcServiceNamePattern AS CHARACTER):
        SUPER (poInnerException, pcErrorString).

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (poInnerException AS Progress.Lang.Error,
                                                                         pcErrorString AS CHARACTER,
                                                                         piMessageNumber AS INTEGER,
                                                                         pcServiceNamePattern AS CHARACTER):
        SUPER (poInnerException, pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param poInnerException The reference to the original error
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (poInnerException AS Progress.Lang.Error,
                                                                         pcServiceNamePattern AS CHARACTER):
        SUPER (poInnerException).

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (pcErrorString AS CHARACTER,
                                                                         pcServiceNamePattern AS CHARACTER ):
        SUPER (pcErrorString).

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (pcErrorString AS CHARACTER,
                                                                         piMessageNumber AS INTEGER,
                                                                         pcServiceNamePattern AS CHARACTER):
        SUPER (pcErrorString, piMessageNumber).

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the InconsistentServiceLifeCycleDefinitionsException class
        Notes:
        @param pcServiceNamePattern The Service Name Pattern associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC InconsistentServiceLifeCycleDefinitionsException (pcServiceNamePattern AS CHARACTER):
        SUPER ().

        ASSIGN THIS-OBJECT:ServiceNamePattern = pcServiceNamePattern .

    END CONSTRUCTOR.

END CLASS.

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
    File        : UnableToLaunchBusinessServiceException
    Purpose     : Exception thrown when the Service Manager is unable
                  to launch a Business Entity or Business Task
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Mar 30 00:00:08 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*      FROM PROPATH .
USING Consultingwerk.OERA.Exceptions.* FROM PROPATH .
USING Progress.Lang.*                  FROM PROPATH .

CLASS Consultingwerk.OERA.Exceptions.UnableToLaunchBusinessServiceException
    INHERITS Exception:

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the Business Service that cannot be started
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BusinessServiceName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UnableToLaunchBusinessServiceException class
        Notes:
        @param poError The original error instance
        @param pcServiceName The name of the Business Service that is not running
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnableToLaunchBusinessServiceException (poError AS Progress.Lang.Error,
                                                               pcServiceName AS CHARACTER):

        SUPER (poError).

        THIS-OBJECT:BusinessServiceName = pcServiceName .

        IF VALID-OBJECT (poError) THEN
            THIS-OBJECT:AddMessagesFrom(poError) .
        ELSE
            THIS-OBJECT:AddMessage(SUBSTITUTE ("Unable to launch Business Service &1."{&TRAN}, pcServiceName), 0) .

    END CONSTRUCTOR.

END CLASS.

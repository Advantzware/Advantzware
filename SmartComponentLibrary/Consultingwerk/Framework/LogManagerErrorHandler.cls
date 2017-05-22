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
    File        : LogManagerErrorHandler
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 14 13:14:16 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Framework.IErrorHandler    FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

CLASS Consultingwerk.Framework.LogManagerErrorHandler
    IMPLEMENTS IErrorHandler:

    /*------------------------------------------------------------------------------
        Purpose: Handles an Error object by writing it to the client logfile
        Notes:   Logical return value indicates if the error has been completely
                 Handled, that is when this method returns TRUE, the ErrorHelper
                 will not display the error anymore, return false when the
                 ErrorHelper should still display the error message dialog.
                 This method will not mark the error as handled. Thus it will always
                 return false.
        @param poError The reference to the Error object
        @return Logical value indicating if the error has been handled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL HandleError (poError AS Progress.Lang.Error):

        LogManager:WriteError (poError) .

        RETURN FALSE .

    END METHOD.

END CLASS.
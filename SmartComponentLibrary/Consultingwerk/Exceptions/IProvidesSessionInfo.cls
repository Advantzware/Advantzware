/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IProvidesSessionInfo
    Purpose     : Interface for error classes that provide the SessionInfo
                  property
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 13 21:27:16 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.*            FROM PROPATH .
USING Consultingwerk.Exceptions.* FROM PROPATH .
USING Progress.Lang.*             FROM PROPATH .

INTERFACE Consultingwerk.Exceptions.IProvidesSessionInfo:

    /**
     * Purpose: Returns the Information about the Session the error occurred in
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SessionInfo AS SessionInfo NO-UNDO
    GET.

END INTERFACE.

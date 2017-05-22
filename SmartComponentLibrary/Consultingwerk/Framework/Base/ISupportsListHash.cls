/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ISupportsListHash
    Purpose     : Interface for Enumeratable classes that provide a hash
                  value representing the current list contents as an alternative
                  to the ISupportsListChanged event which depends on event
                  subscriptions and thus may cause circular object
                  references
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Mar 01 18:18:26 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.Framework.Base.ISupportsListHash:

    /**
     * Purpose: Returns a hash value representing the current list contents
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ListHash AS CHARACTER NO-UNDO
    GET.

END INTERFACE.

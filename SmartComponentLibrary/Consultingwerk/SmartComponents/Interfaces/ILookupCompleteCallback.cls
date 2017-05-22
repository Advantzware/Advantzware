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
    File        : ILookupCompleteCallback
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 09 15:22:48 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ILookupCompleteCallback:

    /**
     * Purpose: Callback method for the LookupComplete event of the SmartLookup
     * Notes:
     * @param sender The reference to an SmartLookup instance which has invoked the method
     */
    METHOD PUBLIC VOID LookupCompleteCallback (sender AS SmartLookup) .

END INTERFACE.

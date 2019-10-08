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
    File        : ISmartGroupCreateTarget
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 02 11:05:45 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartGroupCreateTarget:

    /**
     * Purpose: Gets and sets the SmartGroupAssignSource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY SmartGroupCreateSource AS ISmartGroupCreateSource NO-UNDO
    GET .
    SET .

    /**
     * Purpose: Cancels the creation of a new record in the ISmartGroupCreateTarget
     * Notes:   Typically invoked when the ISmartGroupCreateSource also cancels the
     *          creation of a new record
     * @param poSource The ISmartgroupCreateSource that caused cancelling the creation of a new row
     */
    METHOD PUBLIC VOID CancelCreateByGroupCreateSource (poSource AS ISmartGroupCreateSource).

END INTERFACE.
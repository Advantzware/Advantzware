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
    File        : ISupportsSubmitChanges
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 28 18:14:54 CET 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISupportsSubmitChanges:

    /**
     * Purpose: Assigns the internal flag that data is being saved
     * Notes:   Enforced by ISupportsSubmitChanges
     * @param plSaving The new value for the flag that data is being saved
     */
    METHOD PUBLIC VOID SetSavingData (plSaving AS LOGICAL) .

    /**
     * Purpose: Creates a dataset containing the modifications of the
     *          dataset and submits the changes dataset to the service adapter
     *          (and then to the Business Entity).
     * Notes:   Used from UpdateRow after every modification from the UI.
     */
    METHOD PUBLIC VOID SubmitChanges () .

END INTERFACE.
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
    File        : IEmbeddedSmartEditor
    Purpose     : Mediates the communication between the SmartUpdatableBrowser
                  and the SmartLookup / SmartBusinessEntityLookup
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 24 22:37:55 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY.
USING Progress.Lang.*                 FROM PROPATH .
USING System.ComponentModel.*         FROM ASSEMBLY.
USING System.Windows.Forms.*          FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IEmbeddedSmartEditor:

    /**
     * Purpose: Gets and sets the reference to the grid that owns this SmartLookup
     *          as an Embedded Editor Control for a cell
     * Notes:
     */
    DEFINE PUBLIC PROPERTY OwningGrid AS UltraGrid NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Performs the KeyDown event for an embedded SmartLookup
     * Notes:   Only supported when the lookup is used as an embedded editor
     * @param e The KeyEventArgs of the original UltraGrid event
     */
    METHOD PUBLIC VOID PerformKeyDown (e AS KeyEventArgs) .

    /**
     * Purpose: Performs the KeyPress event for an embedded SmartLookup
     * Notes:   Only supported when the lookup is used as an embedded editor
     * @param e The KeyPressEventArgs of the original UltraGrid event
     */
    METHOD PUBLIC VOID PerformKeyPress (e AS KeyPressEventArgs) .

    /**
     * Purpose: Performs the Leave (BeforeCellDeactivate) event for an embedded SmartLookup
     * Notes:   Only supported when the lookup is used as an embedded editor
     * @param e The CancelEventArgs of the original UltraGrid event
     */
    METHOD PUBLIC VOID PerformLeave (e AS CancelEventArgs) .


END INTERFACE.
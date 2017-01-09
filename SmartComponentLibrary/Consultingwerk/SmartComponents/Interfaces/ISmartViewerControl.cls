/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ISmartViewerControl
    Purpose     : Interface describing the methods of the SmartViewerControl
                  referenced by the SmartDataBrowser class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 15 20:22:45 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartViewerControl:

    /**
     * Purpose: Get and Set the current SmartTableIOSource of the SmartViewerControl.
     *          This Property is set by the developer when initializing a Control
     *          implementing the Interface ISmartDataTarget.
     *          Enforced by Interface ISmartDataTarget.
     * Notes:   Cannot be changed while the SmartViewerControl is in ModifyingData mode.
     */
    DEFINE PUBLIC PROPERTY SmartTableIOSource AS ISmartTableIOSource NO-UNDO
    GET .

    /**
     * Purpose: The SmartTableIOState property reflects the operationmode of the
     *          SmartViewerControl.
     *          Value changes of this Property are only made in the Class
     *          SmartViewerControl.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * Notes:   Possible values (TableIOStateEnum):
     *          - AddCopyAllowed
     *          - FieldsDisabled
     *          - FieldsEnabled
     *          - ModifyingData
     *          - NoDataSource
     */
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO
    GET .

    /**
     * Purpose: Enables controls of the SmartViewerControl
     * Notes:   This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * @param pcFields The EnableFieldsEnum value indicating if the fields are enabled for Add or Update
     */
    METHOD PUBLIC VOID EnableFields (INPUT pcFields AS CHARACTER) .

END INTERFACE.
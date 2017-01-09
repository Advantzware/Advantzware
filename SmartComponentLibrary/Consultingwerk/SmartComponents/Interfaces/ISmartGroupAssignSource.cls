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
    File        : ISmartGroupAssignSource
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Mar 04 16:57:17 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignSource:

    /**
     * Purpose: Event fired whenever the SmartTableIOState property has changed
     * Notes:
     * @param sender The object that raised the event
     * @param e The SmartTableIOStateChangedEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT SmartTableIOStateChanged SIGNATURE VOID (sender AS System.Object,
                                                                 e AS SmartTableIOStateChangedEventArgs).
    /**
     * Purpose: Deregister a SmartDataTarget with the SmartGroupAssignTarget
     * Notes:   Enforced by Interface ISmartGroupAssignSource.
     * @param poSmartGroupAssignTarget The ISmartGroupAssignTarget to remove from this instance
     */
    METHOD PUBLIC VOID DeregisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget).

    /**
     * Purpose: Register a SmartDataTarget with the SmartGroupAssignTarget
     * Notes:   This Method is executed every time a new SmartGroupAssignSource is
     *          registered to the corresponding Property of a Class implementing the
     *          Interface ISmartGroupAssignSource.
     *          Enforced by Interface ISmartGroupAssignSource.
     * @param poSmartGroupAssignTarget The ISmartGroupAssignTarget to register
     */
    METHOD PUBLIC VOID RegisterSmartGroupAssignTarget (poSmartGroupAssignTarget AS ISmartGroupAssignTarget).

    /**
     * Purpose: Set's the current opened EditorDropDownControl
     * Notes:   See Bug 1878 for details
     * @param poControl The reference to the currently opened EditorDropDownControl
     */
    METHOD PUBLIC VOID SetOpenedEditorDropDownControl (poControl AS System.Windows.Forms.Control).

    /**
     * Purpose: Initiate switching to the ModifyinData Mode if the user enters data.
     * Notes:   This Method is executed every time data changes in the EditorControls
     *          of a SmartViewerControl.
     *          Event handler for the TextChangedEvent event of Editor Controls on
     *          the SmartViewerControl.
     *          Enforced by Interface ISmartGroupAssignSource.
     * @param sender The reference to the object that raised this event
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PUBLIC VOID TextChangedEventHandler (sender AS System.Object,
                                                e AS System.EventArgs).

END INTERFACE.

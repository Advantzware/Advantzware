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
    File        : INavigationSource
    Purpose     :
    Syntax      :
    Description : An Interface declaring the members of a nativation source
                  (toolbar) component.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 17 14:36:29 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartNavigationSource:

    /**
     * Purpose: Registers a SmartNavigationTarget with the SmartNavigationSource.
     * Notes:   This Method is invoked by the SmartDataAdapter in the setter of
     *          the Property SmartNavigationSource.
     *          Enforced by the Interface ISmartNavigationSource.
     * @param poSmartNavigationTarget The ISmartNavigationTarget to register
     */
    METHOD PUBLIC VOID RegisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget) .

    /**
     * Purpose: Deregisters a SmartNavigationTarget from the SmartNavigationSource.
     * Notes:
     * @param poSmartNavigationTarget The ISmartNavigationTarget to deregister
     */
    METHOD PUBLIC VOID DeregisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget) .

    /**
     * Purpose: Event handler for the SmartNavigationTargetPositionChanged event of the SmartNavigationTarget
     * Notes:
     * @param sender The reference to the object that raised the evnet
     * @param e The SmartNavigationTargetPositionChangedEventArgs instance with the data for this event
     */
    METHOD PUBLIC VOID UpdateSmartNavigationTargetPosition (sender AS System.Object,
                                                            e AS SmartNavigationTargetPositionChangedEventArgs) .

    /**
     * Purpose: Event handler for the SmartNavigationTargetStateChanged event
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The SmartNavigationTargetStateChangedEventArgs instance with the data for this event
     */
    METHOD PUBLIC VOID UpdateSmartNavigationTargetState (sender AS System.Object,
                                                         e AS SmartNavigationTargetStateChangedEventArgs) .

    /**
     * Purpose: Activate the given SmartNavigationTarget if it is registered with
     *          the SmartNavigationSource.
     * Notes:   This Method has to be called if one Toolbar handels two or more
     *          SmartDataSources at a time.
     *          Enforced by Interface ISmartNavigationSource.
     * @param poSmartNavigationTarget The ISmartNavigationTarget instance to activate
     */
    METHOD PUBLIC VOID ActivateSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget).

END INTERFACE.

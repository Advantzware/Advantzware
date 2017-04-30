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
    File        : ISmartFilterTarget
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 24 15:44:50 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* .
USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartFilterTarget:

    /**
     * Purpose: Event raised to collect Filter Values from Filter Sources
     * Notes:   Required by IFilterTarget
     * @param sender The reference to the object that raised the event
     * @param e The CollectFilterValuesEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT CollectFilterValues SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                            e AS CollectFilterValuesEventArgs).

    /**
     * Purpose: Reads Data from a Datasource and links the DataHandle
     * Notes:
     * @return Logical value indicating the success of the method
     */
    METHOD PUBLIC LOGICAL RetrieveData ().

END INTERFACE.

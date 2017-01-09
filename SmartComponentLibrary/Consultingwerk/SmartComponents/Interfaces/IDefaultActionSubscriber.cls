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
    File        : IDefaultActionSubscriber
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Mar 17 10:57:34 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IDefaultActionSubscriber:

    /**
     * Purpose: Method to be executed inside the class which implements the Interface
     *          IDefaultActionSubscriber.
     * Notes:   Invoked by the SmartDataBrowser Instance if the DefaultActionHandler
     *          Method is executed.
     * @param sender The reference to the Instance of the SmartDataBrowser which has invoked the Method
     * @param e The System.EventArgs with the data for this event
     */
    METHOD PUBLIC VOID SmartDataBrowserDefaultActionHandler (sender AS SmartDataBrowser,
                                                             e AS System.EventArgs).

END INTERFACE.
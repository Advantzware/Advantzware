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
    File        : ISmartViewerLogic
    Purpose     : Interface defining the requirements for classes providing
                  client side logic for SmartViewerControl instances 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 01 02:27:55 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .  
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartViewerLogic:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the SmartViewerControl that owns this
                 instance of the Viewer Logic Object 
        Notes:   The Owner property cannot be modified once set   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Owner AS SmartViewerControl NO-UNDO 
	GET.
	SET.
	 
    /*------------------------------------------------------------------------------
        Purpose: Initializes the SmartViewerLogic instance
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Initialize () .
    	 
    /*------------------------------------------------------------------------------
        Purpose: Handshake method for SmartViewerControl instances 
        Notes:   
        @param poSmartViewerControl The SmartViewerControl instance this logic class will provid logic for
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID RegisterSmartViewerControl (poSmartViewerControl AS SmartViewerControl).

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the SmartViewerLogic instance
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown () .

END INTERFACE.

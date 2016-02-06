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
    File        : SmartDataBrowserSettings
    Purpose     : Helper class for the SmartDataBrowser class to avoid 
                  the usage of static in the hybrid type
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Sep 03 22:09:33 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataBrowserSettings: 

    /*------------------------------------------------------------------------------
        Purpose: Static event raised whenever a SmartDataBrowser is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.                                                                 
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC EVENT SmartDataBrowserActivated DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Controls if SmartDataBrowsers should store the column settings 
                 when the containing Form is closed.                                                                 
        Notes:   
    ------------------------------------------------------------------------------*/       
    DEFINE PUBLIC STATIC PROPERTY SaveColumnSettings AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the SmartDataBrowserSettings class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE SmartDataBrowserSettings ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Raises the SmartDataBrowserActivated event                                                                        
        Notes:   Static event raised whenever a SmartDataBrowser is activated. This
                 event is useful to control a central UltraGridColumnChooser etc.   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID OnSmartDataBrowserActivated (sender AS System.Object,
                                                           e AS System.EventArgs):
        
        IF NOT VALID-OBJECT (e) THEN 
            e = System.EventArgs:Empty .
            
        SmartDataBrowserSettings:SmartDataBrowserActivated:Publish (sender, e) .

    END METHOD.


END CLASS.

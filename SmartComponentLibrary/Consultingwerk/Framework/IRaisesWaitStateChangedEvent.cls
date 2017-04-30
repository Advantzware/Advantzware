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
    File        : IRaisesWaitStateChangedEvent
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jun 17 10:52:24 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.Framework.IRaisesWaitStateChangedEvent:  
  
    /*------------------------------------------------------------------------------
        Purpose: Raised whenever the WaitState changes                                                                       
        Notes:      
        @param sender The reference to the object that raised the event
        @param e The WaitStateChangedEventArgs with the data for this event                                                                   
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC EVENT WaitStateChanged SIGNATURE VOID (sender AS Progress.Lang.Object,    
                                                         e AS WaitStateChangedEventArgs).
  
END INTERFACE.

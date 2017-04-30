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
    File        : IServiceAdapterWithContextEvents
    Purpose     : Declares events that may be used to collect the Context
                  for the AppServer
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 12:44:36 CET 2013
    Notes       : This interface is no longer required to use. As of SCL-1045 
                  the basic IServiceAdapter interface was enhanced to contain 
                  all ServiceAdapter functionality. As such, all methods of 
                  the ServiceAdapter class are directly accessible from the 
                  FrameworkSettings:ServiceAdapter reference withour the 
                  previous need to case to this interface.
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.OERA.IServiceAdapterWithContextEvents:  
  
    /*------------------------------------------------------------------------------
        Purpose: Raised after the ServiceAdapter has performed an Request to the AppServer
        Notes:   
        @param sender The object that raised the CollectContextFromServer event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT CollectContextFromServer SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                 e AS Consultingwerk.EventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Raised before the ServiceAdapter performs an Request to the AppServer
        Notes:   
        @param sender The object that raised the PrepareContextForRequest event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT PrepareContextForServer SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                e AS Consultingwerk.EventArgs).
                                                        
END INTERFACE.

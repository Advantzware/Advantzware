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
    File        : ITableIOSource
    Purpose     : 
    Syntax      : 
    Description : An Interface declaring the members of a TableIO source 
                  (toolbar) component. 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 19 09:17:55 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartTableIOSource:  
  
     /*------------------------------------------------------------------------------
        Purpose: Registers a SmartTableIOTarget with the SmartTableIOSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartTableIOSource.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget) . 

     /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartTableIOTarget with the SmartTableIOSource.             
        Notes:   
        @param poSmartTableIOTarget The ISmartTableIOTarget to deregister 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget) . 
                                                
     /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartTableIOTarget if it is registered as a 
                 SmartTableIOSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget).    
  
END INTERFACE.

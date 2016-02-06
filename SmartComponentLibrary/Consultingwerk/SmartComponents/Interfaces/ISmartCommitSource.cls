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
    File        : ISmartCommitSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 01 23:02:02 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* .
USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartCommitSource:  

    /*------------------------------------------------------------------------------
        Purpose: Deregisters a new SmartCommitTarget                                                                        
        Notes:                                    
        @param poTarget The ISmartCommitTraget to add to this SmartToolbarController                                                              
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartCommitTarget (poTarget AS ISmartCommitTarget) .

    /*------------------------------------------------------------------------------
        Purpose: Registers a new SmartCommitTarget                                                                        
        Notes:                                                                        
        @param poTarget The ISmartCommitTarget to register                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartCommitTarget (poTarget AS ISmartCommitTarget) .
  
END INTERFACE.
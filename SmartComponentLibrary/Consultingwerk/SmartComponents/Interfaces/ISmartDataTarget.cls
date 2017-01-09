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
    File        : IDataTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Feb 17 09:04:10 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartDataTarget:  
  
    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource 
                 can be set to any class implementing the Interface ISmartDataSource.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataTarget.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: This Method attaches a new BindingSource given as the Parameter 
                 poBindingSource to the Objects of the current Class. The new 
                 BindingSource may be created by a DataAdapter or borrowed from a 
                 Browser.
        Notes:   It is called by the SmartDataAdapter Class when creating a new 
                 BindingSource or when a new SmartDataSource is registered to the
                 Class implementing the Interface ISmartDataTarget.   
        @param poBindingSource The BindingSource to be attached to the Grids DataSource Property
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AttachBindingSource (poBindingSource AS Progress.Data.BindingSource).
    
    /*------------------------------------------------------------------------------
        Purpose: This Method sets the Property SmartTableIOState to a value according 
                 to the SmartDataSources state of the current class. Possible values 
                 for SmartTableIOState are defined in the TableIOStateEnum.
                 This Method is called by the SmartDataAdapter when the 
                 PositionChangedHandler is fired or inside the SmartViewerControl if
                 a new BindingSource is attached.
        Notes:   Required only for visual DataTagets (Viewer)
    ------------------------------------------------------------------------------*/    
    METHOD PUBLIC VOID EvaluateTableIOState () .
    
END INTERFACE.
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
    File        : ISmartDataTargetWithBindingSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Mon Mar 23 08:39:38 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartDataTargetWithBindingSource:  

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the BindingSource of the current Class. The BindingSource 
                 can be set to any class derived from Progress.Data.BindingSource.
                 This Property is set internaly if the SmartDataAdapter is attached
                 to a Browser where a DesignTime BindingSource is attached or if
                 the developer calls RetrieveData (). 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BindingSource AS Progress.Data.BindingSource NO-UNDO 
    GET.

END INTERFACE.
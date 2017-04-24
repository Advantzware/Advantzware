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
    File        : IDatasetControllerConsumer
    Purpose     : Interface for objects that expose a DatasetController
                  using a property
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Apr 20 21:28:13 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Implementation.IDatasetControllerConsumer:  
  
    /*------------------------------------------------------------------------------
        Purpose: Raised when the DatasetController property is changed                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT DatasetControllerChanged DELEGATE System.EventHandler.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the DatasetController                                                                     
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetController AS Consultingwerk.SmartComponents.Interfaces.IDatasetController NO-UNDO 
    GET .  

END INTERFACE.
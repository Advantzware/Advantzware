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
    File        : IAfterRetrieveDataSubscriber
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 23 16:12:57 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.IAfterRetrieveDataSubscriber:  
  
    /*------------------------------------------------------------------------------
        Purpose: Callback method for the AfterRetrieveData event of the ISmartDataSource
        Notes:   
        @param sender The reference to an ISmartDataSource instance which has invoked the method
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AfterRetrieveDataCallback (sender AS ISmartDataSource) . 
    
END INTERFACE.
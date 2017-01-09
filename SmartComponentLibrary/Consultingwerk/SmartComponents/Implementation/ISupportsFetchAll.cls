/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ISupportsFetchAll
    Purpose     : Interface for SmartDataAdapter's that support a FetchAll 
                  method to retrieve the remaining records from the backend
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 15 22:19:19 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Implementation.ISupportsFetchAll:  
  
    /*------------------------------------------------------------------------------
        Purpose: Fetches all remaining data from the backend 
        Notes:   
        @return Logical value indicating if more rows have been fetched
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL FetchAll ().

END INTERFACE.
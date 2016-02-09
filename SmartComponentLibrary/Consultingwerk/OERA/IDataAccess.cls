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
    File        : IDataAccess
    Purpose     : Interface definition  for OERA Data Access objects
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 24 22:18:44 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.OERA.IDataAccess:  
  
    /*------------------------------------------------------------------------------
        Purpose: Fetch data from the data source
        Notes:
        @param poFetchDataRequest The IFetchDataRequest obejct with the parameters for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchData (poFetchDataRequest AS IFetchDataRequest) .  

    /*------------------------------------------------------------------------------
        Purpose: Allows retrieve multiple DB records from a Business Entity based on 
                 a temp-table with key values
        Notes:   
        @param poParameter The IFetchDataByKeyTableParameter with the data for this call
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchDataByKeyTable (poParameter AS IFetchDataByKeyTableParameter) .

    /*------------------------------------------------------------------------------
       Purpose:  Save changes to physical storage                
       Notes:                                                                           
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges ().

END INTERFACE.

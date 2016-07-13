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
    File        : DataSourceQueryInfoProvider
    Purpose     : Provides data source query information (for tracing purposes)
                  to the DataAccess:FetchData method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 27 17:34:03 CET 2013
    Notes       : Default IDataSourceQueryInfoProvider implementation 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.DataSourceQueryInfoProvider
    IMPLEMENTS IDataSourceQueryInfoProvider:  
  
	/*------------------------------------------------------------------------------
	   Purpose: Returns the reference to an IDataSourceQueryInfo object to the 
	            DataAccess:FetchData method
	   Notes:   If provided and type matches returns the CustomParameter reference 
	            from the IFetchDataRequest
	   @param pcDataAccessClassName The name of the Data Access class 
	   @param poFetchDataRequest The reference to the fetch data request object 
	   @return The reference to the IDataSourceQueryInfo implementation
	------------------------------------------------------------------------------*/
	METHOD PUBLIC IDataSourceQueryInfo GetDataSourceQueryInfo (pcDataAccessClassName AS CHARACTER,
	                                                           poFetchDataRequest AS IFetchDataRequest):
	     
        IF VALID-OBJECT (poFetchDataRequest) AND 
           VALID-OBJECT (poFetchDataRequest:CustomParameter) AND 
           TYPE-OF (poFetchDataRequest:CustomParameter, IDataSourceQueryInfo) THEN 
            
            RETURN CAST (poFetchDataRequest:CustomParameter, IDataSourceQueryInfo) .      
	                                                               
        RETURN ? .
        	                                                              
    END METHOD .	                                                              

END CLASS.

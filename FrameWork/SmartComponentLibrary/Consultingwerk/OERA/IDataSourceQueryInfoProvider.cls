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
    File        : IDataSourceQueryInfoProvider
    Purpose     : Interface for classes that provide data source query
                  information (for tracing purposes)
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Dec 27 17:34:03 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IDataSourceQueryInfoProvider:  
  
    /*------------------------------------------------------------------------------
       Purpose: Returns the reference to an IDataSourceQueryInfo object to the 
                DataAccess:FetchData method
       Notes:   An unknown value may be returned
       @param pcDataAccessClassName The name of the Data Access class 
       @param poFetchDataRequest The reference to the fetch data request object 
       @return The reference to the IDataSourceQueryInfo implementation
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IDataSourceQueryInfo GetDataSourceQueryInfo (pcDataAccessClassName AS CHARACTER,
                                                               poFetchDataRequest AS IFetchDataRequest).

END INTERFACE.

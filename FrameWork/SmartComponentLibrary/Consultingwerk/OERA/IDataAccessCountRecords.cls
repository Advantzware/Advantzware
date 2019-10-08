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
    File        : IDataAccessCountRecords
    Purpose     : Interface for Data Access classes that do support the 
                  CountResultRecords method
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 25 22:44:07 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IDataAccessCountRecords:  

    /*------------------------------------------------------------------------------
        Purpose: Counts the number of result records in the data source query
        Notes:   
        @param poFetchDataRequest The ICountRecordsRequest that also implements IFetchDataRequest  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CountResultRecords (poFetchDataRequest AS ICountRecordsRequest) .
          
END INTERFACE.

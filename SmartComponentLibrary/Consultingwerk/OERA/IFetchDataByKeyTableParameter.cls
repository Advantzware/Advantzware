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
    File        : IFetchDataByKeyTableParameter
    Purpose     : Parameter object for the FetchDataByKey method of the 
                  BusinessEntity and DataAccess class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Feb 07 14:17:38 CET 2015
    Notes       : Allows filtering rows from the DB by a table with key
                  values
                  SCL-647
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.OERA.IFetchDataByKeyTableParameter:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the temp-tabel with the key values used by 
                 the method
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY KeyValueTableHandle AS HANDLE NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of Tables to retrieve 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tables AS CHARACTER NO-UNDO 
    GET.

END INTERFACE.

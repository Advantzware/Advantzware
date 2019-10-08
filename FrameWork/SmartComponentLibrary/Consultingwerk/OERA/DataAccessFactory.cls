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
    File        : DataAccessFactory
    Purpose     : Default Factory of IDataAccess classes
    Syntax      : 
    Description : Default IDataAccessFactory implementation. Launches the 
                  Data Access object that is requested by a Business Entity
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sun Nov 29 19:19:20 CET 2015
    Notes       : See IDataAccessFactory documentation for use cases for 
                  custom service implementations
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

CLASS Consultingwerk.OERA.DataAccessFactory
    IMPLEMENTS IDataAccessFactory:  

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to an instance of the Data Access 
        Notes:
        @param pcDataAccessName The name of the Data Access obejct as known to the Business Entity 
        @param phDatasetHandle The handle of the Dataset that the Data Access is expected to bind to
        @return The reference to the Data Access class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IDataAccess GetDataAccessObject (pcDataAccessName AS CHARACTER,
                                                   phDatasetHandle AS HANDLE):

        DEFINE VARIABLE oDataAccess AS IDataAccess NO-UNDO .
        
        oDataAccess = DYNAMIC-NEW (pcDataAccessName) 
                                  (phDatasetHandle) .

        RETURN oDataAccess .

    END METHOD . 

END CLASS .

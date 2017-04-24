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
    File        : IDataAccessFactory
    Purpose     : Factory of IDataAccess classes
    Syntax      : 
    Description : Service Interface that allows to register custom factories
                  for Data Access objects. This might be useful when the 
                  Data Access of a Business Entity requires customization
                  for various tenants or to mock a Data Access obejct for
                  unit tests
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Sun Nov 29 19:19:20 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IDataAccessFactory:  

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to an instance of the Data Access 
        Notes:
        @param pcDataAccessName The name of the Data Access obejct as known to the Business Entity 
        @param phDatasetHandle The handle of the Dataset that the Data Access is expected to bind to
        @return The reference to the Data Access class
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IDataAccess GetDataAccessObject (pcDataAccessName AS CHARACTER,
                                                   phDatasetHandle AS HANDLE).

END INTERFACE.

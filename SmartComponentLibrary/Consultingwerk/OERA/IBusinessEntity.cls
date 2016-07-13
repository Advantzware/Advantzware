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
    File        : IBusinessEntity
    Purpose     : Provides Access and Business Logik for a ProDataset
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 24 22:22:20 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.OERA.IBusinessEntity:  

    /*------------------------------------------------------------------------------
        Purpose:  Fetch data and/or definitions  
        Notes:               
        @param poFetchDataRequest The IFetchDataRequest object with the parameter for this call
        @param phDataset OUTPUT Dataset with the resulting data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchData (poFetchDataRequest AS IFetchDataRequest,
                                  OUTPUT DATASET-HANDLE phDataset) .  

    /*------------------------------------------------------------------------------
        Purpose: Creates and returns an empty dataset to the caller                                                                         
        Notes:   Useful fo rdynamic callers that need access to the dataset schema prior to calling FetchData
        @param phDataset OUTPUT Dataset with the resulting data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FetchDataset (OUTPUT DATASET-HANDLE phDataset) .  
    
    /*------------------------------------------------------------------------------
        Purpose: Saves changes                                                                       
        Notes:   This method is called from consumers of the Business Entity and
                 receives the dataset.
                 This method is called by the ServiceInterface when no parameter object
                 has been passed in   
        @param phDataset INPUT-OUTPUT Dataset with changes                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset) .

    /*------------------------------------------------------------------------------
        Purpose: Saves changes                                                                       
        Notes:   This method is called from consumers of the Business Entity and
                 receives the dataset. 
                 This method is called by the ServiceInterface when a parameter object
                 has been passed in. The parameter object will be accessible from within 
                 the business entity using the protected property SaveChangesParameter    
        @param phDataset INPUT-OUTPUT Dataset with changes              
        @param poParameter Optional Parameter object for the SaveChanges method                                                       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SaveChanges (INPUT-OUTPUT DATASET-HANDLE phDataset,
                                    poParameter AS Progress.Lang.Object) .
                                          
END INTERFACE.

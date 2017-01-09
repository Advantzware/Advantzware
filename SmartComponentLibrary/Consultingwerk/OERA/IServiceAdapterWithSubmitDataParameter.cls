/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IServiceAdapterWithSubmitDataParameter
    Purpose     : Interface to be implemented by ServiceAdapter implementations
                  that support the SubmitData calls with on additional parameter
                  object instance 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 24 08:53:12 CEST 2014
    Notes       : This interface is no longer required to use. As of SCL-1045 
                  the basic IServiceAdapter interface was enhanced to contain 
                  all ServiceAdapter functionality. As such, all methods of 
                  the ServiceAdapter class are directly accessible from the 
                  FrameworkSettings:ServiceAdapter reference withour the 
                  previous need to case to this interface.
  ----------------------------------------------------------------------*/

USING Consultingwerk.*      FROM PROPATH . 
USING Consultingwerk.OERA.* FROM PROPATH .  
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IServiceAdapterWithSubmitDataParameter:  
  
    /*------------------------------------------------------------------------------
        Purpose: Saves modified (Create, Update, Delete) data to a Business Entity                                                                        
        Notes:   It's recommended but not required to just pass a changes dataset
                 The phContextDataset parameter is intended to be passed by-reference         
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
        @param pcContext INPUT-OUTPUT The context to be passed to the business entity
        @param poParameter An optional serializable parameter object to be exchanged with the business entity
        @param phContextDataset The handle of the context dataset for this call                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SubmitData (pcPartition    AS CHARACTER,
                                   pcEntityName   AS CHARACTER,
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset, 
                                   INPUT-OUTPUT pcContext      AS CHARACTER,
                                   poParameter                 AS ISerializable,
                                   phContextDataset AS HANDLE) .   

    /*------------------------------------------------------------------------------
        Purpose: Saves modified (Create, Update, Delete) data to a Business Entity                                                                        
        Notes:   It's recommended but not required to just pass a changes dataset         
        @param pcPartition The name of the AppServer partition for this call or "" for the default partition
        @param pcEntityName The name of the business entity
        @param hChangesDataset INPUT-OUTPUT DATASET-HANDLE The dataset with the modifications to the submitted
        @param pcContext INPUT-OUTPUT The context to be passed to the business entity
        @param poParameter An optional serializable parameter object to be exchanged with the business entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SubmitData (pcPartition AS CHARACTER, 
                                   pcEntityName AS CHARACTER, 
                                   INPUT-OUTPUT DATASET-HANDLE hChangesDataset, 
                                   INPUT-OUTPUT pcContext AS CHARACTER,
                                   poParameter AS ISerializable):
  
END INTERFACE.

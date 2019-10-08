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
    File        : IBusinessEntityGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 17 22:28:29 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Generator.IBusinessEntityGenerator:  
  
     { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }
  
    /*------------------------------------------------------------------------------
        Purpose: Returns a structure that contains the most recently generated file 
                 names
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Generated AS Generated NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Attempt to open files in OpenEdge Architect / Progress Developer Studio
                 for OpenEdge when done.
        Notes:                                                                           
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OpenFilesInIDE AS LOGICAL NO-UNDO 
    GET.
    SET.  
  
    /*------------------------------------------------------------------------------
        Purpose: Ignore status messages on completion
        Notes:   Default value should be FALSE                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SilentMode AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Complies the generated BusinessEntity, DataAcess and DatasetController
                 classes
        Notes:                                                                        
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL Compile (DATASET dsBusinessEntity) .
  
    /*------------------------------------------------------------------------------
        Purpose: Generates the source code                                                                       
        Notes:   This method will overwrite eventually existing source code
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GenerateCode (DATASET dsBusinessEntity) .

    /*------------------------------------------------------------------------------
        Purpose: Re-generates the source code                                                                      
        Notes:
        @param dsBusinessEntity The dataset with the Business Entity model
        @return Logical value indicating the success of the operation                                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL RegenerateCode (DATASET dsBusinessEntity) .

END INTERFACE.

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
    File        : ICustomBusinessEntityNameHandler
    Purpose     : Allows customizing how the after entering the Business 
                  Entity Name in the Business Entity Designer provides 
                  names for the Dataset, Data Access class, Description, ...
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 20 21:07:52 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ICustomBusinessEntityNameHandler:  

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the user fills in the Business EntityName
        Notes:   Allows deriving custom description, DatasetControllerType, dataset 
                 name etc. from the Business Entity Name 
        @param pcBusinessEntityName INPUT-OUTPUT The name for the Business Entity entered in the user interface 
        @param pcDatasetControllerType INPUT-OUTPUT The name for the Dataset Controller class
        @param pcBusinessEntityPurpose INPUT-OUTPUT The Business Entity Purpose
        @param pcDatasetName INPUT-OUTPUT The ProDataset name
        @param pcDataAccessName INPUT-OUTPUT The name for the Data Access class
        @return Logical value indicating if the values have been modified 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL DetermineBusinessEntityNameFields (INPUT-OUTPUT pcBusinessEntityName AS CHARACTER,
	                                                         INPUT-OUTPUT pcDatasetControllerType AS CHARACTER,
	                                                         INPUT-OUTPUT pcBusinessEntityPurpose AS CHARACTER,
	                                                         INPUT-OUTPUT pcDatasetName AS CHARACTER,
	                                                         INPUT-OUTPUT pcDataAccessName AS CHARACTER).

END INTERFACE.
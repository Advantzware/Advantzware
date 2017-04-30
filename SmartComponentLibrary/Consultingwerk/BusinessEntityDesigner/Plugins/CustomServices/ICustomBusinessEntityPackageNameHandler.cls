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
    File        : ICustomBusinessEntityPackageNameHandler
    Purpose     : Allows customizing how the after entering the Business 
                  Entity Package in the Business Entity Designer provides 
                  packages/paths for the Dataset, Data Access class, 
                  Description, ...
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 20 22:02:49 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ICustomBusinessEntityPackageNameHandler:  

    /*------------------------------------------------------------------------------
        Purpose: Invoked when the user fills in the Business Entity Package
        Notes:   Allows deriving custom packages or paths for Business Entity, DatasetController, 
                 dataset, temp-tables, etc. from the Business Entity Package 
        @param pcBusinessEntityPackage INPUT-OUTPUT The name for the Package of the Business Entity class 
        @param pcDatasetControllerPackage INPUT-OUTPUT The name for the Package of the Dataset Controller class
        @param pcDatasetPath INPUT-OUTPUT The dataset include path
        @param pcDefaultTablePath INPUT-OUTPUT The default temp-table include path
        @param pcDataAccessPackage INPUT-OUTPUT The name for the package for the Data Access class
        @return Logical value indicating if the values have been modified 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DetermineBusinessEntityPackageFields (INPUT-OUTPUT pcBusinessEntityPackage AS CHARACTER,
                                                                INPUT-OUTPUT pcDatasetControllerPackage AS CHARACTER,
                                                                INPUT-OUTPUT pcDatasetPath AS CHARACTER,
                                                                INPUT-OUTPUT pcDefaultTablePath AS CHARACTER,
                                                                INPUT-OUTPUT pcDataAccessPackage AS CHARACTER).

END INTERFACE.
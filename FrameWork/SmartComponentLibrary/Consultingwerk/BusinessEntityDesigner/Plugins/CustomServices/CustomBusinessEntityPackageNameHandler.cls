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
    File        : CustomBusinessEntityPackageNameHandler
    Purpose     : Default ICustomBusinessEntityPackageNameHandler implementation
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 20 22:09:09 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.CustomBusinessEntityPackageNameHandler 
    IMPLEMENTS ICustomBusinessEntityPackageNameHandler: 

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
                                                                INPUT-OUTPUT pcDataAccessPackage AS CHARACTER):

        DEFINE VARIABLE cBusinessEntityPackage    AS CHARACTER NO-UNDO .

        ASSIGN cBusinessEntityPackage = pcBusinessEntityPackage . 

        IF cBusinessEntityPackage = "":U THEN
            RETURN FALSE .
            
        IF cBusinessEntityPackage > "":U THEN DO:
            ASSIGN cBusinessEntityPackage = REPLACE (cBusinessEntityPackage, "~\":U, ".":U)
                   cBusinessEntityPackage = REPLACE (cBusinessEntityPackage, "/":U, ".":U)
                   
                   pcBusinessEntityPackage = cBusinessEntityPackage . 
            
            IF pcDatasetControllerPackage = "":U THEN 
                ASSIGN pcDatasetControllerPackage = cBusinessEntityPackage .   

            IF pcDatasetPath = "":U THEN 
                ASSIGN pcDatasetPath = REPLACE (cBusinessEntityPackage, ".":U, "/":U) .   

            IF pcDefaultTablePath = "":U THEN 
                ASSIGN pcDefaultTablePath = REPLACE (cBusinessEntityPackage, ".":U, "/":U) .   

            IF pcDataAccessPackage = "":U THEN 
                ASSIGN pcDataAccessPackage = cBusinessEntityPackage .   
        END.

        RETURN TRUE .

    END METHOD . 

END CLASS.

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
    File        : CustomBusinessEntityNameHandler
    Purpose     : Default service for determing the Business Entity Name
                  and names for the Dataset, Data Access class, Description
                  after typing the Business Entity Name
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 20 21:10:04 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.CustomBusinessEntityNameHandler
    IMPLEMENTS ICustomBusinessEntityNameHandler:

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
                                                             INPUT-OUTPUT pcDataAccessName AS CHARACTER):

        DEFINE VARIABLE cBusinessEntityName    AS CHARACTER NO-UNDO .

        ASSIGN cBusinessEntityName = pcBusinessEntityName .

        IF cBusinessEntityName = "":U THEN
            RETURN FALSE .

        IF cBusinessEntityName = "New":U THEN
            RETURN FALSE .

        IF INDEX (cBusinessEntityName, " ":U) > 0 THEN
            ASSIGN cBusinessEntityName = REPLACE (cBusinessEntityName, " ":U, "":U) .

        IF INDEX (cBusinessEntityName, ".":U) > 0 THEN
            ASSIGN cBusinessEntityName = REPLACE (cBusinessEntityName, ".":U, "":U) .

        pcBusinessEntityName = cBusinessEntityName .

        IF cBusinessEntityName MATCHES "*BusinessEntity":U THEN
            cBusinessEntityName = SUBSTRING (cBusinessEntityName, 1, LENGTH (cBusinessEntityName, "CHARACTER":U) - 14 /*LENGTH ("BusinessEntity":U)*/, "CHARACTER":U) .
        ELSE IF cBusinessEntityName MATCHES "*Entity":U THEN
            cBusinessEntityName = SUBSTRING (cBusinessEntityName, 1, LENGTH (cBusinessEntityName, "CHARACTER":U) - 6 /*LENGTH ("Entity":U)*/, "CHARACTER":U) .

        IF cBusinessEntityName > "":U THEN DO:
            IF pcDatasetControllerType = "":U THEN
                ASSIGN pcDatasetControllerType = cBusinessEntityName + "DatasetController":U .

            IF pcBusinessEntityPurpose = "":U THEN
                ASSIGN pcBusinessEntityPurpose = "Business Entity for "{&TRAN} + cBusinessEntityName .

            IF pcDatasetName = "":U THEN
                ASSIGN pcDatasetName = "ds":U + cBusinessEntityName .

            IF pcDataAccessName = "":U THEN
                ASSIGN pcDataAccessName = cBusinessEntityName + "DataAccess":U .
        END.

        RETURN TRUE .

    END METHOD .

END CLASS.

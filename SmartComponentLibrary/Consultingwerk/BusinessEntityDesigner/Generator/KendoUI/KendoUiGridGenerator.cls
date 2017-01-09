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
    File        : KendoUiGridGenerator
    Purpose     : Generates an html page containing a Kendo UI grid 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon May 04 11:05:32 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                                FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.* FROM PROPATH .  
USING Consultingwerk.Util.*                                     FROM PROPATH .
USING Progress.Lang.*                                           FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.KendoUI.KendoUiGridGenerator: 

    /*------------------------------------------------------------------------------
        Purpose: Generates the html page based on the IKendoUiGridBuilder instance
        Notes:   
        @param poBuilder The reference to the KendoUiGridBuilder instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Generate (poBuilder AS IKendoUiGridBuilder):
        
        DEFINE VARIABLE lcSource AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcGrid   AS LONGCHAR NO-UNDO .
        
        ObjectAssert:IsValid (poBuilder) .

        lcGrid = poBuilder:GetGrid() .
        
        lcGrid = StringHelper:Indent (lcGrid, 20) .

        COPY-LOB FROM FILE poBuilder:TemplateFileName TO lcSource . 
        
        ASSIGN lcSource = REPLACE (lcSource, "@PAGE_TITLE@":U, poBuilder:PageTitle) 
               lcSource = REPLACE (lcSource, "@DATASOURCE_NAME@":U, poBuilder:DataSourceName)
               lcSource = REPLACE (lcSource, "@TABLE_NAME@":U, poBuilder:DataSourceTableName)
               lcSource = REPLACE (lcSource, "@GRID_DEFINITION@":U, lcGrid)
            .

        COPY-LOB FROM lcSource TO FILE poBuilder:OutputFileName .

    END METHOD .

END CLASS.

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
    File        : SampleNewTempTableHandler
    Purpose     : Sample service as a classes that provides call back 
                  functionality when a new Temp-Table has been created
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Feb 25 19:38:51 CET 2014
    Notes       : Allows modification of the eTable, eBusinessEntity record as well 
                  as the eField records
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.BusinessEntityDesigner.Services.*               FROM PROPATH .                     
USING Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.SampleNewTempTableHandler 
    IMPLEMENTS INewTempTableHandler: 

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i }

    /*------------------------------------------------------------------------------
        Purpose: Callback method that allows modification of the Business Entity Model
                 after a new Table has been created
        Notes:   Allows modification of the eTable, eBusinessEntity record as well 
                 as the eField records
        @param pcTempTableName The name of the new temp-table
        @param dsBusinessEntity INPUT-OUTPUT DATASET The Businesss Entity Dataset (should be passed BY-REFERENCE)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID HandleNewTempTable (pcTempTableName AS CHARACTER,
                                           INPUT-OUTPUT DATASET dsBusinessEntity):
		
		DEFINE VARIABLE oDatasetController AS BusinessEntityDatasetController  NO-UNDO .
        DEFINE VARIABLE oNameHandler       AS ICustomBusinessEntityNameHandler NO-UNDO . 

		oDatasetController = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Services.BusinessEntityDatasetController} . 
		
        FIND FIRST eBusinessEntity NO-ERROR . 
        
        IF NOT AVAILABLE eBusinessEntity THEN 
            RETURN . 
           
        FIND eTable WHERE eTable.TempTableName = pcTempTableName .   
            
        IF eBusinessEntity.BusinessEntityName = "New":U AND eTable.SourceTableNames > "":U THEN DO:
            
            ASSIGN eBusinessEntity.BusinessEntityName = SUBSTITUTE ("&1BusinessEntity":U,
                                                                    ENTRY (1, eTable.SourceTableNames)) 
                   
                   oNameHandler = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.CustomServices.ICustomBusinessEntityNameHandler} .

            IF VALID-OBJECT (oNameHandler) THEN 
                oNameHandler:DetermineBusinessEntityNameFields (INPUT-OUTPUT eBusinessEntity.BusinessEntityName,
                                                                INPUT-OUTPUT eBusinessEntity.DatasetControllerName,
                                                                INPUT-OUTPUT eBusinessEntity.BusinessEntityPurpose,
                                                                INPUT-OUTPUT eBusinessEntity.DatasetName,
                                                                INPUT-OUTPUT eBusinessEntity.DataAccessName) .

            oDatasetController:BusinessEntityViewerControl:SmartDataSource:BindingSource:Refresh () .
        END. 

/*            define buffer bMLD_eTable for eTable.                                                      */
/*                                                                                                       */
/*            create bMLD_eTable.                                                                        */
/*            assign                                                                                     */
/*                bMLD_eTable.BusinessEntityName = eTable.BusinessEntityName                             */
/*                bMLD_eTable.TempTableName = eTable.TempTableName + "_MLD"                              */
/*                bMLD_eTable.SourceTableNames = eTable.SourceTableNames + "_MLD"                        */
/*                bMLD_eTable.SourceBufferNames = eTable.SourceBufferNames + "_MLD"                      */
/*                bMLD_eTable.SourceDefaultQuery = "" /*eTable.SourceDefaultQuery*/                      */
/*                bMLD_eTable.TempTablePurpose = substitute ("Translations for &1", eTable.TempTableName)*/
/*                bMLD_eTable.TempTableDescription = ""                                                  */
/*                bMLD_eTable.TempTablePath = eTable.TempTablePath                                       */
/*                bMLD_eTable.TempTableBeforeName = eTable.TempTableBeforeName + "_MLD"                  */
/*                bMLD_eTable.LayoutColumn = eTable.LayoutColumn + eTable.LayoutWidth + 10               */
/*                bMLD_eTable.LayoutRow = eTable.LayoutRow + 10                                          */
/*                bMLD_eTable.LayoutWidth = eTable.LayoutWidth                                           */
/*                bMLD_eTable.LayoutHeight = eTable.LayoutHeight                                         */
/*                bMLD_eTable.NamespaceUri = eTable.NamespaceUri                                         */
/*                bMLD_eTable.NamespacePrefix = eTable.NamespacePrefix                                   */
/*                bMLD_eTable.XmlNodeName = eTable.XmlNodeName  + "_MLD"                                 */
/*                bMLD_eTable.SerializeName = eTable.SerializeName  + "_MLD"                             */
/*                bMLD_eTable.NoBeforeTable = eTable.NoBeforeTable                                       */
/*                bMLD_eTable.EntityPackageName = eTable.EntityPackageName                               */
/*                bMLD_eTable.EntityClassName = eTable.EntityClassName                                   */
/*                bMLD_eTable.DeletedFields = "" .                                                       */
/*                                                                                                       */
/*            oDatasetController:Refresh() .                                                             */

	END METHOD .

END CLASS.

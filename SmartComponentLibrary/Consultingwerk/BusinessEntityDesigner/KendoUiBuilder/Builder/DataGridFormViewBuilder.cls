/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DataGridFormViewBuilder
    Purpose     : Builder for data-grid-form Views
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Oct 03 08:00:50 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.*       FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.Builder.* FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.*         FROM PROPATH .
USING Consultingwerk.Framework.*                                     FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                         FROM PROPATH .
USING Consultingwerk.Web.KendoUiBuilder.Module.*                     FROM PROPATH .
USING Consultingwerk.Util.*                                          FROM PROPATH .
USING Progress.Lang.*                                                FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.Builder.DataGridFormViewBuilder
    IMPLEMENTS IViewBuilder:

    {Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i}

    /**
     * Purpose: Creates a view with the provided specifications to the module
     * Notes:   Launches the specific view generator instance
     * @param poModule The module to add the new view to
     * @param dsBusinessEntity The dataset with the Business Entity Model
     * @param pcTables The entity table and view tables
     * @param pcViewType The type of the view to create
     * @param pcViewName The name of the view to create
     * @param pcDataProvider The name of the data provider
     * @param pcDataSource The name of the data source
     * @param poGridColumns The list with the column names to use for the grid
     * @param poViewerFields The list with the viewer field specs
     */
    METHOD PUBLIC VOID CreateView (poModule AS Module,
                                   DATASET FOR dsBusinessEntity,
                                   pcTables AS CHARACTER,
                                   pcViewType AS CHARACTER,
                                   pcViewName AS CHARACTER,
                                   pcDataProvider AS CHARACTER,
                                   pcDataSource AS CHARACTER,
                                   poGridColumns AS CharacterList,
                                   poViewerFields AS ListViewerFieldSpec):

        DEFINE VARIABLE oView                 AS View                   NO-UNDO .
        DEFINE VARIABLE oDataGridForm         AS DataGridForm           NO-UNDO .
        DEFINE VARIABLE hDataset              AS HANDLE                 NO-UNDO .
        DEFINE VARIABLE oSemanticTypeProvider AS ISemanticTypesProvider NO-UNDO .
        DEFINE VARIABLE cSemanticType         AS CHARACTER              NO-UNDO .
        DEFINE VARIABLE cEditorType           AS CHARACTER              NO-UNDO .

        oSemanticTypeProvider = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.KendoUiBuilder.ISemanticTypesProvider
                                                              "NEW SemanticTypesProvider()"} .

        oView = poModule:children:Add (pcViewName, "data-grid-form":U) .

        oDataGridForm = NEW DataGridForm (pcViewName) .

        oView:children:Add (oDataGridForm) .

        ASSIGN oDataGridForm:extensionFolderName = SUBSTITUTE ("&1-&2":U, poModule:Name, pcViewName)
               oDataGridForm:dataProvider        = pcDataProvider
               oDataGridForm:dataSource          = pcDataSource
            .

        FIND FIRST eBusinessEntity .

        FrameworkSettings:ServiceAdapter:FetchDataset ("":U,
                                                      SUBSTITUTE ("&1.&2":U,
                                                                  eBusinessEntity.BusinessEntityPackage,
                                                                  eBusinessEntity.BusinessEntityName),
                                                      OUTPUT DATASET-HANDLE hDataset) .

        {Consultingwerk/foreachPrimitiveList.i Character cColumnName in poGridColumns}
            oDataGridForm:columns:Add (cColumnName) .
        END.

        {Consultingwerk/foreachABL.i ViewerFieldSpec oField IN poViewerFields}

            ASSIGN cSemanticType = oSemanticTypeProvider:DefaultSemanticTypeForDataType (oField:DataType)
                   cEditorType   = oSemanticTypeProvider:DefaultEditorForSemanticType(cSemanticType) .

            oDataGridForm:fields:Add (oField:ColumnName,
                                      oField:FieldLabel,
                                      cEditorType,
                                      "~{0}":U,
                                      oField:ControlName) .
        END.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN
                DELETE OBJECT hDataset .
        END FINALLY.

    END METHOD .

END CLASS.

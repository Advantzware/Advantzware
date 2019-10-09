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
    File        : RollbaseImporterPlugin
    Purpose     : Business Entity Designer Plugin that supports importing
                  Rollbase Objects
    Syntax      :
    Description : Adds an "Import Rollbase Object" button to the Ribbon of
                  the Business Entity Designer. This plugin allows to import
                  the structure of an object (class) in Rollbase as a temp-table
                  as part of the Business Entity.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 29 07:36:31 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.*                           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.*                   FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.RollbaseImporter.*  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.UI.*                        FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                            FROM PROPATH .
USING Consultingwerk.Util.*                                             FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                               FROM ASSEMBLY  .
USING Progress.Lang.*                                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Plugins.RollbaseImporterPlugin
    IMPLEMENTS IBusinessEntityDesignerPlugin:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &REFERENCE-ONLY="REFERENCE-ONLY" }

&IF NOT PROVERSION EQ "10.2B":U &THEN
    { Consultingwerk/RollbaseAdapter/MetaData/dsObjectDef.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the BusinessEntityDatasetController
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Parent Form
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Form AS System.Windows.Forms.Form NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Imports an Object from Rollbase
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportRollbaseObject ():

        DEFINE VARIABLE oForm          AS SelectObjectNameDialog                          NO-UNDO .
        DEFINE VARIABLE oDialogResult  AS System.Windows.Forms.DialogResult               NO-UNDO .
        DEFINE VARIABLE oAdapter       AS Consultingwerk.RollbaseAdapter.IRollbaseAdapter NO-UNDO .
        DEFINE VARIABLE cName          AS CHARACTER                                       NO-UNDO .
        DEFINE VARIABLE cDataType      AS CHARACTER                                       NO-UNDO .

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity .
        DEFINE BUFFER b_eTable FOR eTable .
        DEFINE BUFFER b_eField FOR eField .
        DEFINE BUFFER b_eIndex FOR eIndex .

        DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.

        DATASET dsObjectDef:EMPTY-DATASET ().

        /* Initialize RollbaseCredentials Service - when not previously registered */
        {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseCredentials
                                      "NEW Consultingwerk.RollbaseAdapter.RollbaseCredentials ()"} .

        oForm = NEW SelectObjectNameDialog () .

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT:Form) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult}

        ASSIGN cName = oForm:IntegrationName .

        oAdapter = {Consultingwerk/get-service.i Consultingwerk.RollbaseAdapter.IRollbaseAdapter
                                                 "NEW Consultingwerk.RollbaseAdapter.RollbaseAdapter()"} .

        DO ON ERROR UNDO, THROW:

            oAdapter:GetObjectDef (cName,
                                   OUTPUT DATASET dsObjectDef) .

            CATCH err AS Progress.Lang.Error:
                UNDO, THROW NEW Consultingwerk.Exceptions.Exception (err,
                                                                     SUBSTITUTE ("Error retrieving Object Definition for ~"&1~" from Rollbase server"{&TRAN}, cName),
                                                                     0) .
            END CATCH.
        END .

        FIND FIRST DataObjectDef .
        FIND FIRST b_eBusinessEntity .

        CREATE b_eTable .
        ASSIGN b_eTable.BusinessEntityName = b_eBusinessEntity.BusinessEntityName
               b_eTable.TempTablePath      = b_eBusinessEntity.DatasetPath
               b_eTable.EntityClassName    = DataObjectDef.SingularName
               b_eTable.EntityPackageName  = b_eBusinessEntity.BusinessEntityPackage
               b_eTable.SerializeName      = DataObjectDef.objDefName
               b_eTable.XmlNodeName        = DataObjectDef.objDefName
               b_eTable.TempTablePurpose   = SUBSTITUTE ("Temp-Table for Rollbase Object &1"{&TRAN}, DataObjectDef.objDefName)
               cName                       = SUBSTITUTE ("&1&2&3":U,
                                                         BusinessEntityDesignerSettings:DefaultTablePrefix,
                                                         DataObjectDef.SingularName,
                                                         BusinessEntityDesignerSettings:DefaultTableSuffix).

        IF NOT CAN-FIND (FIRST b_eTable WHERE b_eTable.TempTableName = cName) THEN
            ASSIGN b_eTable.TempTableName = cName .
        ELSE
            ASSIGN b_eTable.TempTableName = THIS-OBJECT:Controller:BuildUniqueTableName (cName) .

        CREATE b_eIndex .
        ASSIGN b_eIndex.BusinessEntityName = b_eTable.BusinessEntityName
               b_eIndex.TempTableName      = b_eTable.TempTableName
               b_eIndex.IndexName          = "id":U
               b_eIndex.IndexOrder         = 1
               b_eIndex.FieldNames         = "id":U
               b_eIndex.FieldSort          = "ASCENDING":U
               b_eIndex.IndexPrimary       = TRUE
               b_eIndex.IndexUnique        = TRUE .

        FOR EACH DataFieldDef BY DataFieldDef.id ON ERROR UNDO, THROW:
            IF DataFieldDef.dataName = "FieldRelationShip":U THEN
                NEXT .

            ASSIGN cDataType = oAdapter:RollbaseDataTypeToAbl (DataFieldDef.dataName) .

            IF cDataType > "":U THEN .
            ELSE
                NEXT .

            CREATE b_eField .
            ASSIGN iOrder                      = iOrder + 1
                   b_eField.BusinessEntityName = b_eTable.BusinessEntityName
                   b_eField.TempTableName      = b_eTable.TempTableName
                   b_eField.FieldOrder         = iOrder
                   b_eField.FieldName          = DataFieldDef.fieldName
                   b_eField.FieldLabel         = DataFieldDef.DisplayLabel
                   b_eField.FieldDataType      = cDataType
                   b_eField.FieldSerializeName = DataFieldDef.fieldName
                   b_eField.FieldXmlNodeName   = DataFieldDef.fieldName
                   .

            RELEASE b_eField .
        END.

        THIS-OBJECT:Controller:Refresh() .

        RELEASE b_eTable .

        FINALLY:
            DATASET dsObjectDef:EMPTY-DATASET ().
        END FINALLY.

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Shuts down the plugin
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Shutdown ():

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the plugin
        Notes:
        @param dsBusinessEntity Business Entity with the Design dataset, called with the BIND keyword
        @param poController The BusinessEntityDatasetController
        @param poForm The main Form of the BusinessEntityDesigner
        @param poToolbar The reference to the UltraToolbarsManager used by the BusinessEntityDesigner
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Startup (DATASET FOR dsBusinessEntity BIND,
                                poController AS BusinessEntityDatasetController,
                                poForm AS BusinessEntityDesignerForm,
                                poToolbar AS UltraToolbarsManager):

&IF NOT PROVERSION EQ "10.2B":U &THEN
        DEFINE VARIABLE oGenerarteViewer AS ButtonTool            NO-UNDO .
        DEFINE VARIABLE oGroup           AS RibbonGroup           NO-UNDO .
        DEFINE VARIABLE oBitmap          AS System.Drawing.Bitmap NO-UNDO .

        THIS-OBJECT:Form = poForm .
        THIS-OBJECT:Controller = poController .

        oGenerarteViewer = NEW ButtonTool ("RollbaseImporter_Plugin_Import":U) .
        oGenerarteViewer:SharedProps:Caption = "Import Rollbase Object"{&TRAN} .
        oGenerarteViewer:SharedProps:ToolTipText = "Import Rollbase Object Definition as Temp-Table"{&TRAN} .
        oGenerarteViewer:SharedProps:DisplayStyle = ToolDisplayStyle:ImageAndText .
        poToolbar:Tools:Add (oGenerarteViewer) .

        oBitmap = Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/Plugins/RollbaseImporter/Images/psc_spark_16.png":U) .
        oBitmap:MakeTransparent () .

        ASSIGN oGenerarteViewer:SharedProps:AppearancesSmall:Appearance:Image = oBitmap .

        oBitmap = Consultingwerk.Util.ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/Plugins/RollbaseImporter/Images/psc_spark_32.png":U) .
        oBitmap:MakeTransparent () .

        ASSIGN oGenerarteViewer:SharedProps:AppearancesLarge:Appearance:Image = oBitmap .

        oGroup = poToolbar:Ribbon:Tabs [0]:Groups ["Design":U] .

        oGroup:Tools:AddTool ("RollbaseImporter_Plugin_Import":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .
        oGroup:Tools["RollbaseImporter_Plugin_Import":U]:InstanceProps:IsFirstInGroup = TRUE .

        poToolbar:ToolClick:Subscribe (ToolClickHandler) .
&ENDIF
    END METHOD .

&IF NOT PROVERSION EQ "10.2B":U &THEN
    /*------------------------------------------------------------------------------
        Purpose: Handles the ToolClick event of the Business Entity Designer's
                 UltraToolbarsManager
        Notes:
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToolClickHandler (sender AS System.Object,
                                            e AS ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "RollbaseImporter_Plugin_Import":U THEN
                ImportRollbaseObject () .
        END CASE .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.
    END METHOD.
&ENDIF

END CLASS.

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
    File        : BusinessEntitySourceEditorForm
    Purpose     : An Editor for the generated source code
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jun 28 12:26:40 CEST 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.BusinessEntityDesigner.*                                  FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.* FROM PROPATH.
USING Consultingwerk.BusinessEntityDesigner.UI.*                               FROM PROPATH .
USING Consultingwerk.Framework.Collections.*                                   FROM PROPATH .
USING Consultingwerk.Studio.ProcedureEditor.*                                  FROM PROPATH .
USING Consultingwerk.Util.*                                                    FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                                      FROM ASSEMBLY .
USING Progress.Lang.*                                                          FROM PROPATH .
USING org.prorefactor.core.JPNode                                              FROM ASSEMBLY .

CLASS Consultingwerk.BusinessEntityDesigner.UI.BusinessEntitySourceEditorForm
    INHERITS SmartEditorForm:

    { Consultingwerk/BusinessEntityDesigner/Services/dsBusinessEntity.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    DEFINE VARIABLE lLoading AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntitySourceEditorForm class
        Notes:
        @param phDataset The handle of the dsBusinessEntity used in the Business Entity Designer
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntitySourceEditorForm (phDataset AS HANDLE):
        SUPER ().

        THIS-OBJECT:Text = "Business Entity Editor":U .
        THIS-OBJECT:WindowPositionRegistryKey = "BusinessEntitySourceEditorForm":U .
        THIS-OBJECT:DeleteOnFormClosed = TRUE .

        BindDataset (DATASET-HANDLE phDataset BIND) .

        THIS-OBJECT:CreateRibbonTools () .

        THIS-OBJECT:ultraToolbarsManager1:ToolClick:Subscribe (ultraToolbarsManager1_ToolClick) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsBusinessEntity DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsBusinessEntity BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Ribbon Tools specific to the Business Entity Source Editor
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateRibbonTools ():

        DEFINE VARIABLE oRibbonGroup AS RibbonGroup NO-UNDO .
        DEFINE VARIABLE oTool        AS ButtonTool  NO-UNDO .

        oRibbonGroup = ultraToolbarsManager1:Ribbon:Tabs[0]:Groups:Insert (2, "BE Wizards":U) .
        oRibbonGroup:Caption = "Business Entity Wizards"{&TRAN} .

        oTool = NEW ButtonTool ("BE_Client_Callable_Method":U) .
        ultraToolbarsManager1:Tools:Add (oTool) .
        oTool:SharedProps:Caption = "Invokable method"{&TRAN} .
        oTool:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/handshake_16.png":U) .
        oTool:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/handshake_32.png":U) .
        oTool:SharedProps:Enabled = FALSE .
        oRibbonGroup:Tools:AddTool ("BE_Client_Callable_Method":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        /* ** */

        oTool = NEW ButtonTool ("DAO_Dataset_Callback":U) .
        ultraToolbarsManager1:Tools:Add (oTool) .
        oTool:SharedProps:Caption = "ProDataset Callback"{&TRAN} .
        oTool:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/data_gear_16.png":U) .
        oTool:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/data_gear_32.png":U) .
        oTool:SharedProps:Enabled = FALSE .
        oRibbonGroup:Tools:AddTool ("DAO_Dataset_Callback":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        /* ** */

        oTool = NEW ButtonTool ("DAO_Validation":U) .
        ultraToolbarsManager1:Tools:Add (oTool) .
        oTool:SharedProps:Caption = "Validation method"{&TRAN} .
        oTool:SharedProps:AppearancesSmall:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/data_ok_16.png":U) .
        oTool:SharedProps:AppearancesLarge:Appearance:Image = ImageHelper:Load ("Consultingwerk/BusinessEntityDesigner/UI/Images/data_ok_32.png":U) .
        oTool:SharedProps:Enabled = FALSE .
        oRibbonGroup:Tools:AddTool ("DAO_Validation":U):InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog and allows inserting a client-callable method
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertClientCallableMethod ():

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oForm         AS InsertClientCallableMethodWizard  NO-UNDO .
        DEFINE VARIABLE oGenerator    AS IClassAnnotationGenerator         NO-UNDO .
        DEFINE VARIABLE cDirectory    AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cFileMask     AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE cCurrentFile  AS CHARACTER                         NO-UNDO .

        DEFINE VARIABLE cTemplate     AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE lcCode        AS LONGCHAR                          NO-UNDO .

        THIS-OBJECT:SetTimersEnabled (FALSE) .

        oForm = NEW InsertClientCallableMethodWizard () .

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity.

        FIND FIRST b_eBusinessEntity .

        cTemplate = TemplateHelper:Find ("Methods/client_callable_method.template":U) .

        COPY-LOB FROM FILE cTemplate TO lcCode .

        ASSIGN lcCode = SUBSTITUTE (lcCode, oForm:MethodName,
                                            b_eBusinessEntity.DatasetName,
                                            oForm:ParameterType) .

        THIS-OBJECT:InsertMethod (oForm:MethodName, STRING (lcCode)) .

        IF oForm:UpdateAnnotations = TRUE THEN DO:
            THIS-OBJECT:Save () .

            ASSIGN cCurrentFile = THIS-OBJECT:GetCurrentFileName ()
                   cDirectory   = FileHelper:DirectoryName (cCurrentFile)
                   cFileMask    = FileHelper:ShortFileName (cCurrentFile) .

            ASSIGN oGenerator = {Consultingwerk/get-service.i Consultingwerk.BusinessEntityDesigner.Plugins.ClassAnnotationGenerator.IClassAnnotationGenerator
                                                              "NEW ClassAnnotationGenerator ()" } .

            oGenerator:GenerateClassAnnotations (cDirectory,
                                                 cFileMask) .
        END.

        FINALLY:
            THIS-OBJECT:SetTimersEnabled (TRUE) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog and allows inserting a dataset read callback
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertDatasetCallback ():

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oForm         AS InsertDatasetCallbackWizard       NO-UNDO .
        DEFINE VARIABLE cTemplate     AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE lcCode        AS LONGCHAR                          NO-UNDO .
        DEFINE VARIABLE oBuffers      AS CharacterList                     NO-UNDO .
        DEFINE VARIABLE cMethodName   AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE oEndNode      AS JPNode                            NO-UNDO .
        DEFINE VARIABLE oEditor       AS ProcedureEditorForm               NO-UNDO .

        DEFINE VARIABLE hWindow       AS HANDLE                            NO-UNDO .
        DEFINE VARIABLE hEditor       AS HANDLE                            NO-UNDO .

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity .
        DEFINE BUFFER b_eTable          FOR eTable .

        THIS-OBJECT:SetTimersEnabled (FALSE) .

        FIND FIRST b_eBusinessEntity .

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN
            RETURN .

        oEditor = CAST (THIS-OBJECT:ActiveMdiChild,
                        ProcedureEditorForm) .

        oBuffers = NEW CharacterList () .

        oBuffers:Add (SUBSTITUTE ("&1 (ProDataset)":U, b_eBusinessEntity.DatasetName)) .

        FOR EACH b_eTable:
            oBuffers:Add (b_eTable.TempTableName) .
        END.

        oForm = NEW InsertDatasetCallbackWizard () .

        oForm:BufferList = oBuffers .

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        cTemplate = TemplateHelper:Find ("Methods/dataset_callback_method.template":U) .

        COPY-LOB FROM FILE cTemplate TO lcCode .

        ASSIGN cMethodName = ENTRY (1, oForm:BufferName, " ":U) + oForm:CallbackType
               cMethodName = REPLACE (cMethodName, "AFTER-ROW-FILL":U, "AfterRowFill":U)
               cMethodName = REPLACE (cMethodName, "BEFORE-ROW-FILL":U, "BeforeRowFill":U)
               cMethodName = REPLACE (cMethodName, "AFTER-FILL":U, "AfterFill":U)
               cMethodName = REPLACE (cMethodName, "BEFORE-FILL":U, "BeforeFill":U)

               lcCode      = SUBSTITUTE (lcCode,
                                         cMethodName,
                                         ENTRY (1, oForm:BufferName, " ":U),
                                         oForm:CallbackType,
                                         b_eBusinessEntity.DatasetName) .

        THIS-OBJECT:InsertMethod (cMethodName,
                                  STRING (lcCode)) .

        THIS-OBJECT:PopulateMethodTempTable () .

        FIND FIRST ttMethods WHERE ttMethods.MethodName = "DefineReadEvents":U .

        IF AVAILABLE ttMethods THEN DO:
            oEndNode = Consultingwerk.Studio.Proparse.ProparseHelper:FindChildNodeOfNodeType
                            (THIS-OBJECT:Nodes[ttMethods.NodeNum],
                             "END":U) .

            IF VALID-OBJECT (oEndNode) THEN DO:
                oEditor:MoveCursor (oEndNode:getLine ()) .

                ASSIGN hWindow = oEditor:EmbeddedWindow .

                RUN adecomm/_pwgeteh.p (hWindow , OUTPUT hEditor).

                cTemplate = TemplateHelper:Find ("Methods/dataset_callback_subscribe.template":U) .

                COPY-LOB FROM FILE cTemplate TO lcCode .

                ASSIGN lcCode = SUBSTITUTE (lcCode,
                                            (IF NUM-ENTRIES (oForm:BufferName, " ":U) > 1 THEN "DATASET":U ELSE "BUFFER":U),
                                            ENTRY (1, oForm:BufferName, " ":U),
                                            oForm:CallbackType,
                                            cMethodName) .

                hEditor:INSERT-STRING (STRING (lcCode)) .
            END.
        END.

        FINALLY:
            THIS-OBJECT:SetTimersEnabled (TRUE) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Shows a dialog and allows inserting a validation hook
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertValidationHook ():

        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE oForm         AS InsertDataAccessValidationWizard  NO-UNDO .

        DEFINE VARIABLE cTemplate     AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE lcCode        AS LONGCHAR                          NO-UNDO .

        DEFINE VARIABLE oBuffers      AS CharacterList                     NO-UNDO .

        DEFINE BUFFER b_eTable          FOR eTable .

        DEFINE VARIABLE cFind AS CHARACTER NO-UNDO.

        THIS-OBJECT:SetTimersEnabled (FALSE) .

        oBuffers = NEW CharacterList () .

        FOR EACH b_eTable:
            oBuffers:Add (b_eTable.TempTableName) .
        END.

        oForm = NEW InsertDataAccessValidationWizard () .

        oForm:BufferList = oBuffers .

        WAIT-FOR oForm:ShowDialog (THIS-OBJECT) SET oDialogResult .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        cTemplate = TemplateHelper:Find ("Methods/data_access_validation_method.template":U) .

        COPY-LOB FROM FILE cTemplate TO lcCode .

        IF NOT oForm:ValidationType MATCHES "*delete*":U THEN DO:
            FIND FIRST b_eTable WHERE b_eTable.TempTableName = oForm:BufferName .

            IF b_eTable.TempTableBeforeName > "":U THEN
                ASSIGN cFind = SUBSTITUTE ("Consultingwerk.Util.BufferHelper:FindAfterBuffer (BUFFER &1:HANDLE) .":U,
                                           b_eTable.TempTableBeforeName) .
        END.

        ASSIGN lcCode = SUBSTITUTE (lcCode, oForm:BufferName,
                                            oForm:ValidationType,
                                            cFind) .

        THIS-OBJECT:InsertMethod (oForm:BufferName + oForm:ValidationType,
                                  STRING (lcCode)) .

        FINALLY:
            THIS-OBJECT:SetTimersEnabled (TRUE) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MdiChildActivate event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnFormClosed (e AS System.Windows.Forms.FormClosedEventArgs):

        DEFINE VARIABLE oAbhackSupportService AS IAbhackSupportService NO-UNDO .

        oAbhackSupportService = {Consultingwerk/get-service.i Consultingwerk.Studio.ProcedureEditor.IAbhackSupportService} .

        IF VALID-OBJECT (oAbhackSupportService) THEN
            oAbhackSupportService:CloseAbhack() .

        SUPER:OnFormClosed (e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the MdiChildActivate event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnMdiChildActivate (e AS System.EventArgs):

        DEFINE VARIABLE lReadOnly             AS LOGICAL               NO-UNDO INIT FALSE .
        DEFINE VARIABLE oAbhackSupportService AS IAbhackSupportService NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) AND
                TYPE-OF (THIS-OBJECT:ActiveMdiChild, ProcedureEditorForm) THEN

            lReadOnly = CAST (THIS-OBJECT:ActiveMdiChild, ProcedureEditorForm):IsReadOnly .

        SUPER:OnMdiChildActivate (e).

        ASSIGN ultraToolbarsManager1:Tools["BE_Client_Callable_Method":U]:SharedProps:Enabled = FALSE
               ultraToolbarsManager1:Tools["DAO_Dataset_Callback":U]:SharedProps:Enabled = FALSE
               ultraToolbarsManager1:Tools["DAO_Validation":U]:SharedProps:Enabled = FALSE
               .

        IF VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) THEN DO:
            THIS-OBJECT:ultraToolbarsManager1:Tools ["Insert Method":U]:SharedProps:Enabled = NOT lReadOnly .

            IF UNBOX (THIS-OBJECT:ActiveMdiChild:Tag) = "BE":U THEN
                ASSIGN ultraToolbarsManager1:Tools["BE_Client_Callable_Method":U]:SharedProps:Enabled = NOT lReadOnly .
            ELSE IF UNBOX (THIS-OBJECT:ActiveMdiChild:Tag) = "DAO":U THEN
                ASSIGN ultraToolbarsManager1:Tools["DAO_Dataset_Callback":U]:SharedProps:Enabled = NOT lReadOnly
                       ultraToolbarsManager1:Tools["DAO_Validation":U]:SharedProps:Enabled = NOT lReadOnly
                        .
        END.

        IF NOT lLoading AND NOT lReadOnly AND
           VALID-OBJECT (THIS-OBJECT:ActiveMdiChild) AND
                TYPE-OF (THIS-OBJECT:ActiveMdiChild, ProcedureEditorForm) THEN DO:
            oAbhackSupportService = {Consultingwerk/get-service.i Consultingwerk.Studio.ProcedureEditor.IAbhackSupportService} .

            IF VALID-OBJECT (oAbhackSupportService) THEN
                oAbhackSupportService:ActivateEditor (THIS-OBJECT:ActiveMdiChild) .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        DEFINE VARIABLE oService AS IAbhackSupportService NO-UNDO .

        SUPER:OnShown (e) .

        PROCESS EVENTS .

        oService = {Consultingwerk/get-service.i Consultingwerk.Studio.ProcedureEditor.IAbhackSupportService} .

        IF VALID-OBJECT (oService) THEN
            oService:InitializeAbhack() .

        PROCESS EVENTS .

        THIS-OBJECT:OpenBusinessEntity () .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the Business Entity files
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenBusinessEntity ():

        DEFINE VARIABLE cBusinessEntityFile    AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDataAccessFile        AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDatasetControllerFile AS CHARACTER NO-UNDO.

        DEFINE VARIABLE cIncludeFile           AS CHARACTER NO-UNDO .

        DEFINE VARIABLE oForm AS System.Windows.Forms.Form NO-UNDO .

        DEFINE BUFFER b_eBusinessEntity FOR eBusinessEntity.
        DEFINE BUFFER b_eTable FOR eTable .

        FIND FIRST b_eBusinessEntity .

        ASSIGN lLoading = TRUE .

        THIS-OBJECT:Text = SUBSTITUTE ("&1 <&2>":U,
                                       THIS-OBJECT:Text,
                                       b_eBusinessEntity.BusinessEntityName) .

        ASSIGN cBusinessEntityFile = TRIM (SUBSTITUTE ("&1.&2":U,
                                                       b_eBusinessEntity.BusinessEntityPackage,
                                                       b_eBusinessEntity.BusinessEntityName),
                                           ".":U)
               cDataAccessFile = TRIM (SUBSTITUTE ("&1.&2":U,
                                                   b_eBusinessEntity.DataAccessPackage,
                                                   b_eBusinessEntity.DataAccessName),
                                           ".":U)
               cDatasetControllerFile = TRIM (SUBSTITUTE ("&1.&2":U,
                                                          b_eBusinessEntity.DatasetControllerPackage,
                                                          b_eBusinessEntity.DatasetControllerName),
                                           ".":U)
                                           .

        IF b_eBusinessEntity.GenerateBusinessEntity THEN
            oForm = THIS-OBJECT:OpenBusinessEntityFile (cBusinessEntityFile,    "Business Entity":U,    BusinessEntityPartEnum:BusinessEntity) .
                IF VALID-OBJECT (oForm) THEN oForm:Tag    = "BE":U .

        IF b_eBusinessEntity.GenerateDataAccess THEN
            oForm = THIS-OBJECT:OpenBusinessEntityFile (cDataAccessFile,        "Data Access Object":U, BusinessEntityPartEnum:DataAccess) .
                IF VALID-OBJECT (oForm) THEN oForm:Tag = "DAO":U .

        IF b_eBusinessEntity.GenerateDatasetController THEN
            oForm = THIS-OBJECT:OpenBusinessEntityFile (cDatasetControllerFile, "Dataset Controller":U, BusinessEntityPartEnum:DatasetController) .
                IF VALID-OBJECT (oForm) THEN oForm:Tag = "DC":U .

        ASSIGN cIncludeFile = SUBSTITUTE ("&1/&2.i":U, b_eBusinessEntity.DatasetPath, b_eBusinessEntity.DatasetName) .

        IF FileHelper:Exists (cIncludeFile) THEN
            THIS-OBJECT:OpenFile (cIncludeFile) .

        FOR EACH b_eTable ON ERROR UNDO, THROW:
            ASSIGN cIncludeFile = SUBSTITUTE ("&1/&2.i":U, b_eTable.TempTablePath, b_eTable.TempTableName) .

            IF FileHelper:Exists (cIncludeFile) THEN
                THIS-OBJECT:OpenFile (cIncludeFile) .
        END.

        ASSIGN lLoading = FALSE .

        IF THIS-OBJECT:MdiChildren:Length > 0 THEN
            CAST (THIS-OBJECT:MdiChildren:GetValue(0), System.Windows.Forms.Form):Activate () .

        FINALLY:
            ASSIGN lLoading = FALSE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a file of the business entity
        Notes:
        @param pcClassName The file name to open
        @param pcFileTitle The file title for error messages
        @param poPart The BusinessEntityPartEnum value describing the part to be opened
        @return The reference to the ProcedureEditorForm
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ProcedureEditorForm OpenBusinessEntityFile (pcClassName AS CHARACTER,
                                                                 pcFileTitle AS CHARACTER,
                                                                 poPart AS BusinessEntityPartEnum):

        DEFINE VARIABLE oException AS Consultingwerk.Exceptions.Exception NO-UNDO .
        DEFINE VARIABLE cFileName  AS CHARACTER                           NO-UNDO .

        ASSIGN cFileName = ClassHelper:ClassNameToFile (pcClassName) .

        IF poPart = BusinessEntityPartEnum:DatasetController THEN
            cFileName = BusinessEntityDesignerSettings:SourceCodeBaseFolderGui + "~\":U + cFileName .

        Consultingwerk.Assertion.FileAssert:Exists (cFileName) .

        FILE-INFORMATION:FILE-NAME = cFileName .

        RETURN THIS-OBJECT:OpenFile (FILE-INFORMATION:FULL-PATHNAME) .

        CATCH err AS Progress.Lang.Error:
            oException = NEW Consultingwerk.Exceptions.Exception (err,
                                                                  SUBSTITUTE ("Error opening the &1: &2"{&TRAN}, pcFileTitle, cFileName),
                                                                              0) .

            ErrorHelper:ShowErrorMessage (oException) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the given file
        Notes:
        @param pcFileName The name of the file to open
        @return The reference to the ProcedureEditorForm
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC OVERRIDE ProcedureEditorForm OpenFile (pcFileName AS CHARACTER):

        DEFINE VARIABLE lBefore AS LOGICAL NO-UNDO.

        ASSIGN lBefore  = lLoading
               lLoading = TRUE .

        RETURN SUPER:OpenFile (pcFileName) .

        FINALLY:
            ASSIGN lLoading = lBefore .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The reference to the event that raised this event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:

            WHEN "BE_Client_Callable_Method":U THEN
                THIS-OBJECT:InsertClientCallableMethod () .

            WHEN "DAO_Dataset_Callback":U THEN
                THIS-OBJECT:InsertDatasetCallback () .

            WHEN "DAO_Validation":U THEN
                THIS-OBJECT:InsertValidationHook () .

        END CASE .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

END CLASS.

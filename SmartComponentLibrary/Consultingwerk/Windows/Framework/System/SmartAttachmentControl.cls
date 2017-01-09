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
    File        : SmartAttachmentControl
    Purpose     : Allows access to attachments (add, delete, open, export)
                  based on the SmartAttachment table
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 13 19:05:04 CET 2013
    Notes       : Customers using the SmartAttachmentControl should add
                  a call to the CleanTempFiles methods of the ITempFileManager
                  service after the main WAIT-FOR statement. Attachments 
                  directly opened will be saved in the temp directory 
                  (-T parameter) and will be deleted be the call to the 
                  CleanUp method 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Exceptions.*                        FROM PROPATH .  
USING Consultingwerk.Framework.*                         FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*              FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* . /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY . 
USING Consultingwerk.SmartFramework.System.*             FROM PROPATH . 
USING Consultingwerk.Util.*                              FROM PROPATH .
USING Consultingwerk.Windows.API.*                       FROM PROPATH .
USING Consultingwerk.Windows.Framework.*                 FROM PROPATH .
USING Consultingwerk.Windows.Framework.Controls.*        FROM PROPATH .
USING Infragistics.Win.UltraWinToolbars.*                FROM ASSEMBLY .   
USING Infragistics.Win.UltraWinListView.*                FROM ASSEMBLY . 
USING Progress.Lang.*                                    FROM PROPATH .
USING System.Windows.Forms.*                             FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Framework.System.SmartAttachmentControl 
    INHERITS SmartUserControl: 

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE smartBusinessEntityAdapter1 AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE timer1 AS System.Windows.Forms.Timer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraNavigationBar1 AS Infragistics.Win.Misc.UltraNavigationBar NO-UNDO.

    DEFINE VARIABLE oSmartDataSource   AS ISmartDataSource NO-UNDO . 
    DEFINE VARIABLE oImageProvider     AS IImageProvider   NO-UNDO .
    DEFINE VARIABLE cOptionSetKey      AS CHARACTER        NO-UNDO .

    DEFINE VARIABLE cDownAttachmentKey AS CHARACTER        NO-UNDO INIT ? .
    DEFINE VARIABLE lParentFormClosed  AS LOGICAL          NO-UNDO INIT FALSE .
    DEFINE VARIABLE lLoaded            AS LOGICAL          NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Returns if new Attachments can be added
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CanAddAttachments AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PROTECTED SET(INPUT arg AS LOGICAL):
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager NO-UNDO . 
        
        IF VALID-OBJECT (THIS-OBJECT:ContextualRibbonTabGroup) THEN 
            oToolbarsManager = THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:ToolbarsManager .

        THIS-OBJECT:CanAddAttachments = arg . 

        IF VALID-OBJECT (oToolbarsManager) THEN         
            oToolbarsManager:Tools ["Attachments_Add":U]:SharedProps:Enabled  = arg .                  
    END SET.
     

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Contextual RibbonTabGroup created by this Control
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContextualRibbonTabGroup AS ContextualTabGroup NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartDataSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
                SCL-725 - code moved from LinkDataSource.i directly into class file
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) THEN 
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, Consultingwerk.SmartComponents.Interfaces.ISmartDataSource) .         
    END.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource 
                 can be set to any class implementing the Interface ISmartDataSource.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartDataSource AS ISmartDataSource NO-UNDO 
    GET():
        RETURN oSmartDataSource .        
    END GET.
    SET (INPUT arg AS ISmartDataSource):
        ASSIGN oSmartDataSource = arg . 
        
        THIS-OBJECT:smartBusinessEntityAdapter1:ForeignFields   = "SmartRecordKey,RecordKeyValue":U .
        THIS-OBJECT:smartBusinessEntityAdapter1:SmartDataSource = oSmartDataSource .
    END SET.    

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartAttachmentControl class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartAttachmentControl ():
        
        SUPER().

        InitializeComponent().

        ASSIGN cOptionSetKey = GUID . 

        THIS-OBJECT:smartBusinessEntityAdapter1:AfterRetrieveData:Subscribe (AfterRetrieveDataHandler) .
        THIS-OBJECT:smartBusinessEntityAdapter1:QueryClosed:Subscribe (QueryClosedHandler) .
        
        THIS-OBJECT:Leave:Subscribe (LeaveHandler) .
        
        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
    
        ultraListView1:SubItemColumns:Add ("AttachmentRole":U):Text = "Attachment Role"{&TRAN} . 
        ultraListView1:SubItemColumns:Add ("AttachmentSize":U):Text = "Attachment Size"{&TRAN} . 
    
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds Attachments using a File Open Dialog 
        Notes:   Supports adding multiple files   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddAttachment ():
        
        DEFINE VARIABLE oOpenFileDialog AS OpenFileDialog NO-UNDO .
        DEFINE VARIABLE oDialogResult   AS DialogResult   NO-UNDO . 
        DEFINE VARIABLE oForm           AS Form           NO-UNDO .     
        
        ASSIGN oOpenFileDialog = NEW OpenFileDialog () 
        
               oOpenFileDialog:CheckFileExists  = TRUE
               oOpenFileDialog:CheckPathExists  = TRUE 
               oOpenFileDialog:Filter           = "All files (*.*)|*.*|Documents (*.doc, *.docx, *.xls, *.xlsx, *.ppt, *.pptx, *.pdf)|*.doc;*.docx;*.xls;*.xlsx;*.ppt;*.pptx;*.pdf|":U + 
                                                  "Images (*.png, *.jpg, *.jpeg, *.bmp, *.tif)|*.png;*.jpg;*.jpeg;*.bmp;*.tif":U
               oOpenFileDialog:Multiselect      = TRUE
               oOpenFileDialog:RestoreDirectory = TRUE
               oOpenFileDialog:Title            = "Add attachments"{&TRAN} 
                . 
        
        oForm = THIS-OBJECT:FindForm () . 
        
        WAIT-FOR oOpenFileDialog:ShowDialog (oForm) SET oDialogResult .
         
        {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
        
        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):CreateAttachments (UNBOX (oOpenFileDialog:FileNames),
                                                              ultraNavigationBar1:Text) .        
        

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the AfterRetrieveData event of the Attachment's 
                 Business Entity Adapter
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AfterRetrieveDataHandler (sender AS System.Object,
                                                    e AS System.EventArgs):
        
        THIS-OBJECT:CanAddAttachments = TRUE . 
        
        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):ListView = ultraListView1 .

        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):PopulateListView ("":U) .
              
        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a contextual RibbonTab for the SmartAttachmentControl
        Notes:   
        @param poToolbar The UltraToolbarsManager instance to add the Ribobn Tab to
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID BuildRibbonTab (poToolbar AS UltraToolbarsManager):
        
        DEFINE VARIABLE oContextualTabGroup AS ContextualTabGroup NO-UNDO . 
        DEFINE VARIABLE oRibbonTab          AS RibbonTab          NO-UNDO .
        DEFINE VARIABLE oRibbonGroup        AS RibbonGroup        NO-UNDO .
        DEFINE VARIABLE oTool               AS ButtonTool         NO-UNDO . 
                
        DEFINE VARIABLE cKey                AS CHARACTER          NO-UNDO .
        DEFINE VARIABLE cCaption            AS CHARACTER          NO-UNDO .
        
        ASSIGN cKey     = GUID  /* ensure a group/tab for this Control only */
               cCaption = {Consultingwerk/translate.i &scope="'Attachment':U" &string="'Attachments':U"}
              .
              
        oContextualTabGroup = poToolbar:Ribbon:ContextualTabGroups:Add (cKey) .
        oContextualTabGroup:Caption = cCaption .
        oContextualTabGroup:Visible = FALSE . /* wait for Focus */ 

        THIS-OBJECT:ContextualRibbonTabGroup = oContextualTabGroup . 
        
        oRibbonTab = poToolbar:Ribbon:Tabs:Add (cKey) .
        oRibbonTab:Caption = cCaption .

        oContextualTabGroup:Tabs:Add (oRibbonTab) .

        oRibbonGroup = oRibbonTab:Groups:Add (cKey) .
        oRibbonGroup:Caption = cCaption .

        /* Add button */
        oTool = THIS-OBJECT:CreateRibbonButton (poToolbar,
                                                oRibbonGroup,
                                                "Attachments_Add":U,
                                                "Add Attachment"{&TRAN},
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_add_16.png":U,
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_add_32.png":U) .        
        oTool:SharedProps:Enabled = THIS-OBJECT:CanAddAttachments .                                         
                                                 
        
/*        /* Edit button */                                                                                                          */
/*        oTool = THIS-OBJECT:CreateRibbonButton (poToolbar,                                                                         */
/*                                                oRibbonGroup,                                                                      */
/*                                                "Attachments_Edit":U,                                                              */
/*                                                "Edit Attachment"{&TRAN},                                                          */
/*                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_edit_16.png":U,  */
/*                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_edit_32.png":U) .*/
/*        oTool:SharedProps:Enabled = FALSE .                                                                                        */

        /* Delete button */
        oTool = THIS-OBJECT:CreateRibbonButton (poToolbar,
                                                oRibbonGroup,
                                                "Attachments_Delete":U,
                                                "Remove Attachment"{&TRAN},
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_delete_16.png":U,
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/paperclip_delete_32.png":U) .
        oTool:SharedProps:Enabled = FALSE .                                                  

        /* Open button */
        oTool = THIS-OBJECT:CreateRibbonButton (poToolbar,
                                                oRibbonGroup,
                                                "Attachments_Open":U,
                                                "Open Attachment"{&TRAN},
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/folder_document_16.png":U,
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/folder_document_32.png":U) .
        oTool:SharedProps:Enabled = FALSE .                                         
        oTool:InstanceProps:IsFirstInGroup = TRUE .
                                         
        /* Export button */
        oTool = THIS-OBJECT:CreateRibbonButton (poToolbar,
                                                oRibbonGroup,
                                                "Attachments_Export":U,
                                                "Export Attachment"{&TRAN},
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/export_16.png":U,
                                                "Consultingwerk/Windows/Framework/Images/SmartFramework/export_32.png":U) . 
        oTool:SharedProps:Enabled = FALSE .                                         

        /* View Group */
        ASSIGN cKey     = GUID  /* ensure a group/tab for this Control only */
               cCaption = {Consultingwerk/translate.i &scope="'Attachment':U" &string="'View':U"}
              .

        oRibbonGroup = oRibbonTab:Groups:Add (cKey) .
        oRibbonGroup:Caption = cCaption .

        /* List / Tiles / Icon */
        /* List */
        THIS-OBJECT:CreateRibbonStateButton (poToolbar,
                                             oRibbonGroup,
                                             "View_List":U,
                                             "List"{&TRAN},
                                             cOptionSetKey) . 

        THIS-OBJECT:CreateRibbonStateButton (poToolbar,
                                             oRibbonGroup,
                                             "View_Details":U,
                                             "Details"{&TRAN},
                                             cOptionSetKey) . 

        THIS-OBJECT:CreateRibbonStateButton (poToolbar,
                                             oRibbonGroup,
                                             "View_Icons":U,
                                             "Icons"{&TRAN},
                                             cOptionSetKey) . 

        THIS-OBJECT:CreateRibbonStateButton (poToolbar,
                                             oRibbonGroup,
                                             "View_Tiles":U,
                                             "Tiles"{&TRAN},
                                             cOptionSetKey) . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a Ribbon Button (ButtonTool)
        Notes:   
        @param poToolbar The UltraToolbarsManager instance to add the Ribobn Tab to
        @param poRibbonGroup The RibbonGroup to add the new Ribbon Button to
        @param pcKey The key of the new Ribbon Button
        @param pcLabel The label of the new Ribbon Button
        @param pcImageSmall The key of the small image to be retrieved from the IImageProvider instance
        @param pcImageLarge The key of the large image to be retrieved from the IImageProvider instance 
        @return The reference to the created ButtonTool
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED ButtonTool CreateRibbonButton (poToolbar AS UltraToolbarsManager,
                                                    poRibbonGroup AS RibbonGroup,
                                                    pcKey AS CHARACTER,
                                                    pcLabel AS CHARACTER,
                                                    pcImageSmall AS CHARACTER,
                                                    pcImageLarge AS CHARACTER):
        
        DEFINE VARIABLE oTool AS ButtonTool         NO-UNDO . 
                
        oTool = NEW ButtonTool (pcKey) . 
        poToolbar:Tools:Add (oTool) .
        oTool:SharedProps:Caption = {Consultingwerk/translate.i &scope="'Attachment':U" &string=pcLabel} .
        
        IF VALID-OBJECT (oImageProvider) THEN DO:
            oTool:SharedProps:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (pcImageSmall) .
            oTool:SharedProps:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (pcImageLarge) .
        END. 
        
        oTool = CAST (poRibbonGroup:Tools:AddTool (pcKey),
                      ButtonTool) .         

        oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Large .

        RETURN oTool .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Creates a Ribbon State Button (StateButtonTool)
        Notes:   
        @param poToolbar The UltraToolbarsManager instance to add the Ribobn Tab to
        @param poRibbonGroup The RibbonGroup to add the new Ribbon Button to
        @param pcKey The key of the new Ribbon Button
        @param pcLabel The label of the new Ribbon Button
        @param pcOptionSetKey The key of the option set this belongs to 
        @return The reference to the created StateButtonTool
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED StateButtonTool CreateRibbonStateButton (poToolbar AS UltraToolbarsManager,
                                                              poRibbonGroup AS RibbonGroup,
                                                              pcKey AS CHARACTER,
                                                              pcLabel AS CHARACTER,
                                                              pcOptionSetKey AS CHARACTER):
        
        DEFINE VARIABLE oTool      AS StateButtonTool NO-UNDO . 
        DEFINE VARIABLE oOptionSet AS OptionSet       NO-UNDO . 
                
        oTool = NEW StateButtonTool (pcKey) . 
        poToolbar:Tools:Add (oTool) .
        oTool:SharedProps:Caption = {Consultingwerk/translate.i &scope="'Attachment':U" &string=pcLabel} .
        
        IF poToolbar:OptionSets:Exists (pcOptionSetKey) THEN 
            oTool:OptionSet = poToolbar:OptionSets [pcOptionSetKey] . 
        ELSE DO:
            oOptionSet = NEW OptionSet (pcOptionSetKey) .
            oOptionSet:AllowAllUp = FALSE .
            
            poToolbar:OptionSets:Add (oOptionset) .
             
            oTool:OptionSet = oOptionSet .
        END.
        
        oTool = CAST (poRibbonGroup:Tools:AddTool (pcKey),
                      StateButtonTool) .         

        oTool:InstanceProps:PreferredSizeOnRibbon = RibbonToolSize:Normal .

        RETURN oTool .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deletes the selected attachment 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DeleteAttachment ():

        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO . 
        
        {Consultingwerk/foreach.i UltraListViewItem oItem in ultraListView1:SelectedItems}
            THIS-OBJECT:smartBusinessEntityAdapter1:FindRowWhere 
                    (SUBSTITUTE ("where eSmartAttachment.AttachmentGUID = &1":U, 
                     QUOTER (oItem:Key)),
                     Consultingwerk.SmartComponents.Enum.FindRowWhereModeEnum:FindOnClient) .
            
            THIS-OBJECT:smartBusinessEntityAdapter1:DeleteRow() . 
        END.
        
        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):PopulateListView ("":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Exports the selected attachments to a local folder 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ExportAttachment ():
        
        DEFINE VARIABLE cFileName            AS CHARACTER           NO-UNDO .
        DEFINE VARIABLE cFolderName          AS CHARACTER           NO-UNDO .
        
        DEFINE VARIABLE oDialogResult        AS DialogResult        NO-UNDO . 
        DEFINE VARIABLE oForm                AS Form                NO-UNDO .     
        DEFINE VARIABLE oSaveFileDialog      AS SaveFileDialog      NO-UNDO . 
        DEFINE VARIABLE oFolderBrowserDialog AS FolderBrowserDialog NO-UNDO . 
        
        oForm = THIS-OBJECT:FindForm () . 
        
        CASE ultraListView1:SelectedItems:Count:
            WHEN 0 THEN 
                RETURN . 
                
            WHEN 1 THEN DO: 
                
                /* Save file as for a single file */
                ASSIGN oSaveFileDialog = NEW SaveFileDialog () 
                
                       oSaveFileDialog:CheckPathExists  = TRUE 
                       oSaveFileDialog:OverwritePrompt  = TRUE 
                       oSaveFileDialog:RestoreDirectory = TRUE 
                       oSaveFileDialog:Title            = "Save attachement as"{&TRAN} 
                       oSaveFileDialog:FileName         = ultraListView1:SelectedItems[0]:Text .  

                WAIT-FOR oSaveFileDialog:ShowDialog (oForm) SET oDialogResult .  

                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
                
                ASSIGN cFileName = oSaveFileDialog:FileName .
                
                CAST (smartBusinessEntityAdapter1:DatasetController,
                      AttachmentDatasetController):SaveAttachment (ultraListView1:SelectedItems[0]:Key,
                                                                   cFileName) .
            END . 
            OTHERWISE DO:
                /* Prompt for folder, when multiple files */
                /* Save file as for a single file */
                ASSIGN oFolderBrowserDialog = NEW FolderBrowserDialog () 
                
                       oFolderBrowserDialog:Description = "Save attachments as"{&TRAN}
                       oFolderBrowserDialog:ShowNewFolderButton = TRUE . 

                WAIT-FOR oFolderBrowserDialog:ShowDialog (oForm) SET oDialogResult .  
                
                {Consultingwerk/check-dialogresult-ok.i oDialogResult} .
                
                ASSIGN cFolderName = oFolderBrowserDialog:SelectedPath .
                
                {Consultingwerk/foreach.i UltraListViewItem oItem in ultraListView1:SelectedItems}
                
                    CAST (smartBusinessEntityAdapter1:DatasetController,
                          AttachmentDatasetController):SaveAttachment (oItem:Key,
                                                                       FileHelper:CombinePath(cFolderName, oItem:Text)) .
                END.
            END.

        END CASE . 

    END METHOD .

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.System.SmartAttachmentControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:ultraNavigationBar1 = NEW Infragistics.Win.Misc.UltraNavigationBar().
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        THIS-OBJECT:smartBusinessEntityAdapter1 = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        THIS-OBJECT:timer1 = NEW System.Windows.Forms.Timer(THIS-OBJECT:components).
        CAST(THIS-OBJECT:ultraNavigationBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraNavigationBar1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ultraNavigationBar1, "ultraNavigationBar1":U).
        appearance1:Image = CAST(resources:GetObject("appearance1.Image":U), System.Object).
        THIS-OBJECT:ultraNavigationBar1:LocationSettings:Appearance = appearance1.
        THIS-OBJECT:ultraNavigationBar1:Name = "ultraNavigationBar1":U.
        THIS-OBJECT:ultraNavigationBar1:NonAutoSizeHeight = 24.
        THIS-OBJECT:ultraNavigationBar1:RootLocation:DisplayText = "Root":U.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image":U), System.Object).
        THIS-OBJECT:ultraNavigationBar1:RootLocation:Settings:Appearance = appearance2.
        THIS-OBJECT:ultraNavigationBar1:RootLocation:ToolTipText = "Root Folder for Attachments":U.
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:AllowDrop = TRUE.
        resources:ApplyResources(THIS-OBJECT:ultraListView1, "ultraListView1":U).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Tiles.
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:ultraListView1_ItemSelectionChanged).
        THIS-OBJECT:ultraListView1:DragDrop:Subscribe(THIS-OBJECT:ultraListView1_DragDrop).
        THIS-OBJECT:ultraListView1:DragEnter:Subscribe(THIS-OBJECT:ultraListView1_DragEnter).
        THIS-OBJECT:ultraListView1:MouseDown:Subscribe(THIS-OBJECT:ultraListView1_MouseDown).
        THIS-OBJECT:ultraListView1:MouseMove:Subscribe(THIS-OBJECT:ultraListView1_MouseMove).
        THIS-OBJECT:ultraListView1:MouseUp:Subscribe(THIS-OBJECT:ultraListView1_MouseUp).
        /*  */
        /* smartBusinessEntityAdapter1 */
        /*  */
        THIS-OBJECT:smartBusinessEntityAdapter1:DatasetControllerType = "Consultingwerk.SmartFramework.System.AttachmentDatasetController":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityName = "Consultingwerk.SmartFramework.System.AttachmentBusinessEntity":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:EntityTable = "eSmartAttachment":U.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkCommitSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkDataSource = ?.
        THIS-OBJECT:smartBusinessEntityAdapter1:LinkNavigationSource = ?.
        /*  */
        /* timer1 */
        /*  */
        THIS-OBJECT:timer1:Interval = 750.
        THIS-OBJECT:timer1:Tick:Subscribe(THIS-OBJECT:timer1_Tick).
        /*  */
        /* SmartAttachmentControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraNavigationBar1).
        THIS-OBJECT:Name = "SmartAttachmentControl":U.
        CAST(THIS-OBJECT:ultraNavigationBar1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:smartBusinessEntityAdapter1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Raises the Enter event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnEnter (e AS System.EventArgs):
        
        SUPER:OnEnter (e).

        THIS-OBJECT:ToggleToolbarObjectsVisible (TRUE) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Leave event of the SmartAttachmentControl
        Notes:
        @param sender The refenence to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID LeaveHandler (sender AS System.Object, 
                                        e AS System.EventArgs):

        IF lParentFormClosed = TRUE THEN    
            RETURN . 

        THIS-OBJECT:ToggleToolbarObjectsVisible (FALSE) .

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event
        Notes:   
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager                      NO-UNDO . 
        DEFINE VARIABLE oForm            AS System.Windows.Forms.Form                 NO-UNDO . 
        DEFINE VARIABLE cView            AS CHARACTER                                 NO-UNDO .
        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 

        SUPER:OnLoad (e) .

        /* Mike Fechner, Consultingwerk Ltd. 07.07.2014
           It appears that the UltraTabbedMdiManager may cause the Window to be recreated while 
           it's closed. In this case, don't execute OnLoad a second time. */
        IF lLoaded = TRUE THEN 
            RETURN . 
        ELSE 
            ASSIGN lLoaded = TRUE . 

        oForm = THIS-OBJECT:FindForm () .

        IF NOT VALID-OBJECT (oForm) THEN 
            RETURN . 
            
        IF NOT THIS-OBJECT:DesignTime THEN DO:            
            oToolbarsManager = UltraToolbarsManager:FromForm (oForm) .
    
            IF VALID-OBJECT (oToolbarsManager) THEN DO: 
                THIS-OBJECT:BuildRibbonTab (oToolbarsManager) .
                
                oToolbarsManager:ToolClick:Subscribe (ToolClickHandler) .                
            END.
    
            oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
            
            IF VALID-OBJECT (oSettingsService) THEN DO: 
                cView = oSettingsService:GetSetting ("SmartAttachmentControl":U,
                                                     "View":U) .    
                
                IF cView > "":U THEN 
                    oToolbarsManager:OptionSets [cOptionSetKey]:SelectedTool = 
                        CAST (oToolbarsManager:Tools ["View_":U + cView], StateButtonTool) .
            END.
            
            oForm:FormClosed:Subscribe (ParentFormClosedHandler) .
        END.
        
    END METHOD .  

    /*------------------------------------------------------------------------------
        Purpose: Opens the selected Attachments 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenAttachment ():
        
        DEFINE VARIABLE oListViewItem    AS UltraListViewItem NO-UNDO . 
        DEFINE VARIABLE cFileName        AS CHARACTER         NO-UNDO .
        
        DEFINE VARIABLE oTempFileManager AS ITempFileManager  NO-UNDO . 
        
        ASSIGN oTempFileManager = {Consultingwerk/get-service.i Consultingwerk.Framework.ITempFileManager
                                                                "NEW Consultingwerk.Framework.TempFileManager()"} .
        
        {Consultingwerk/foreach.i UltraListViewItem oItem in ultraListView1:SelectedItems}
    
            ASSIGN cFileName = SESSION:TEMP-DIRECTORY + oItem:Text  .

            oTempFileManager:RegisterTempFile (cFileName) .

            CAST (smartBusinessEntityAdapter1:DatasetController,
                  AttachmentDatasetController):SaveAttachment (oItem:Key,
                                                               cFileName) .
                                                               
            Win32:ShellExecute ("open":U, cFileName) .                                                               
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the FormClosed event of the parent Form
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The FormClosedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParentFormClosedHandler (sender AS System.Object,
                                                   e AS System.Windows.Forms.FormClosedEventArgs):
        
        DEFINE VARIABLE oForm            AS System.Windows.Forms.Form NO-UNDO . 
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager      NO-UNDO . 
        
        THIS-OBJECT:Leave:Unsubscribe (LeaveHandler) .

        ASSIGN oForm = THIS-OBJECT:FindForm () .

        IF VALID-OBJECT (oForm) THEN DO:        
            oToolbarsManager = UltraToolbarsManager:FromForm (oForm) .
    
            IF VALID-OBJECT (oToolbarsManager) THEN DO:
                oToolbarsManager:ToolClick:Unsubscribe (ToolClickHandler) .
            END.
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

        FINALLY:
            ASSIGN lParentFormClosed = TRUE .         
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the QueryClosed event of the smartBusinessEntityAdapter1
                 instance 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID QueryClosedHandler (sender AS System.Object,
                                            e AS System.EventArgs):
        
        THIS-OBJECT:CanAddAttachments = FALSE . 
        
        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):ListView = ultraListView1 .

        CAST (smartBusinessEntityAdapter1:DatasetController,
              AttachmentDatasetController):ClearListView () .
              
        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Tick event of the timer1 component
        Notes:   Starts the Drag and drop operation
        @param sender The reference to the object instance that raised this event
        @param e The reference to the System.EventArgs with the data for this event 
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID timer1_Tick (sender AS System.Object,
                                     e AS System.EventArgs):
        
        ASSIGN timer1:Enabled  = FALSE .
        
        IF cDownAttachmentKey > "":U THEN
            /* Start drag and drop from the DatasetController */
            CAST (smartBusinessEntityAdapter1:DatasetController,
                  AttachmentDatasetController):DragAttachment (cDownAttachmentKey,
                                                               THIS-OBJECT) .
    
        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
        FINALLY:
            ASSIGN cDownAttachmentKey = ? .         
        END FINALLY.  

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Toggles the Visibility of the toolbar objects of this Control
        Notes:   
        @param plVisible Logical value indicating if the Toolbar objects (RibbonTab) should be visible 
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ToggleToolbarObjectsVisible (plVisible AS LOGICAL):
    
        IF lParentFormClosed = TRUE THEN    
            RETURN . 
        
        IF VALID-OBJECT (THIS-OBJECT:ContextualRibbonTabGroup) THEN DO:
            THIS-OBJECT:ContextualRibbonTabGroup:Visible = plVisible . 
    
            IF plVisible THEN DO:
                IF THIS-OBJECT:ContextualRibbonTabGroup:Tabs:Count > 0 THEN 
                    THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:SelectedTab = THIS-OBJECT:ContextualRibbonTabGroup:Tabs[0] .              
            END.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the View of the ListView
        Notes:   
        @param pcView The View: Icon, List, Tiles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToggleView (pcView AS CHARACTER):
        
        DEFINE VARIABLE oSettingsService AS Consultingwerk.Framework.ISettingsService NO-UNDO . 
        
        CASE pcView:
            WHEN "List":U THEN 
                THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List .
        
            WHEN "Details":U THEN 
                THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details .

            WHEN "Icons":U THEN 
                THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Icons .

            WHEN "Tiles":U THEN 
                THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Tiles .
        END CASE .         

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        IF VALID-OBJECT (oSettingsService) THEN 
            oSettingsService:StoreSetting ("SmartAttachmentControl":U,
                                           "View":U,
                                           pcView) .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the UltraToolbarsManager
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ToolClickHandler (sender AS System.Object,
                                          e AS ToolClickEventArgs):
        
        IF e:Tool:Key BEGINS "View_":U THEN DO:
            
            IF VALID-OBJECT (THIS-OBJECT:ContextualRibbonTabGroup) AND 
               VALID-OBJECT (THIS-OBJECT:ContextualRibbonTabGroup:Ribbon) AND 
               THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:ToolbarsManager:OptionSets:Exists (cOptionSetKey) AND  
               VALID-OBJECT (THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:ToolbarsManager:OptionSets [cOptionSetKey]:SelectedTool) THEN 
               
                THIS-OBJECT:ToggleView (ENTRY (2, 
                                               THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:ToolbarsManager:OptionSets [cOptionSetKey]:SelectedTool:Key,
                                               "_":U)) .
            
            
        END .
        ELSE CASE e:Tool:Key:
            
            WHEN "Attachments_Add":U THEN 
                THIS-OBJECT:AddAttachment () .
            
            WHEN "Attachments_Delete":U THEN 
                THIS-OBJECT:DeleteAttachment () .
                
            WHEN "Attachments_Export":U THEN
                THIS-OBJECT:ExportAttachment () . 
                
            WHEN "Attachments_Open":U THEN 
                THIS-OBJECT:OpenAttachment () . 
            
        END CASE . 

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_DragDrop (sender AS System.Object, 
                                                 e AS System.Windows.Forms.DragEventArgs):
        
        DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO EXTENT .

        IF DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            ASSIGN cFileNames = DragAndDropHelper:GetDropFileNames (e:Data) .

            CAST (smartBusinessEntityAdapter1:DatasetController,
                  AttachmentDatasetController):CreateAttachments (cFileNames,
                                                                  ultraNavigationBar1:Text) .
        END.

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragEnter event of the ListView control
        Notes:   
        @param sender The reference to the sending Control
        @param e The DragEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_DragEnter (sender AS System.Object, 
                                                  e AS System.Windows.Forms.DragEventArgs):
        
        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN 
            e:Effect = System.Windows.Forms.DragDropEffects:Link .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemSelectionChanged event of the UltraListView1
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The ItemSelectionChangedEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_ItemSelectionChanged (sender AS System.Object,
                                                             e AS Infragistics.Win.UltraWinListView.ItemSelectionChangedEventArgs):
        
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager NO-UNDO . 
        DEFINE VARIABLE lEnabled         AS LOGICAL              NO-UNDO.
        
        oToolbarsManager = THIS-OBJECT:ContextualRibbonTabGroup:Ribbon:ToolbarsManager .
        
        IF e:SelectedItems:Count > 0 THEN 
            lEnabled = TRUE .
        ELSE 
            lEnabled = FALSE . 
        
          ASSIGN 
/*                   oToolbarsManager:Tools ["Attachments_Add":U]:SharedProps:Enabled = lEnabled */
/*                 oToolbarsManager:Tools ["Attachments_Edit":U]:SharedProps:Enabled = lEnabled */
                 oToolbarsManager:Tools ["Attachments_Delete":U]:SharedProps:Enabled = lEnabled 
                 oToolbarsManager:Tools ["Attachments_Open":U]:SharedProps:Enabled = lEnabled 
                 oToolbarsManager:Tools ["Attachments_Export":U]:SharedProps:Enabled = lEnabled .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseDown even of the ultraListView1                                                                       
        Notes:                                                              
        @param sender The object that raised the MouseDown event
        @param e The System.Windows.Forms.MouseEventArgs with the data for this event          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseDown (sender AS System.Object,
                                                  e AS System.Windows.Forms.MouseEventArgs):
        
        DEFINE VARIABLE oListViewItem AS UltraListViewItem NO-UNDO . 
        
        oListViewItem = ultraListView1:ItemFromPoint (e:Location) .
        
        IF VALID-OBJECT (oListViewItem) THEN
            ASSIGN cDownAttachmentKey = oListViewItem:Key 
                   timer1:Enabled     = TRUE .              

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseMove event of the ultarListView1 control
        Notes:   Cancels a pending drag and drop operation 
        @param sender The reference to the object that raised the event
        @param e The reference to the MouseEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseMove (sender AS System.Object, 
                                                  e AS System.Windows.Forms.MouseEventArgs):
        
        ASSIGN timer1:Enabled     = FALSE
               cDownAttachmentKey = ? .  

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the MouseUp event of the ultarListView1 control
        Notes:   Cancels a pending drag and drop operation 
        @param sender The reference to the object that raised the event
        @param e The reference to the MouseEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_MouseUp (sender AS System.Object, 
                                                e AS System.Windows.Forms.MouseEventArgs):
        
        ASSIGN timer1:Enabled     = FALSE
               cDownAttachmentKey = ? .  

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartAttachmentControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartAttachmentControl ():

    END DESTRUCTOR .

END CLASS.

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
    File        : PasteSourceCodeForm
    Purpose     : Provides an user interface to paste ABL source code from the
                  clipboard 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 20 22:25:58 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.BaseForm.

USING Consultingwerk.BusinessEntityDesigner.UI.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.UI.PasteSourceCodeForm INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTextEditor1 AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabPageControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton2 AS Infragistics.Win.Misc.UltraButton NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraButton1 AS Infragistics.Win.Misc.UltraButton NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ABL source code as a LONGCHAR value 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY CodeBlock AS LONGCHAR NO-UNDO 
    GET:
        RETURN ultraTextEditor1:Text .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PasteSourceCodeForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PasteSourceCodeForm ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
        ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
        THIS-OBJECT:ultraTabPageControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
        THIS-OBJECT:ultraTextEditor1 = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraButton2 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraButton1 = NEW Infragistics.Win.Misc.UltraButton().
        THIS-OBJECT:ultraTabControl1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
        THIS-OBJECT:ultraTabPageControl1:SuspendLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:ultraTabControl1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraTabPageControl1 */
        /*  */
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraTextEditor1).
        THIS-OBJECT:ultraTabPageControl1:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:ultraTabPageControl1:Location = NEW System.Drawing.Point(1, 23).
        THIS-OBJECT:ultraTabPageControl1:Name = "ultraTabPageControl1":U.
        THIS-OBJECT:ultraTabPageControl1:Size = NEW System.Drawing.Size(573, 304).
        /*  */
        /* ultraTextEditor1 */
        /*  */
        THIS-OBJECT:ultraTextEditor1:AcceptsReturn = TRUE.
        THIS-OBJECT:ultraTextEditor1:AcceptsTab = TRUE.
        THIS-OBJECT:ultraTextEditor1:AllowDrop = TRUE.
        THIS-OBJECT:ultraTextEditor1:AlwaysInEditMode = TRUE.
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTextEditor1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        appearance2:FontData:BoldAsString = "True":U.
        appearance2:FontData:Name = "Courier New":U.
        appearance2:FontData:SizeInPoints = 10.
        THIS-OBJECT:ultraTextEditor1:Appearance = appearance2.
        THIS-OBJECT:ultraTextEditor1:AutoSize = FALSE.
        THIS-OBJECT:ultraTextEditor1:Font = NEW System.Drawing.Font("Courier New":U, 10, System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:ultraTextEditor1:HideSelection = FALSE.
        THIS-OBJECT:ultraTextEditor1:Location = NEW System.Drawing.Point(3, 28).
        THIS-OBJECT:ultraTextEditor1:MaxLength = 210000000.
        THIS-OBJECT:ultraTextEditor1:Multiline = TRUE.
        THIS-OBJECT:ultraTextEditor1:Name = "ultraTextEditor1":U.
        THIS-OBJECT:ultraTextEditor1:Scrollbars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:ultraTextEditor1:Size = NEW System.Drawing.Size(567, 273).
        THIS-OBJECT:ultraTextEditor1:TabIndex = 1.
        THIS-OBJECT:ultraTextEditor1:UseAppStyling = FALSE.
        THIS-OBJECT:ultraTextEditor1:WordWrap = FALSE.
        THIS-OBJECT:ultraTextEditor1:TextChanged:Subscribe(THIS-OBJECT:ultraTextEditor1_TextChanged).
        THIS-OBJECT:ultraTextEditor1:DragDrop:Subscribe(THIS-OBJECT:ultraTextEditor1_DragDrop).
        THIS-OBJECT:ultraTextEditor1:DragEnter:Subscribe(THIS-OBJECT:ultraTextEditor1_DragEnter).
        THIS-OBJECT:ultraTextEditor1:KeyDown:Subscribe(THIS-OBJECT:ultraTextEditor1_KeyDown).
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance1:BackColorAlpha = Infragistics.Win.Alpha:Transparent.
        THIS-OBJECT:ultraLabel1:Appearance = appearance1.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(3, 8).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(475, 14).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Paste or Drop the source here that contains the temp-tables you wish to import into the model:":U.
        /*  */
        /* ultraButton2 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton2:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton2:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ultraButton2:Location = NEW System.Drawing.Point(514, 348).
        THIS-OBJECT:ultraButton2:Name = "ultraButton2":U.
        THIS-OBJECT:ultraButton2:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton2:TabIndex = 5.
        THIS-OBJECT:ultraButton2:Text = "&Cancel":U.
        /*  */
        /* ultraButton1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:ultraButton1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraButton1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ultraButton1:Enabled = FALSE.
        THIS-OBJECT:ultraButton1:Location = NEW System.Drawing.Point(433, 348).
        THIS-OBJECT:ultraButton1:Name = "ultraButton1":U.
        THIS-OBJECT:ultraButton1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:ultraButton1:TabIndex = 4.
        THIS-OBJECT:ultraButton1:Text = "&OK":U.
        /*  */
        /* ultraTabControl1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl1).
        THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1":U.
        THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
        THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(577, 330).
        THIS-OBJECT:ultraTabControl1:TabIndex = 3.
        ultraTab1:TabPage = THIS-OBJECT:ultraTabPageControl1.
        ultraTab1:Text = "Paste ABL Source Code":U.
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
        arrayvar0[1] = ultraTab1.
        THIS-OBJECT:ultraTabControl1:Tabs:AddRange(arrayvar0).
        /*  */
        /* ultraTabSharedControlsPage1 */
        /*  */
        THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
        THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1":U.
        THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(573, 304).
        /*  */
        /* PasteSourceCodeForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ultraButton1.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ultraButton2.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(601, 383).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraButton1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:Name = "PasteSourceCodeForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:Text = "Paste ABL Source Code into model":U.
        THIS-OBJECT:WindowPositionRegistryKey = "PasteSourceCodeForm":U.
        THIS-OBJECT:ultraTabPageControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ultraTabPageControl1:PerformLayout().
        CAST(THIS-OBJECT:ultraTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ultraTabControl1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragDrop event of the ultraTextEditor1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_DragDrop (sender AS System.Object, 
                                                   e AS System.Windows.Forms.DragEventArgs):

        DEFINE VARIABLE lcText AS LONGCHAR  NO-UNDO.
         DEFINE VARIABLE cFiles AS CHARACTER NO-UNDO EXTENT .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) THEN DO:
            cFiles = Consultingwerk.Util.DragAndDropHelper:GetDropFileNames (e:Data) .
           
            DO i = 1 TO EXTENT (cFiles):
                COPY-LOB FROM FILE (cFiles[i]) TO lcText . 
            
                ultraTextEditor1:AppendText (lcText) .
            END.
        END .
        ELSE DO:
            lcText = UNBOX (e:Data:GetData (System.Windows.Forms.DataFormats:Text)) .
           
            ultraTextEditor1:AppendText (lcText) .
        END.
        
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the DragEnter event of the ultraTextEditor1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The DragEventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_DragEnter (sender AS System.Object, 
                                                    e AS System.Windows.Forms.DragEventArgs):
        
        IF Consultingwerk.Util.DragAndDropHelper:IsFileDrop (e:Data) OR 
           Consultingwerk.Util.DragAndDropHelper:IsTextDrop (e:Data) THEN 

            e:Effect = System.Windows.Forms.DragDropEffects:Copy .                
       
       CATCH err AS Progress.Lang.Error :
              Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
       END CATCH.
                
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDwown event of the ultraTextEditor1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_KeyDown (sender AS System.Object, 
                                                  e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual(e:KeyCode,
                                             System.Windows.Forms.Keys:A) AND 
           e:Control THEN 
              
              ultraTextEditor1:SelectAll () . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the ultraTextEditor1 instance                                                                        
        Notes:                                              
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                          
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraTextEditor1_TextChanged (sender AS System.Object, 
                                                      e AS System.EventArgs):
        
        ultraButton1:Enabled = ultraTextEditor1:TextLength > 0 . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the PasteSourceCodeForm class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC PasteSourceCodeForm ():

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.

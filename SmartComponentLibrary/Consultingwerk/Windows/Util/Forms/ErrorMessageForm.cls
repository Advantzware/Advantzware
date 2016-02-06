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
    File        : ErrorMessageForm
    Purpose     : Default Error Message Form
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 16 16:01:51 CEST 2009
    Notes       : Implements Consultingwerk.Util.Forms.IErrorMessageForm 
                  and ISupportsInnerExceptionDisplay
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH.
USING Consultingwerk.Util.Forms.*         FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH . 
USING Progress.Lang.*                     FROM ASSEMBLY .
USING Progress.Windows.Form               FROM ASSEMBLY .
USING System.Windows.Forms.*              FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Util.Forms.ErrorMessageForm INHERITS Form 
    IMPLEMENTS IErrorMessageForm, ISupportsInnerExceptionDisplay, ISupportsExceptionDisplay: 
    
    DEFINE PRIVATE VARIABLE btnInnerException AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnProperties AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btnSessionInfo AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE copyImageToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE copyTextToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE contextMenuStrip1 AS System.Windows.Forms.ContextMenuStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE detailsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE groupBox1 AS System.Windows.Forms.GroupBox NO-UNDO.
    DEFINE PRIVATE VARIABLE textBox2 AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.
    DEFINE PRIVATE VARIABLE btnStack AS System.Windows.Forms.Button NO-UNDO.

    DEFINE PRIVATE VARIABLE lDisplayStack AS LOGICAL             NO-UNDO INIT TRUE .
    DEFINE PRIVATE VARIABLE oPreviousSize AS System.Drawing.Size NO-UNDO .
            
    DEFINE VARIABLE oFullWindowDragManager          AS FullWindowDragManager           NO-UNDO EXTENT 3 .              
    DEFINE VARIABLE oErrorMessageFormButtonProvider AS IErrorMessageFormButtonProvider NO-UNDO .            
                
    /*------------------------------------------------------------------------------
        Purpose: Get's and sets if the stack trace of the error should be opened or not                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY DisplayStack AS LOGICAL NO-UNDO  
    GET:
        RETURN lDisplayStack .
    END.
    SET (arg AS LOGICAL):
        /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
           Save size, when closing Stack trace */        
        IF arg = FALSE AND lDisplayStack = TRUE THEN 
            ASSIGN oPreviousSize = NEW System.Drawing.Size (THIS-OBJECT:Size:Width,
                                                            THIS-OBJECT:Size:Height) .        
        
        ASSIGN lDisplayStack = arg . 
        
        IF arg = FALSE THEN DO: 
            ASSIGN THIS-OBJECT:FormBorderStyle = FormBorderStyle:FixedDialog 
                   THIS-OBJECT:SizeGripStyle   = SizeGripStyle:HIDE
                   THIS-OBJECT:Size            = THIS-OBJECT:MinimumSize .
        END. 
        ELSE DO: 
            ASSIGN THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:Sizable 
                   THIS-OBJECT:SizeGripStyle   = SizeGripStyle:Show 
                   THIS-OBJECT:Size            = oPreviousSize.
        END.  
    END.    
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the refernece to the error object 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Exception AS Progress.Lang.Error NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the inner error object                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY InnerException AS Progress.Lang.Error NO-UNDO 
    GET.
    SET.     
    
    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the error message text                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageText AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:textBox1:Text.
    END GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:textBox1:Text = REPLACE(arg, CHR(10), CHR(13) + CHR(10)) .
    END SET.    
    
    /*------------------------------------------------------------------------------
        Purpose: Get's and sets the error stack trace text                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY StackTraceText AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:textBox2:Text .
    END GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:textBox2:Text = REPLACE(arg, CHR(10), CHR(13) + CHR(10)) .
    END SET.
    
    DEFINE PRIVATE VARIABLE toolStripSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolTip1 AS System.Windows.Forms.ToolTip NO-UNDO.

    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ErrorMessageForm class                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
     CONSTRUCTOR PUBLIC ErrorMessageForm ():
                
        SUPER().
        InitializeComponent().

        ASSIGN oPreviousSize = NEW System.Drawing.Size (THIS-OBJECT:Size:Width,
                                                        THIS-OBJECT:Size:Height) .        

        ASSIGN oFullWindowDragManager[1] = NEW FullWindowDragManager (THIS-OBJECT) 
               oFullWindowDragManager[2] = NEW FullWindowDragManager (THIS-OBJECT:panel1) 
               oFullWindowDragManager[3] = NEW FullWindowDragManager (THIS-OBJECT:pictureBox1) .
        
        oErrorMessageFormButtonProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Util.Forms.IErrorMessageFormButtonProvider} .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event of the btnInnerException                                                                         
        Notes:                                                             
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnInnerException_Click (sender AS System.Object, 
                                                 e AS System.EventArgs):
        
        IF VALID-OBJECT (THIS-OBJECT:InnerException) THEN 
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (THIS-OBJECT:InnerException) .

    END METHOD.

	/*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnProperties
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnProperties_Click (sender AS System.Object, e AS System.EventArgs ):
		
		&IF DEFINED (AblReflection) NE 0 &THEN
		ProgressLangObjectPropertiesForm:DisplayPropertiesOfObject (THIS-OBJECT:Exception) .
		&ENDIF

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnSessionInfo
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID btnSessionInfo_Click (sender AS System.Object, 
	                                          e AS System.EventArgs):
		
		Consultingwerk.Windows.Session.SessionInfoForm:ShowSessionInformation() .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event of the btnInnerException                                                                         
        Notes:   Toggles display of the stack trace                                                                        
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnStack_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
        
        THIS-OBJECT:DisplayStack = NOT THIS-OBJECT:DisplayStack .
        
        THIS-OBJECT:ToggleButtonState (THIS-OBJECT:DisplayStack) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Copy's the message dialog as an image to the clipboard
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyImage ():
        
        DEFINE VARIABLE oBitmap AS System.Drawing.Bitmap NO-UNDO .
        
        oBitmap = NEW System.Drawing.Bitmap (THIS-OBJECT:Width, THIS-OBJECT:Height) .
        
        THIS-OBJECT:DrawToBitmap (oBitmap, NEW System.Drawing.Rectangle (0, 0, THIS-OBJECT:Width, THIS-OBJECT:Height)) .

        System.Windows.Forms.Clipboard:SetImage (oBitmap) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Copies the Dialog Box contents as plain text 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyText ():
        
        IF THIS-OBJECT:DisplayStack THEN  
            System.Windows.Forms.Clipboard:SetText (SUBSTITUTE ("&1&2&2Call Stack:&2&3&2&2":U, 
                                                                THIS-OBJECT:MessageText,
                                                                System.Environment:NewLine,
                                                                THIS-OBJECT:StackTraceText)) . 
        ELSE 
            System.Windows.Forms.Clipboard:SetText (SUBSTITUTE ("&1&2":U, 
                                                                THIS-OBJECT:MessageText,
                                                                System.Environment:NewLine)) . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the click event of the copyTextToolStripMenuItem 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID copyTextToolStripMenuItem_Click (sender AS System.Object, 
                                                         e AS System.EventArgs):
        
        THIS-OBJECT:CopyText () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler for the click event of the copyImageToolStripMenuItem 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event           
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID copyImageToolStripMenuItem_Click (INPUT sender AS System.Object, INPUT e AS System.EventArgs):
        
        THIS-OBJECT:CopyImage () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event of custom buttons 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data to this event
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID CustomButtonClickHandler (sender AS System.Object,
	                                                e AS System.EventArgs):
		
		IF oErrorMessageFormButtonProvider:HandleButtonClick (CAST (sender, System.Windows.Forms.Button), 
		                                                      THIS-OBJECT:Exception) = TRUE THEN 
            THIS-OBJECT:Close () .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the detailsToolStripMenuItem  
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID detailsToolStripMenuItem_Click (sender AS System.Object, 
                                                        e AS System.EventArgs):
		
		DEFINE VARIABLE oForm AS ErrorMessageDetailForm NO-UNDO . 
		
		oForm = NEW ErrorMessageDetailForm () . 
		
		oForm:DisplayErrorDetails (CAST (THIS-OBJECT:Exception, 
		                                 Progress.Lang.SysError)) . 

        CATCH err AS Progress.Lang.Error :
        	Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Visual Design                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeComponent ():
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ErrorMessageForm":U).
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:groupBox1 = NEW System.Windows.Forms.GroupBox().
        THIS-OBJECT:textBox2 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:btnStack = NEW System.Windows.Forms.Button().
        THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
        THIS-OBJECT:toolTip1 = NEW System.Windows.Forms.ToolTip(THIS-OBJECT:components).
        THIS-OBJECT:btnSessionInfo = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btnProperties = NEW System.Windows.Forms.Button().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:btnInnerException = NEW System.Windows.Forms.Button().
        THIS-OBJECT:contextMenuStrip1 = NEW System.Windows.Forms.ContextMenuStrip(THIS-OBJECT:components).
        THIS-OBJECT:copyTextToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:copyImageToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        THIS-OBJECT:toolStripSeparator1 = NEW System.Windows.Forms.ToolStripSeparator().
        THIS-OBJECT:detailsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:groupBox1:SuspendLayout().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:contextMenuStrip1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(474, 226).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 0.
        THIS-OBJECT:button1:Text = "&Ok":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(12, 12).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(32, 32).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:AutoSize.
        THIS-OBJECT:pictureBox1:TabIndex = 1.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* textBox1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:textBox1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox1:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:textBox1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(50, 12).
        THIS-OBJECT:textBox1:Multiline = TRUE.
        THIS-OBJECT:textBox1:Name = "textBox1":U.
        THIS-OBJECT:textBox1:ReadOnly = TRUE.
        THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(499, 72).
        THIS-OBJECT:textBox1:TabIndex = 2.
        /*  */
        /* groupBox1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:groupBox1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:groupBox1:Controls:Add(THIS-OBJECT:textBox2).
        THIS-OBJECT:groupBox1:Location = NEW System.Drawing.Point(12, 91).
        THIS-OBJECT:groupBox1:Name = "groupBox1":U.
        THIS-OBJECT:groupBox1:Size = NEW System.Drawing.Size(536, 111).
        THIS-OBJECT:groupBox1:TabIndex = 3.
        THIS-OBJECT:groupBox1:TabStop = FALSE.
        THIS-OBJECT:groupBox1:Text = "Call Stack:":U.
        THIS-OBJECT:groupBox1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* textBox2 */
        /*  */
        THIS-OBJECT:textBox2:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:textBox2:BorderStyle = System.Windows.Forms.BorderStyle:None.
        THIS-OBJECT:textBox2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:textBox2:Location = NEW System.Drawing.Point(3, 16).
        THIS-OBJECT:textBox2:Multiline = TRUE.
        THIS-OBJECT:textBox2:Name = "textBox2":U.
        THIS-OBJECT:textBox2:ReadOnly = TRUE.
        THIS-OBJECT:textBox2:ScrollBars = System.Windows.Forms.ScrollBars:Both.
        THIS-OBJECT:textBox2:Size = NEW System.Drawing.Size(530, 92).
        THIS-OBJECT:textBox2:TabIndex = 3.
        THIS-OBJECT:textBox2:WordWrap = FALSE.
        /*  */
        /* btnStack */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnStack:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnStack:Location = NEW System.Drawing.Point(12, 225).
        THIS-OBJECT:btnStack:Name = "btnStack":U.
        THIS-OBJECT:btnStack:Size = NEW System.Drawing.Size(24, 24).
        THIS-OBJECT:btnStack:TabIndex = 4.
        THIS-OBJECT:toolTip1:SetToolTip(THIS-OBJECT:btnStack, "Toggles the display of call stack information":U).
        THIS-OBJECT:btnStack:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnStack:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnStack:Click:Subscribe(THIS-OBJECT:btnStack_Click).
        /*  */
        /* imageList1 */
        /*  */
        THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream":U), System.Windows.Forms.ImageListStreamer).
        THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:imageList1:Images:SetKeyName(0, "arrow-up_16.gif":U).
        THIS-OBJECT:imageList1:Images:SetKeyName(1, "arrow-down_16.gif":U).
        /*  */
        /* btnSessionInfo */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnSessionInfo:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnSessionInfo:Image = CAST(resources:GetObject("btnSessionInfo.Image":U), System.Drawing.Image).
        THIS-OBJECT:btnSessionInfo:Location = NEW System.Drawing.Point(153, 225).
        THIS-OBJECT:btnSessionInfo:Name = "btnSessionInfo":U.
        THIS-OBJECT:btnSessionInfo:Size = NEW System.Drawing.Size(24, 24).
        THIS-OBJECT:btnSessionInfo:TabIndex = 7.
        THIS-OBJECT:toolTip1:SetToolTip(THIS-OBJECT:btnSessionInfo, "Shows Information about the OpenEdge session":U).
        THIS-OBJECT:btnSessionInfo:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnSessionInfo:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnSessionInfo:Click:Subscribe(THIS-OBJECT:btnSessionInfo_Click).
        /*  */
        /* btnProperties */
        /*  */
        DEFINE VARIABLE nestedvar5 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar5 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnProperties:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar5, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnProperties:Image = CAST(resources:GetObject("btnProperties.Image":U), System.Drawing.Image).
        THIS-OBJECT:btnProperties:Location = NEW System.Drawing.Point(42, 225).
        THIS-OBJECT:btnProperties:Name = "btnProperties":U.
        THIS-OBJECT:btnProperties:Size = NEW System.Drawing.Size(24, 24).
        THIS-OBJECT:btnProperties:TabIndex = 8.
        THIS-OBJECT:toolTip1:SetToolTip(THIS-OBJECT:btnProperties, "Shows Information about the OpenEdge session":U).
        THIS-OBJECT:btnProperties:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnProperties:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnProperties:Click:Subscribe(THIS-OBJECT:btnProperties_Click).
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar6 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar6 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar6, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:groupBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(561, 213).
        THIS-OBJECT:panel1:TabIndex = 5.
        /*  */
        /* btnInnerException */
        /*  */
        DEFINE VARIABLE nestedvar7 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar7 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnInnerException:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar7, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnInnerException:Location = NEW System.Drawing.Point(72, 225).
        THIS-OBJECT:btnInnerException:Name = "btnInnerException":U.
        THIS-OBJECT:btnInnerException:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:btnInnerException:TabIndex = 6.
        THIS-OBJECT:btnInnerException:Text = "More":U.
        THIS-OBJECT:btnInnerException:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnInnerException:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnInnerException:Click:Subscribe(THIS-OBJECT:btnInnerException_Click).
        /*  */
        /* contextMenuStrip1 */
        /*  */
        @VisualDesigner.FormMember (NeedsInitialize="false":U, InitializeArray="true":U).
        DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 4 NO-UNDO.
        arrayvar0[1] = THIS-OBJECT:copyTextToolStripMenuItem.
        arrayvar0[2] = THIS-OBJECT:copyImageToolStripMenuItem.
        arrayvar0[3] = THIS-OBJECT:toolStripSeparator1.
        arrayvar0[4] = THIS-OBJECT:detailsToolStripMenuItem.
        THIS-OBJECT:contextMenuStrip1:Items:AddRange(arrayvar0).
        THIS-OBJECT:contextMenuStrip1:Name = "contextMenuStrip1":U.
        THIS-OBJECT:contextMenuStrip1:Size = NEW System.Drawing.Size(178, 76).
        /*  */
        /* copyTextToolStripMenuItem */
        /*  */
        THIS-OBJECT:copyTextToolStripMenuItem:Image = CAST(resources:GetObject("copyTextToolStripMenuItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:copyTextToolStripMenuItem:Name = "copyTextToolStripMenuItem":U.
        DEFINE VARIABLE nestedvar8 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar8 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:copyTextToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar8, System.Windows.Forms.Keys:C), System.Windows.Forms.Keys).
        THIS-OBJECT:copyTextToolStripMenuItem:Size = NEW System.Drawing.Size(177, 22).
        THIS-OBJECT:copyTextToolStripMenuItem:Text = "&Copy Text":U.
        THIS-OBJECT:copyTextToolStripMenuItem:Click:Subscribe(THIS-OBJECT:copyTextToolStripMenuItem_Click).
        /*  */
        /* copyImageToolStripMenuItem */
        /*  */
        THIS-OBJECT:copyImageToolStripMenuItem:Image = CAST(resources:GetObject("copyImageToolStripMenuItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:copyImageToolStripMenuItem:Name = "copyImageToolStripMenuItem":U.
        DEFINE VARIABLE nestedvar9 AS System.Windows.Forms.Keys NO-UNDO.
        nestedvar9 = System.Windows.Forms.Keys:Control.
        THIS-OBJECT:copyImageToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar9, System.Windows.Forms.Keys:I), System.Windows.Forms.Keys).
        THIS-OBJECT:copyImageToolStripMenuItem:Size = NEW System.Drawing.Size(177, 22).
        THIS-OBJECT:copyImageToolStripMenuItem:Text = "Copy &Image":U.
        THIS-OBJECT:copyImageToolStripMenuItem:Click:Subscribe(THIS-OBJECT:copyImageToolStripMenuItem_Click).
        /*  */
        /* toolStripSeparator1 */
        /*  */
        THIS-OBJECT:toolStripSeparator1:Name = "toolStripSeparator1":U.
        THIS-OBJECT:toolStripSeparator1:Size = NEW System.Drawing.Size(174, 6).
        /*  */
        /* detailsToolStripMenuItem */
        /*  */
        THIS-OBJECT:detailsToolStripMenuItem:Image = CAST(resources:GetObject("detailsToolStripMenuItem.Image":U), System.Drawing.Image).
        THIS-OBJECT:detailsToolStripMenuItem:Name = "detailsToolStripMenuItem":U.
        THIS-OBJECT:detailsToolStripMenuItem:Size = NEW System.Drawing.Size(177, 22).
        THIS-OBJECT:detailsToolStripMenuItem:Text = "Details":U.
        THIS-OBJECT:detailsToolStripMenuItem:Click:Subscribe(THIS-OBJECT:detailsToolStripMenuItem_Click).
        /*  */
        /* ErrorMessageForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:button1.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(561, 261).
        THIS-OBJECT:ContextMenuStrip = THIS-OBJECT:contextMenuStrip1.
        THIS-OBJECT:ControlBox = FALSE.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnProperties).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnSessionInfo).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnInnerException).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnStack).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:KeyPreview = TRUE.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(567, 161).
        THIS-OBJECT:Name = "ErrorMessageForm":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
        THIS-OBJECT:Text = "Message Form":U.
        THIS-OBJECT:KeyDown:Subscribe(THIS-OBJECT:KeyDownHandler).
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:groupBox1:ResumeLayout(FALSE).
        THIS-OBJECT:groupBox1:PerformLayout().
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:panel1:PerformLayout().
        THIS-OBJECT:contextMenuStrip1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the KeyDown event of the MessageForm
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID KeyDownHandler (sender AS System.Object, e AS System.Windows.Forms.KeyEventArgs):
        
        IF Progress.Util.EnumHelper:AreEqual (e:KeyCode,
                                              System.Windows.Forms.Keys:Escape) THEN 
            THIS-OBJECT:Close () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event                                                                       
        Notes:      
        @param e An EventArgs that contains the event data                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
        
        DEFINE VARIABLE oTextSize    AS System.Drawing.Size NO-UNDO . 
        DEFINE VARIABLE iDeltaHeight AS INTEGER             NO-UNDO .
        DEFINE VARIABLE iIndex       AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cMessageText AS CHARACTER           NO-UNDO .
        
        ASSIGN iIndex = INDEX (THIS-OBJECT:MessageText, "Backend-Stacktrace:":U) .

        IF iIndex > 0 THEN
            ASSIGN cMessageText               = REPLACE (THIS-OBJECT:MessageText, CHR(13) + CHR (10), CHR(10))
                   iIndex                     = INDEX (cMessageText, "Backend-Stacktrace:":U) 
                   THIS-OBJECT:StackTraceText = SUBSTITUTE ("&1~n~n&2":U,
                                                            SUBSTRING (cMessageText, iIndex),
                                                            THIS-OBJECT:StackTraceText,
                                                            CHR(13) + CHR(10))
                   THIS-OBJECT:MessageText    = TRIM (SUBSTRING (cMessageText, 1, iIndex - 1),
                                                      CHR (13) + CHR (10)).
                        
        IF THIS-OBJECT:StackTraceText = "Stacktrace not provided (-errorstack missing).":U THEN 
            THIS-OBJECT:DisplayStack = FALSE . 
        ELSE 
            THIS-OBJECT:DisplayStack = FrameworkSettings:DebugMode . 

        THIS-OBJECT:ToggleButtonState (THIS-OBJECT:DisplayStack) .

        oTextSize = System.Windows.Forms.TextRenderer:MeasureText (THIS-OBJECT:MessageText,
                                                                   THIS-OBJECT:textBox1:Font,
                                                                   NEW System.Drawing.Size (THIS-OBJECT:textBox1:Width,
                                                                                            THIS-OBJECT:textBox1:Height),
                                                                   System.Windows.Forms.TextFormatFlags:WordBreak) .

        ASSIGN iDeltaHeight = oTextSize:Height - THIS-OBJECT:textBox1:Height .

        IF iDeltaHeight > 0 THEN DO:
             IF THIS-OBJECT:HEIGHT + iDeltaHeight > SESSION:HEIGHT-PIXELS * .8 THEN
                ASSIGN iDeltaHeight = SESSION:HEIGHT-PIXELS * .8 - THIS-OBJECT:HEIGHT.

             IF iDeltaHeight <= 0 THEN
                iDeltaHeight = 0 .
        END.
        ELSE
            iDeltaHeight = 0  .

        THIS-OBJECT:Size = NEW System.Drawing.Size

                (MINIMUM (INTEGER (SESSION:WIDTH-PIXELS * .8),
                          oTextSize:Width + (THIS-OBJECT:Width - THIS-OBJECT:textBox1:Width)),
                 THIS-OBJECT:Height + iDeltaHeight) .

        groupBox1:Top = groupBox1:Top + iDeltaHeight .
        groupBox1:Height = groupBox1:HEIGHT - iDeltaHeight .
        textBox1:Height = textBox1:HEIGHT + iDeltaHeight .

        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size (THIS-OBJECT:Width, THIS-OBJECT:MinimumSize:Height + iDeltaHeight) .
        
        SUPER:OnLoad (e) .

        THIS-OBJECT:btnInnerException:Visible = VALID-OBJECT (THIS-OBJECT:InnerException) .

&IF DEFINED (AblReflection) EQ 0 &THEN
        DEFINE VARIABLE iDelta AS INTEGER NO-UNDO .
        
        iDelta = btnProperties:Left - THIS-OBJECT:btnInnerException:Left .
        
        ASSIGN btnInnerException:Left = btnInnerException:Left + iDelta
               btnSessionInfo:Left    = btnSessionInfo:LEFT + iDelta 
               btnProperties:Visible  = FALSE .            
&ENDIF

        IF NOT THIS-OBJECT:btnInnerException:Visible THEN 
            THIS-OBJECT:btnSessionInfo:Location = THIS-OBJECT:btnInnerException:Location. 

        IF VALID-OBJECT (oErrorMessageFormButtonProvider) THEN 
            THIS-OBJECT:RenderCustomButtons () .

        IF VALID-OBJECT (THIS-OBJECT:Exception) AND TYPE-OF (THIS-OBJECT:Exception, 
                                                             Progress.Lang.SysError) AND 
           VALID-OBJECT ({Consultingwerk/get-service.i Consultingwerk.SmartFramework.IMessageProvider}) THEN 
           
            ASSIGN THIS-OBJECT:detailsToolStripMenuItem:Visible = TRUE 
                   THIS-OBJECT:toolStripSeparator1:Visible      = TRUE . 
        ELSE 
            ASSIGN THIS-OBJECT:detailsToolStripMenuItem:Visible = FALSE 
                   THIS-OBJECT:toolStripSeparator1:Visible      = FALSE . 

        /* Mike Fechner, Consultingwerk Ltd. 02.09.2014
           SCL-418 - ability to use the MessageFormImage:Error for the ErrorMessageForm as well */
        IF VALID-OBJECT (MessageFormImages:ImageError) THEN 
            THIS-OBJECT:pictureBox1:Image = MessageFormImages:ImageError . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Renders buttons provided by the IErrorMessageFormButtonProvider 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID RenderCustomButtons ():
		
		DEFINE VARIABLE oList     AS "System.Collections.Generic.List<System.Windows.Forms.Button>" NO-UNDO .
        DEFINE VARIABLE iPosition AS INTEGER                                                        NO-UNDO .
        
        oList = oErrorMessageFormButtonProvider:GetButtonsForError (THIS-OBJECT:Exception) .

        ASSIGN iPosition = THIS-OBJECT:btnSessionInfo:Left + 
                           THIS-OBJECT:btnSessionInfo:Width + 6 .   

        {Consultingwerk/foreach.i System.Windows.Forms.Button oButton in oList}
        
            oButton:Height = THIS-OBJECT:btnSessionInfo:Height.
            oButton:Location = NEW System.Drawing.Point (iPosition, THIS-OBJECT:btnSessionInfo:Top) .
            oButton:Anchor = CAST (Progress.Util.EnumHelper:Add (System.Windows.Forms.AnchorStyles:Left,
                                                                 System.Windows.Forms.AnchorStyles:Bottom),
                                   System.Windows.Forms.AnchorStyles) .
        
            oButton:Click:Subscribe (CustomButtonClickHandler) .
        
            THIS-OBJECT:Controls:Add (oButton) .
                
            ASSIGN iPosition = oButton:Left + 
                               oButton:Width + 6 .   
                
        END.

        /* Mike Fechner, Consultingwerk Ltd. 13.10.2014
           Don't display errors using ErrorHelper:ShowErrorMessage to 
           avoid errors raised from here to cause additional errors */
        CATCH err AS Progress.Lang.Error:
        	MESSAGE ErrorHelper:FormattedErrorMessages (err)
                VIEW-AS ALERT-BOX.	
        END CATCH.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: toggles the image of the btnStack based on the state                                                                        
        Notes:                                                       
        @param plDisplayStack Logical value indicating if the stack trace is visible                 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToggleButtonState (plDisplayStack AS LOGICAL):
        
        IF plDisplayStack THEN 
            btnStack:IMAGE = imageList1:Images["arrow-up_16.gif":U] . 
        ELSE 
            btnStack:IMAGE = imageList1:Images["arrow-down_16.gif":U] .
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor of the ErrorMessageForm class                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ErrorMessageForm ():

        /* Mike Fechner, Consultingwerk Ltd. 05.08.2014
           SCL-392: Workaround to issue in OE11.4 with calling into 
                    GarbageCollectorHelper:DeleteObject(Object[])  */
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[1]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[2]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[3]) .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
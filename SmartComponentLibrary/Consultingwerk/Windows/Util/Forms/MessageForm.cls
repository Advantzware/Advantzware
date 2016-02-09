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
    File        : MessageForm
    Purpose     : Default message Form, see Consultingwerk.Util.MessageFormHelper
    Syntax      : 
    Description : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Using+custom+buttons+on+the+MessageForm
                  for instructions on how to add custom buttons to Message Form 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jul 16 16:01:51 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.*          FROM PROPATH .
USING Consultingwerk.Framework.Enum.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.*     FROM PROPATH .
USING Consultingwerk.Util.Forms.*         FROM PROPATH . 
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH . 
USING Progress.Lang.*                     FROM ASSEMBLY .
USING Progress.Windows.Form               FROM ASSEMBLY .
USING System.Windows.Forms.*              FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Util.Forms.MessageForm INHERITS Form 
    IMPLEMENTS IMessageForm: 
    
    DEFINE PRIVATE VARIABLE btnDetails AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.

    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

    DEFINE PRIVATE VARIABLE textBox1 AS System.Windows.Forms.TextBox NO-UNDO.

    DEFINE PRIVATE VARIABLE groupBox1 AS System.Windows.Forms.GroupBox NO-UNDO.

    DEFINE PRIVATE VARIABLE textBox2 AS System.Windows.Forms.TextBox NO-UNDO.

    DEFINE PRIVATE VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.


    DEFINE PRIVATE VARIABLE lDisplayDetails AS LOGICAL             NO-UNDO INIT TRUE .
    DEFINE PRIVATE VARIABLE oPreviousSize   AS System.Drawing.Size NO-UNDO .
            
    DEFINE VARIABLE lFirstTime AS LOGICAL NO-UNDO INIT TRUE .        
            
    DEFINE VARIABLE oFullWindowDragManager     AS FullWindowDragManager      NO-UNDO EXTENT 3 .              
    DEFINE VARIABLE oMessageFormButtonProvider AS IMessageFormButtonProvider NO-UNDO .            
                
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the DialogResult of the default button                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DefaultDialogResult AS DialogResultEnum NO-UNDO
    GET.
    SET.                
                
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if details should be shown                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DisplayDetails AS LOGICAL NO-UNDO  
    GET:
        RETURN lDisplayDetails .
    END.
    SET (arg AS LOGICAL):
        /* Mike Fechner, Consultingwerk Ltd. 16.07.2009
           Save size, when closing Stack trace */        
        IF arg = FALSE AND lDisplayDetails = TRUE THEN 
            ASSIGN oPreviousSize = NEW System.Drawing.Size (THIS-OBJECT:Size:Width,
                                                            THIS-OBJECT:Size:Height) .        
        
        ASSIGN lDisplayDetails = arg . 
        
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
        Purpose: Gets and sets the Array of Message Button Dialog Results
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MessageButtonDialogResults AS DialogResultEnum EXTENT NO-UNDO 
    GET.
    SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Array of Message Button Labels                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MessageButtonLabels AS CHARACTER EXTENT NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the detail message text                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MessageDetail AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:textBox2:Text .
    END GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:textBox2:Text = arg .        
    END SET.
     
    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the Message Image                                                                      
        Notes:   This implementation requires a System.Drawing.Image                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MessageImage AS Progress.Lang.Object NO-UNDO 
    GET:
        RETURN pictureBox1:Image.
    END GET.
    SET (arg AS Progress.Lang.Object):
        pictureBox1:Image = CAST (arg, System.Drawing.Image).        
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Text                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MessageText AS CHARACTER NO-UNDO 
    GET:
        RETURN THIS-OBJECT:textBox1:Text.
    END GET.
    SET (arg AS CHARACTER):
        THIS-OBJECT:textBox1:Text = REPLACE(arg, CHR(10), CHR(13) + CHR(10)) .
    END SET.    
     
    
    DEFINE PRIVATE VARIABLE toolTip1 AS System.Windows.Forms.ToolTip NO-UNDO.

    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MessageForm class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
     CONSTRUCTOR PUBLIC MessageForm ():
                
        SUPER().
        InitializeComponent().

        ASSIGN oPreviousSize = NEW System.Drawing.Size (THIS-OBJECT:Size:Width,
                                                        THIS-OBJECT:Size:Height) .        

        THIS-OBJECT:DisplayDetails = FALSE  . 

        THIS-OBJECT:ToggleButtonState (THIS-OBJECT:DisplayDetails) .
        
        ASSIGN oFullWindowDragManager[1] = NEW FullWindowDragManager (THIS-OBJECT) 
               oFullWindowDragManager[2] = NEW FullWindowDragManager (THIS-OBJECT:panel1) 
               oFullWindowDragManager[3] = NEW FullWindowDragManager (THIS-OBJECT:pictureBox1) .
        
        oMessageFormButtonProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Util.Forms.IMessageFormButtonProvider} .
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the btnDetails                                                                        
        Notes:                                                 
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event                       
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID btnDetails_Click (sender AS System.Object, 
                                          e AS System.EventArgs):
        
        THIS-OBJECT:DisplayDetails = NOT THIS-OBJECT:DisplayDetails .
        
        THIS-OBJECT:ToggleButtonState (THIS-OBJECT:DisplayDetails) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Creates the Message Buttons                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateButtons ():
        
        DEFINE VARIABLE i       AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO .
        DEFINE VARIABLE oButton LIKE button1 NO-UNDO .
        
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        
        
        ASSIGN iCount = MIN (EXTENT (THIS-OBJECT:MessageButtonLabels),
                             EXTENT (THIS-OBJECT:MessageButtonDialogResults)) .
        
        ASSIGN button1:Text         = THIS-OBJECT:MessageButtonLabels [iCount]
               button1:DialogResult = DialogResultEnum:ToSystemWindowsForms(THIS-OBJECT:MessageButtonDialogResults [iCount]) 
               button1:TabIndex     = 100
               .

       IF VALID-OBJECT (THIS-OBJECT:DefaultDialogResult) THEN 
           IF THIS-OBJECT:MessageButtonDialogResults [iCount] = THIS-OBJECT:DefaultDialogResult THEN 
               THIS-OBJECT:AcceptButton = button1. 
       
        DO i = iCount - 1 TO 1 BY -1:

            nestedvar0 = System.Windows.Forms.AnchorStyles:Bottom.

            ASSIGN 
                oButton              = NEW System.Windows.Forms.Button () 
                oButton:Text         = THIS-OBJECT:MessageButtonLabels [i]
                oButton:DialogResult = DialogResultEnum:ToSystemWindowsForms(THIS-OBJECT:MessageButtonDialogResults [i])
                oButton:Size         = NEW System.Drawing.Size (button1:Width, button1:Height)
                oButton:Top          = button1:Top
                oButton:Left         = button1:Left -
                                       ((iCount - i) * (button1:Width + button1:Margin:Left + button1:Margin:Right))
                oButton:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles)
                oButton:UseCompatibleTextRendering = TRUE
                oButton:UseVisualStyleBackColor    = TRUE
                oButton:TabIndex                   = 100 - iCount + i
                .
                    
            button1:Parent:Controls:Add (oButton) .

           IF THIS-OBJECT:MessageButtonDialogResults [i] = THIS-OBJECT:DefaultDialogResult THEN 
               THIS-OBJECT:AcceptButton = oButton.             
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the click event of custom buttons 
        Notes:   
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data to this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CustomButtonClickHandler (sender AS System.Object,
                                                    e AS System.EventArgs):
        
        DEFINE VARIABLE oMessage AS Message NO-UNDO .
        
        ASSIGN oMessage         = NEW Message () 
               oMessage:Text    = THIS-OBJECT:MessageText 
               oMessage:Detail  = THIS-OBJECT:MessageDetail .
        
        IF oMessageFormButtonProvider:HandleButtonClick (CAST (sender, System.Windows.Forms.Button), 
                                                         oMessage) = TRUE THEN 
            THIS-OBJECT:Close () .

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
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.MessageForm":U).
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:textBox1 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:groupBox1 = NEW System.Windows.Forms.GroupBox().
        THIS-OBJECT:textBox2 = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:btnDetails = NEW System.Windows.Forms.Button().
        THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
        THIS-OBJECT:toolTip1 = NEW System.Windows.Forms.ToolTip(THIS-OBJECT:components).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:groupBox1:SuspendLayout().
        THIS-OBJECT:panel1:SuspendLayout().
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
        THIS-OBJECT:button1:TabIndex = 2.
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
        THIS-OBJECT:groupBox1:Text = "Details:":U.
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
        /* btnDetails */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:btnDetails:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:btnDetails:Location = NEW System.Drawing.Point(12, 225).
        THIS-OBJECT:btnDetails:Name = "btnDetails":U.
        THIS-OBJECT:btnDetails:Size = NEW System.Drawing.Size(24, 24).
        THIS-OBJECT:btnDetails:TabIndex = 1.
        THIS-OBJECT:toolTip1:SetToolTip(THIS-OBJECT:btnDetails, "Toggles the display of message details":U).
        THIS-OBJECT:btnDetails:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:btnDetails:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:btnDetails:Click:Subscribe(THIS-OBJECT:btnDetails_Click).
        /*  */
        /* imageList1 */
        /*  */
        THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream":U), System.Windows.Forms.ImageListStreamer).
        THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
        THIS-OBJECT:imageList1:Images:SetKeyName(0, "arrow-up_16.gif":U).
        THIS-OBJECT:imageList1:Images:SetKeyName(1, "arrow-down_16.gif":U).
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar4 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar4 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar4, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:groupBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:textBox1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Margin = NEW System.Windows.Forms.Padding(0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(561, 213).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* MessageForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:button1.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(561, 261).
        THIS-OBJECT:ControlBox = FALSE.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btnDetails).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:KeyPreview = TRUE.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:MinimumSize = NEW System.Drawing.Size(567, 161).
        THIS-OBJECT:Name = "MessageForm":U.
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
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnLoad (INPUT e AS System.EventArgs):
                
        IF EXTENT (THIS-OBJECT:MessageButtonDialogResults) >= 1 THEN
            CreateButtons () .
            
        SUPER:OnLoad (INPUT e).
            
        IF THIS-OBJECT:MessageDetail > "":U THEN . 
        ELSE THIS-OBJECT:btnDetails:Visible = FALSE .       

        IF VALID-OBJECT (oMessageFormButtonProvider) THEN 
            THIS-OBJECT:RenderCustomButtons () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event                                                                       
        Notes:                                                                        
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):
        
        SUPER:OnShown (e) .
        
        IF lFirstTime AND VALID-OBJECT (THIS-OBJECT:AcceptButton) THEN 
            CAST (CAST (THIS-OBJECT:AcceptButton, Progress.Lang.Object), System.Windows.Forms.Control):Focus () .
        
        FINALLY:
            ASSIGN lFirstTime = FALSE .         
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Renders buttons provided by the IErrorMessageFormButtonProvider 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RenderCustomButtons ():
        
        DEFINE VARIABLE oList     AS "System.Collections.Generic.List<System.Windows.Forms.Button>" NO-UNDO .
        DEFINE VARIABLE iPosition AS INTEGER                                                        NO-UNDO .
        DEFINE VARIABLE iTabIndex AS INTEGER                                                        NO-UNDO INIT 50 .
    
        DEFINE VARIABLE oMessage AS Message NO-UNDO .
        
        ASSIGN oMessage         = NEW Message () 
               oMessage:Text    = THIS-OBJECT:MessageText 
               oMessage:Detail  = THIS-OBJECT:MessageDetail .
        
        oList = oMessageFormButtonProvider:GetButtonsForMessage (oMessage) .

        IF THIS-OBJECT:btnDetails:Visible THEN 
            ASSIGN iPosition = THIS-OBJECT:btnDetails:Left + 
                               THIS-OBJECT:btnDetails:Width + 6 .   
        ELSE 
            ASSIGN iPosition = THIS-OBJECT:btnDetails:Left .   

        {Consultingwerk/foreach.i System.Windows.Forms.Button oButton in oList}
        
            oButton:Height = THIS-OBJECT:btnDetails:Height.
            oButton:Location = NEW System.Drawing.Point (iPosition, THIS-OBJECT:btnDetails:Top) .
            oButton:Anchor = CAST (Progress.Util.EnumHelper:Add (System.Windows.Forms.AnchorStyles:Left,
                                                                 System.Windows.Forms.AnchorStyles:Bottom),
                                   System.Windows.Forms.AnchorStyles) .
        
            oButton:Click:Subscribe (CustomButtonClickHandler) .
            oButton:TabIndex    = iTabIndex . 
            
        
            THIS-OBJECT:Controls:Add (oButton) .
                
            ASSIGN iPosition = oButton:Left + 
                               oButton:Width + 6
                   iTabIndex = iTabIndex + 1 .   
        END.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Toggles the Button state                                                                        
        Notes:                           
        @param plDisplayStack Logical value indicating if the stack should be shown                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ToggleButtonState (plDisplayStack AS LOGICAL):
        
        IF plDisplayStack THEN 
            btnDetails:Image = imageList1:Images["arrow-up_16.gif":U] . 
        ELSE 
            btnDetails:Image = imageList1:Images["arrow-down_16.gif":U] .
        
    END METHOD.


    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MessageForm class                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC MessageForm ():

        /* Mike Fechner, Consultingwerk Ltd. 05.08.2014
           SCL-392: Workaround to issue with calling into GarbageCollectorHelper:DeleteObject(Object[])  */
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[1]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[2]) .
        Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oFullWindowDragManager[3]) .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.

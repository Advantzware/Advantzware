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
    File        : ProgressLangObjectPropertiesForm
    Purpose     : Visualizes properties on a Progress.Lang.Object instance
                  in a GUI for .NET Control
    Syntax      : Static method
                  Consultingwerk.Windows.Util.Forms.ProgressLangObjectPropertiesForm:DisplayPropertiesOfObject (object)
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Dec 05 13:18:29 CET 2015
    Notes       : Relies on reflection, so requires OpenEdge 11.6 or above
  ----------------------------------------------------------------------*/

USING Consultingwerk.Forms.*              FROM PROPATH .            
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH . 
USING Progress.Lang.*                     FROM PROPATH .
USING Consultingwerk.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Windows.Util.Forms.ProgressLangObjectPropertiesForm 
    INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE button1 AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE label1 AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE progressLangObjectPropertyControl1 AS Consultingwerk.Windows.Util.Controls.ProgressLangObjectPropertyControl NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    {Consultingwerk/products.i}
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProgressLangObjectPropertiesForm class  
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProgressLangObjectPropertiesForm ():
        
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

    END CONSTRUCTOR.

&IF DEFINED (AblReflection) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the passed in object in the property grid
        Notes:
        @param poObject The object to visualize
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisplayPropertiesOfObject (poObject AS Progress.Lang.Object):

        ProgressLangObjectPropertiesForm:DisplayPropertiesOfObject (poObject, ?) .
    
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the passed in object in the property grid
        Notes:
        @param poObject The object to visualize
        @param pcTitle The title to display above the object properties
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DisplayPropertiesOfObject (poObject AS Progress.Lang.Object, 
                                                         pcTitle AS CHARACTER):
        
        DEFINE VARIABLE oForm AS ProgressLangObjectPropertiesForm NO-UNDO . 

        oForm = NEW ProgressLangObjectPropertiesForm () .
        oForm:DisplayObjectProperties (poObject, pcTitle) . 
        
        WAIT-FOR oForm:ShowDialog () . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the passed in object in the property grid
        Notes:
        @param poObject The object to visualize
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayObjectProperties (poObject AS Progress.Lang.Object):

        THIS-OBJECT:DisplayObjectProperties (poObject, ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the properties of the passed in object in the property grid
        Notes:
        @param poObject The object to visualize
        @param pcTitle The title to display above the object properties
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisplayObjectProperties (poObject AS Progress.Lang.Object,
                                                pcTitle AS CHARACTER):
        
        IF CharacterType:IsNullOrEmpty (pcTitle) THEN DO:
        
            IF VALID-OBJECT (poObject) THEN 
                pcTitle = poObject:GetClass():TypeName . 
            ELSE 
                pcTitle = "<null reference>":U .
        
        END.
        
        label1:Text = pcTitle .

        progressLangObjectPropertyControl1:DisplayObjectProperties (poObject) .

    END METHOD.
&ENDIF

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Util.Forms.ProgressLangObjectPropertiesForm":U).
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:progressLangObjectPropertyControl1 = NEW Consultingwerk.Windows.Util.Controls.ProgressLangObjectPropertyControl().
        THIS-OBJECT:button1 = NEW System.Windows.Forms.Button().
        THIS-OBJECT:label1 = NEW System.Windows.Forms.Label().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* panel1 */
        /*  */
        DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar0 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:panel1:BackColor = System.Drawing.SystemColors:ControlLightLight.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:label1).
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:progressLangObjectPropertyControl1).
        THIS-OBJECT:panel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:panel1:Name = "panel1":U.
        THIS-OBJECT:panel1:Size = NEW System.Drawing.Size(515, 365).
        THIS-OBJECT:panel1:TabIndex = 0.
        /*  */
        /* progressLangObjectPropertyControl1 */
        /*  */
        DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar1 = CAST(Progress.Util.EnumHelper:Or(CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Bottom), System.Windows.Forms.AnchorStyles), System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:progressLangObjectPropertyControl1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:progressLangObjectPropertyControl1:Location = NEW System.Drawing.Point(12, 44).
        THIS-OBJECT:progressLangObjectPropertyControl1:Name = "progressLangObjectPropertyControl1":U.
        THIS-OBJECT:progressLangObjectPropertyControl1:Size = NEW System.Drawing.Size(489, 309).
        THIS-OBJECT:progressLangObjectPropertyControl1:TabIndex = 2.
        /*  */
        /* button1 */
        /*  */
        DEFINE VARIABLE nestedvar2 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar2 = System.Windows.Forms.AnchorStyles:Bottom.
        THIS-OBJECT:button1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar2, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:button1:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:button1:Location = NEW System.Drawing.Point(426, 371).
        THIS-OBJECT:button1:Name = "button1":U.
        THIS-OBJECT:button1:Size = NEW System.Drawing.Size(75, 23).
        THIS-OBJECT:button1:TabIndex = 1.
        THIS-OBJECT:button1:Text = "&OK":U.
        THIS-OBJECT:button1:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:button1:UseVisualStyleBackColor = TRUE.
        /*  */
        /* label1 */
        /*  */
        DEFINE VARIABLE nestedvar3 AS System.Windows.Forms.AnchorStyles NO-UNDO.
        nestedvar3 = CAST(Progress.Util.EnumHelper:Or(System.Windows.Forms.AnchorStyles:Top, System.Windows.Forms.AnchorStyles:Left), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar3, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
        THIS-OBJECT:label1:Font = NEW System.Drawing.Font("Microsoft Sans Serif":U, Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:label1:Location = NEW System.Drawing.Point(12, 9).
        THIS-OBJECT:label1:Name = "label1":U.
        THIS-OBJECT:label1:Size = NEW System.Drawing.Size(489, 23).
        THIS-OBJECT:label1:TabIndex = 3.
        THIS-OBJECT:label1:Text = "label1":U.
        THIS-OBJECT:label1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* ProgressLangObjectPropertiesForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(513, 406).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:Icon = CAST(resources:GetObject("$this.Icon":U), System.Drawing.Icon).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "ProgressLangObjectPropertiesForm":U.
        THIS-OBJECT:Text = "Object Properties":U.
        THIS-OBJECT:UseStartupSizeAsMinSize = TRUE.
        THIS-OBJECT:WindowPositionRegistryKey = "ProgressLangObjectPropertiesForm":U.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the ProgressLangObjectPropertyControl class  
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC ProgressLangObjectPropertiesForm ():

    END DESTRUCTOR.

END CLASS.

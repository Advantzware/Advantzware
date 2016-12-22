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
    File        : Splash
    Purpose     : Splash screen for SmartComponent Library tools and 
                  samples 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 02 06:42:17 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.*                    FROM PROPATH .
USING Consultingwerk.Util.*               FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Windows.*                  FROM ASSEMBLY .
USING Consultingwerk.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Support.Splash.Splash INHERITS Form   : 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

    DEFINE VARIABLE oFullWindowDragManager AS FullWindowDragManager NO-UNDO . 
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel3 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel2 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Title of the Application 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ApplicationTitle AS CHARACTER NO-UNDO 
	GET:
	    RETURN ultraLabel2:Text . 
	END GET .  
	SET (arg AS CHARACTER):
	    ASSIGN ultraLabel2:Text = arg .  
	END. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Splash class                                                                        
        Notes:      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Splash ():
        
        DEFINE VARIABLE oFrameworkVersion AS FrameworkVersion NO-UNDO . 
        
        SUPER().

        InitializeComponent().
        
        oFullWindowDragManager = NEW FullWindowDragManager (pictureBox1) .

        oFrameworkVersion = NEW FrameworkVersion () . 

        ultraLabel1:Text = oFrameworkVersion:GetVersionString () . 
        ultraLabel3:Text = oFrameworkVersion:GetCopyRights () . 
         
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
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.SmartComponents.Support.Splash.Splash":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel2 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:ultraLabel3 = NEW Infragistics.Win.Misc.UltraLabel().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pictureBox1 */
        /*  */
        THIS-OBJECT:pictureBox1:BorderStyle = System.Windows.Forms.BorderStyle:FixedSingle.
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image":U), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1":U.
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(520, 390).
        THIS-OBJECT:pictureBox1:TabIndex = 0.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        /*  */
        /* ultraLabel1 */
        /*  */
        appearance1:FontData:BoldAsString = "False":U.
        appearance1:FontData:Name = "Arial Rounded MT Bold":U.
        appearance1:ForeColor = System.Drawing.Color:DarkGray.
        THIS-OBJECT:ultraLabel1:Appearance = appearance1.
        THIS-OBJECT:ultraLabel1:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(6, 372).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1":U.
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(308, 15).
        THIS-OBJECT:ultraLabel1:TabIndex = 1.
        THIS-OBJECT:ultraLabel1:Text = "(c)2008-2015 Consultingwerk Ltd.  - All rights reserved.":U.
        /*  */
        /* ultraLabel2 */
        /*  */
        appearance2:FontData:Name = "Arial Rounded MT Bold":U.
        appearance2:FontData:SizeInPoints = Progress.Util.CastUtil:ToSingle(22).
        THIS-OBJECT:ultraLabel2:Appearance = appearance2.
        THIS-OBJECT:ultraLabel2:Location = NEW System.Drawing.Point(2, 76).
        THIS-OBJECT:ultraLabel2:Name = "ultraLabel2":U.
        THIS-OBJECT:ultraLabel2:Size = NEW System.Drawing.Size(302, 182).
        THIS-OBJECT:ultraLabel2:TabIndex = 2.
        THIS-OBJECT:ultraLabel2:Text = "SmartComponent Library":U.
        /*  */
        /* ultraLabel3 */
        /*  */
        appearance3:FontData:BoldAsString = "False":U.
        appearance3:FontData:Name = "Arial Rounded MT Bold":U.
        appearance3:ForeColor = System.Drawing.Color:DarkGray.
        THIS-OBJECT:ultraLabel3:Appearance = appearance3.
        THIS-OBJECT:ultraLabel3:AutoSize = TRUE.
        THIS-OBJECT:ultraLabel3:Location = NEW System.Drawing.Point(6, 356).
        THIS-OBJECT:ultraLabel3:Name = "ultraLabel3":U.
        THIS-OBJECT:ultraLabel3:Size = NEW System.Drawing.Size(308, 15).
        THIS-OBJECT:ultraLabel3:TabIndex = 3.
        THIS-OBJECT:ultraLabel3:Text = "(c)2008-2015 Consultingwerk Ltd.  - All rights reserved.":U.
        /*  */
        /* Splash */
        /*  */
        THIS-OBJECT:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(522, 392).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel3).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel2).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:None.
        THIS-OBJECT:Name = "Splash":U.
        THIS-OBJECT:ShowIcon = FALSE.
        THIS-OBJECT:ShowInTaskbar = FALSE.
        THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterScreen.
        THIS-OBJECT:Text = "SmartComponent Library":U.
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.


 

    DESTRUCTOR PUBLIC Splash ( ):

        GarbageCollectorHelper:DeleteObject (oFullWindowDragManager) .

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.

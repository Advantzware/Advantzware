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
    File        : DockablePaneSizeTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 04 21:48:12 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Advantzware.WinKit.Samples.DockablePaneSizeTest INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraLabel1 AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE propertyGrid2 AS System.Windows.Forms.PropertyGrid NO-UNDO.
    DEFINE PRIVATE VARIABLE propertyGrid1 AS System.Windows.Forms.PropertyGrid NO-UNDO.

		
	CONSTRUCTOR PUBLIC DockablePaneSizeTest (  ):
		
		
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:ultraLabel1 = NEW Infragistics.Win.Misc.UltraLabel().
        THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
        THIS-OBJECT:propertyGrid1 = NEW System.Windows.Forms.PropertyGrid().
        THIS-OBJECT:propertyGrid2 = NEW System.Windows.Forms.PropertyGrid().
        THIS-OBJECT:splitContainer1:Panel1:SuspendLayout().
        THIS-OBJECT:splitContainer1:Panel2:SuspendLayout().
        THIS-OBJECT:splitContainer1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraLabel1 */
        /*  */
        THIS-OBJECT:ultraLabel1:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:ultraLabel1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraLabel1:Name = "ultraLabel1".
        THIS-OBJECT:ultraLabel1:Size = NEW System.Drawing.Size(552, 23).
        THIS-OBJECT:ultraLabel1:TabIndex = 0.
        THIS-OBJECT:ultraLabel1:Text = "Controlsize".
        /*  */
        /* splitContainer1 */
        /*  */
        THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 23).
        THIS-OBJECT:splitContainer1:Name = "splitContainer1".
        /*  */
        /* splitContainer1.Panel1 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel1:Controls:Add(THIS-OBJECT:propertyGrid1).
        /*  */
        /* splitContainer1.Panel2 */
        /*  */
        THIS-OBJECT:splitContainer1:Panel2:Controls:Add(THIS-OBJECT:propertyGrid2).
        THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(552, 192).
        THIS-OBJECT:splitContainer1:SplitterDistance = 268.
        THIS-OBJECT:splitContainer1:TabIndex = 1.
        /*  */
        /* propertyGrid1 */
        /*  */
        THIS-OBJECT:propertyGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:propertyGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:propertyGrid1:Name = "propertyGrid1".
        THIS-OBJECT:propertyGrid1:Size = NEW System.Drawing.Size(268, 192).
        THIS-OBJECT:propertyGrid1:TabIndex = 0.
        THIS-OBJECT:propertyGrid1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* propertyGrid2 */
        /*  */
        THIS-OBJECT:propertyGrid2:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:propertyGrid2:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:propertyGrid2:Name = "propertyGrid2".
        THIS-OBJECT:propertyGrid2:Size = NEW System.Drawing.Size(280, 192).
        THIS-OBJECT:propertyGrid2:TabIndex = 0.
        THIS-OBJECT:propertyGrid2:UseCompatibleTextRendering = TRUE.
        /*  */
        /* DockablePaneSizeTest */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraLabel1).
        THIS-OBJECT:Name = "DockablePaneSizeTest".
        THIS-OBJECT:Size = NEW System.Drawing.Size(552, 215).
        THIS-OBJECT:splitContainer1:Panel1:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:Panel2:ResumeLayout(FALSE).
        THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnParentChanged (e AS System.EventArgs):

        DEFINE VARIABLE oControlPane AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO . 
        DEFINE VARIABLE oPaneGroup AS Infragistics.Win.UltraWinDock.DockableGroupPane NO-UNDO . 
		
		SUPER:OnParentChanged (e) .

        IF TYPE-OF (THIS-OBJECT:Parent,
                    Infragistics.Win.UltraWinDock.DockableWindow) THEN DO:
                        
            propertyGrid1:SelectedObject = CAST (THIS-OBJECT:Parent,
                                                 Infragistics.Win.UltraWinDock.DockableWindow):Pane NO-ERROR .

            propertyGrid2:SelectedObject = CAST (THIS-OBJECT:Parent,
                                                 Infragistics.Win.UltraWinDock.DockableWindow):Pane:Parent NO-ERROR .
        END.                        

	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PROTECTED OVERRIDE VOID OnSizeChanged (e AS System.EventArgs):
		
		SUPER:OnSizeChanged (e) .
		
		IF VALID-OBJECT (ultraLabel1) AND VALID-OBJECT (THIS-OBJECT:Size) THEN 
		    ultraLabel1:Text = THIS-OBJECT:Size:ToString() . 

        IF TYPE-OF (THIS-OBJECT:Parent,
                    Infragistics.Win.UltraWinDock.DockableWindow) AND VALID-OBJECT (propertyGrid1) AND VALID-OBJECT (propertyGrid2) THEN DO:
                        
            propertyGrid1:SelectedObject = CAST (THIS-OBJECT:Parent,
                                                 Infragistics.Win.UltraWinDock.DockableWindow):Pane NO-ERROR .

            propertyGrid1:Refresh () NO-ERROR .  

            propertyGrid2:SelectedObject = CAST (THIS-OBJECT:Parent,
                                                 Infragistics.Win.UltraWinDock.DockableWindow):Pane:Parent NO-ERROR .
            
            propertyGrid2:Refresh () NO-ERROR .                                                  
        END.                        


	END METHOD.

	DESTRUCTOR PUBLIC DockablePaneSizeTest ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
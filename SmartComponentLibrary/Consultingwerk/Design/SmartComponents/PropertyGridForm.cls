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
    File        : PropertyGridForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 09 23:00:09 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Forms.BaseForm.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Design.SmartComponents.PropertyGridForm INHERITS BaseForm: 
    
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE propertyGrid1 AS System.Windows.Forms.PropertyGrid NO-UNDO.

        
    DEFINE PUBLIC PROPERTY PropertyGrid AS System.Windows.Forms.PropertyGrid NO-UNDO 
    GET.
    PRIVATE SET. 

    CONSTRUCTOR PUBLIC PropertyGridForm (  ):
        
        
        SUPER().
        InitializeComponent().
        
        THIS-OBJECT:PropertyGrid = THIS-OBJECT:propertyGrid1 . 
        
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
        THIS-OBJECT:propertyGrid1 = NEW System.Windows.Forms.PropertyGrid().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* propertyGrid1 */
        /*  */
        THIS-OBJECT:propertyGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:propertyGrid1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:propertyGrid1:Name = "propertyGrid1":U.
        THIS-OBJECT:propertyGrid1:PropertySort = System.Windows.Forms.PropertySort:Alphabetical.
        THIS-OBJECT:propertyGrid1:Size = NEW System.Drawing.Size(278, 548).
        THIS-OBJECT:propertyGrid1:TabIndex = 0.
        THIS-OBJECT:propertyGrid1:UseCompatibleTextRendering = TRUE.
        /*  */
        /* PropertyGridForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(278, 548).
        THIS-OBJECT:ControlBox = FALSE.
        THIS-OBJECT:Controls:Add(THIS-OBJECT:propertyGrid1).
        THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:SizableToolWindow.
        THIS-OBJECT:MaximizeBox = FALSE.
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "PropertyGridForm":U.
        THIS-OBJECT:Text = "Properties":U.
        THIS-OBJECT:TopMost = TRUE.
        THIS-OBJECT:WindowPositionRegistryKey = "PropertyGridForm":U.
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    DESTRUCTOR PUBLIC PropertyGridForm ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
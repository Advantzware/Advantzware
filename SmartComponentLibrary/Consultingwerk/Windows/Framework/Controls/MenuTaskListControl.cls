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
    File        : MenuTaskListControl
    Purpose     : UltraExplorerBar that is populated from an IMenuStructureProvider
                  service and can start menu functions using the IMenuFunctionLauncher
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 26 21:00:43 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*   FROM PROPATH .
USING Consultingwerk.Windows.Controls.*      FROM PROPATH . 
USING Consultingwerk.Windows.Framework.*     FROM PROPATH .  
USING Infragistics.Win.UltraWinExplorerBar.* FROM ASSEMBLY .
USING Progress.Lang.*                        FROM PROPATH .    
USING Progress.Windows.UserControl           FROM ASSEMBLY .
USING System.Drawing.*                       FROM ASSEMBLY . 
USING System.Windows.Forms.*                 FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Framework.Controls.MenuTaskListControl INHERITS UserControl: 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraExplorerBar1 AS Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar NO-UNDO.

    {Consultingwerk/Windows/Framework/ttMenuStructure.i}

    DEFINE         VARIABLE oMenuProvider  AS IMenuStructureProvider NO-UNDO .
    DEFINE PRIVATE VARIABLE oImageProvider AS IImageProvider         NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the root node's key
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY RootNodeKey AS CHARACTER NO-UNDO INIT "":U 
    GET.
    SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MenuTaskListControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC MenuTaskListControl ():
		
        SUPER().
        
        InitializeComponent().

        oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:Add (THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fills the menu 
        Notes:   
        @param pcRootNodeKey The root node key to retrieve
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID FillMenuStructure (pcRootNodeKey AS CHARACTER):
        
        DEFINE VARIABLE cParentNodeKey AS CHARACTER             NO-UNDO .
        DEFINE VARIABLE oGroup         AS UltraExplorerBarGroup NO-UNDO . 
        
        oMenuProvider:GetMenuStructure (NEW GetMenuStructureParameter (pcRootNodeKey, 
                                                                       0,
                                                                       FALSE),
                                        OUTPUT TABLE ttMenuStructure BY-REFERENCE) .

        FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcRootNodeKey
                                    BY ttMenuStructure.MenuSequence:
            
            oGroup = ultraExplorerBar1:Groups:Add (ttMenuStructure.MenuKey,
                                                   ttMenuStructure.MenuName) .
            
            THIS-OBJECT:PopulateItems (oGroup, ttMenuStructure.MenuKey) .
            
            oGroup:Expanded = TRUE  . 
        END.
    
        FINALLY:
            EMPTY TEMP-TABLE ttMenuStructure .      
        END FINALLY.

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
        THIS-OBJECT:ultraExplorerBar1 = NEW Infragistics.Win.UltraWinExplorerBar.UltraExplorerBar().
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraExplorerBar1 */
        /*  */
        THIS-OBJECT:ultraExplorerBar1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraExplorerBar1:ItemClick:Subscribe (ItemClickHandler).
        THIS-OBJECT:ultraExplorerBar1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraExplorerBar1:Name = "ultraExplorerBar1":U.
        THIS-OBJECT:ultraExplorerBar1:Size = NEW System.Drawing.Size(180, 409).
        THIS-OBJECT:ultraExplorerBar1:TabIndex = 0.
        THIS-OBJECT:ultraExplorerBar1:ViewStyle = Infragistics.Win.UltraWinExplorerBar.UltraExplorerBarViewStyle:Office2007.
        /*  */
        /* MenuTaskListControl */
        /*  */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraExplorerBar1).
        THIS-OBJECT:Name = "MenuTaskListControl":U.
        THIS-OBJECT:Size = NEW System.Drawing.Size(180, 409).
        CAST(THIS-OBJECT:ultraExplorerBar1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ItemClick Event of the UltraExplorerBar 
        Notes:   
        @param sender The reference to object that raised the event
        @param e The ItemEventArgs instance with the data for this event                                                                      
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ItemClickHandler (sender AS System.Object, 
                                          e AS ItemEventArgs):
        
        DEFINE VARIABLE oLauncher AS IMenuFunctionLauncher NO-UNDO . 
        DEFINE VARIABLE cKey      AS CHARACTER             NO-UNDO.

        oLauncher = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuFunctionLauncher} .
        
        IF VALID-OBJECT (oLauncher) THEN 
            oLauncher:LaunchMenuFunction (UNBOX (e:Item:Tag), 
                                          NEW MenuContext (UNBOX (e:Item:Tag), e:Item:Text, THIS-OBJECT)) .

        CATCH err AS Progress.Lang.Error:
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
        END CATCH.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Loads the Menu Structure                                                                  
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID LoadMenu ():
        
        oMenuProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IMenuStructureProvider} .

        IF VALID-OBJECT (oMenuProvider) THEN 
            THIS-OBJECT:FillMenuStructure (THIS-OBJECT:RootNodeKey) .        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the Load event 
        Notes:   Populates the Menu Structrure   
        @param e The System.EventArgs with the data for the Load event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnLoad (e AS System.EventArgs):
                
        SUPER:OnLoad (e) .

        THIS-OBJECT:LoadMenu () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates an UltraExplorerBarGroup with items
        Notes:   
        @param poGroup The reference to the UltraExplorerBarGroup to add items to 
        @param pcParentMenuKey The key of the parent menu structure record
    ------------------------------------------------------------------------------*/
	METHOD PROTECTED VOID PopulateItems (poGroup AS UltraExplorerBarGroup,
	                                     pcParentMenuKey AS CHARACTER):
		
		DEFINE VARIABLE oItem AS UltraExplorerBarItem NO-UNDO . 
		
		FOR EACH ttMenuStructure WHERE ttMenuStructure.ParentMenuKey = pcParentMenuKey
		                           AND ttMenuStructure.FunctionKey   > "":U
		                            BY ttMenuStructure.MenuSequence:
		                                
            oItem = poGroup:Items:Add (ttMenuStructure.MenuKey,
                                       ttMenuStructure.MenuName) .		                                  

            oItem:Tag = BOX (ttMenuStructure.FunctionKey) .

            oItem:Settings:AppearancesLarge:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuLargeImage) .
            oItem:Settings:AppearancesSmall:Appearance:Image = oImageProvider:ImageForKey (ttMenuStructure.MenuSmallImage) .
        END.		                               

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the MenuTaskListControl class 
        Notes:   
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC MenuTaskListControl ():

    END DESTRUCTOR .

END CLASS.

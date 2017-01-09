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
    File        : MenuDatasetController
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 11.10.2012 01:41:25
    Notes       : Conditionally implements the IDatasetController Interface
                  when the SmartComponentLibrary preprocessor option is set.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.*                 FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.*            FROM PROPATH .
USING Consultingwerk.Windows.Framework.*              FROM PROPATH . 
USING Consultingwerk.Windows.Framework.Menu.*         FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH . 
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.MenuDatasetController 
    &IF DEFINED (SmartComponentLibrary) NE 0 &THEN IMPLEMENTS IDatasetController &ENDIF : 

    { Consultingwerk/SmartFramework/Menu/dsMenu.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the static dataset defined inside the
                 Dataset Controller                                                                     
        Notes:   Interface IDatasetController member                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the MenuDatasetController class                                                                      
        Notes:   Assigns the DatasetHandle property                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MenuDatasetController ():
        SUPER ().

        THIS-OBJECT:DatasetHandle = DATASET dsMenu:HANDLE .         
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Verifies that the Character Array of file names contains a valid 
                 class naem
        Notes:   
        @param pcFileNames An ABL Character Array of file names
        @return Logical value indicating if the file names contain class names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL ContainsClassFileNames (pcFileNames AS CHARACTER EXTENT):
		
		DEFINE VARIABLE oClass AS Progress.Lang.Class NO-UNDO . 
		DEFINE VARIABLE i      AS INTEGER             NO-UNDO .
		
        &IF DEFINED (DotNetAccessible) NE 0 &THEN
        DO i = 1 TO EXTENT (pcFileNames):
        
            oClass = ClassHelper:FileNameToClass (pcFileNames[i]) NO-ERROR . 
        
            IF VALID-OBJECT (oClass) THEN 
                RETURN TRUE . 
        END.
        &ENDIF

		RETURN FALSE .

	END METHOD .

    &IF DEFINED (DotNetAccessible) NE 0 AND PROVERSION NE "10.2B":U &THEN
    /*------------------------------------------------------------------------------
        Purpose: Creates MenuFunction records for the passed in File Names
        Notes:   
        @param pcFileNames An ABL Character Array of file names
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID CreateFunctionsForClasses (pcFileNames AS CHARACTER EXTENT):

        DEFINE VARIABLE oClass       AS Progress.Lang.Class     NO-UNDO . 
        DEFINE VARIABLE i            AS INTEGER                 NO-UNDO .
		DEFINE VARIABLE cModuleGuid  AS CHARACTER               NO-UNDO . 
		
        DEFINE VARIABLE oLaunchForm    AS LaunchFormCallParameter           NO-UNDO .
        DEFINE VARIABLE oInvokeService AS InvokeServiceMethodCallParameter  NO-UNDO .
        DEFINE VARIABLE oInvokeStatic  AS InvokeStaticMethodCallParameter   NO-UNDO .   
        DEFINE VARIABLE oLookupDialog  AS SmartBusinessEntityLookupDialog   NO-UNDO . 
        DEFINE VARIABLE oResult        AS System.Windows.Forms.DialogResult NO-UNDO . 
        
        DEFINE VARIABLE roRowid       AS ROWID                             NO-UNDO .
        DEFINE VARIABLE cGuid         AS CHARACTER                         NO-UNDO . 

        DEFINE BUFFER eSmartFunction FOR eSmartFunction . 

        oLookupDialog = NEW SmartBusinessEntityLookupDialog ("MenuDatasetControllerLookup":U).
                 
        ASSIGN oLookupDialog:AdapterType                      = "":U 
               oLookupDialog:LookupSmartBusinessEntityAdapter = ? .
        
        oLookupDialog:InitializeDataAdapter() .
         
        /* Set the title of the Dialog */
        oLookupDialog:SetText ("Select Module"{&TRAN}) .

        /* Set the usual properties for accessing a BusinessEntity on the backend */
        ASSIGN 
            oLookupDialog:AppServerPartition          = "":U
            oLookupDialog:LookupBrowserFields         = "ModuleName":U
            oLookupDialog:LookupDialogQueryString     = "FOR EACH eSmartModul":U 
            oLookupDialog:LookupDialogFilterFields    = "ModuleName":U
            oLookupDialog:LookupDialogFilterOperators = "BEGINS":U                                                            
            oLookupDialog:LookupEntityName            = "Consultingwerk.SmartFramework.Menu.ModuleBusinessEntity":U
            oLookupDialog:LookupEntityTable           = "eSmartModule":U
            oLookupDialog:LookupEntityView            = "":U
            oLookupDialog:LookupKeyField              = "ModuleGuid":U
            
            roRowid                                   = ?
            .
          
        /* Show the Dialog, WAIT-FOR Ok or Cancel */ 
        WAIT-FOR oLookupDialog:ShowDialog () SET oResult.

        {Consultingwerk/check-dialogresult-ok.i oResult} 

        ASSIGN cModuleGuid = oLookupDialog:GetLookupFieldValue ("ModuleGuid":U) . 

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET {&dataset-name}:HANDLE, TRUE) .
        
        DO i = 1 TO EXTENT (pcFileNames):
            oClass = ClassHelper:FileNameToClass (pcFileNames[i]) . 
        
            IF NOT VALID-OBJECT (oClass) THEN 
                NEXT . 
        
            IF oClass:IsA ("System.Windows.Forms.Form":U) THEN DO:

                oLaunchForm = NEW LaunchFormCallParameter ().
                oLaunchForm:ClassName = oClass:TypeName .
    
                CREATE eSmartFunction.
                ASSIGN eSmartFunction.FunctionGuid          = GUID /* temporary GUID to allow add multiple */
                       eSmartFunction.FunctionDescription   = oClass:TypeName 
                       eSmartFunction.FunctionName          = ENTRY (NUM-ENTRIES (oClass:TypeName, ".":U),
                                                                     oClass:TypeName, ".":U) 
                       eSmartFunction.FunctionCallParameter = oLaunchForm:Serialize()
                       eSmartFunction.FunctionModuleGuid    = cModuleGuid .                                                                   

                IF roRowid = ? THEN 
                    ASSIGN roRowid = ROWID (eSmartFunction) .

                RELEASE eSmartFunction .
                        
                GarbageCollectorHelper:DeleteObject (oLaunchForm) .
            END. 
            ELSE IF oClass:IsInterface() THEN DO:
                oInvokeService = NEW InvokeServiceMethodCallParameter ().
                oInvokeService:InterfaceName = oClass:TypeName .
    
                CREATE eSmartFunction.
                ASSIGN eSmartFunction.FunctionGuid          = GUID /* temporary GUID to allow add multiple */
                       eSmartFunction.FunctionDescription   = SUBSTITUTE ("Invoke Service &1"{&TRAN}, oClass:TypeName) 
                       eSmartFunction.FunctionName          = ENTRY (NUM-ENTRIES (oClass:TypeName, ".":U),
                                                                     oClass:TypeName, ".":U) 
                       eSmartFunction.FunctionCallParameter = oInvokeService:Serialize()
                       eSmartFunction.FunctionModuleGuid    = cModuleGuid .                                                                   

                IF roRowid = ? THEN 
                    ASSIGN roRowid = ROWID (eSmartFunction) .

                RELEASE eSmartFunction .
                        
                GarbageCollectorHelper:DeleteObject (oInvokeService) .
            END.
            ELSE DO:
                oInvokeStatic = NEW InvokeStaticMethodCallParameter ().
                oInvokeStatic:ClassName = oClass:TypeName .
    
                CREATE eSmartFunction.
                ASSIGN eSmartFunction.FunctionGuid          = GUID /* temporary GUID to allow add multiple */
                       eSmartFunction.FunctionDescription   = SUBSTITUTE ("Invoke &1"{&TRAN}, oClass:TypeName) 
                       eSmartFunction.FunctionName          = ENTRY (NUM-ENTRIES (oClass:TypeName, ".":U),
                                                                     oClass:TypeName, ".":U) 
                       eSmartFunction.FunctionCallParameter = oInvokeStatic:Serialize()
                       eSmartFunction.FunctionModuleGuid    = cModuleGuid .                                                                   

                IF roRowid = ? THEN 
                    ASSIGN roRowid = ROWID (eSmartFunction) .

                RELEASE eSmartFunction .
                        
                GarbageCollectorHelper:DeleteObject (oInvokeStatic) .                
            END.
        END.
        
        DatasetHelper:SetTrackingChanges (DATASET {&dataset-name}:HANDLE, FALSE) .
        
        SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT):SubmitChanges() .
        
        FIND eSmartFunction WHERE ROWID (eSmartFunction) = roRowid NO-ERROR .
        IF AVAILABLE eSmartFunction THEN 
            ASSIGN cGuid = eSmartFunction.FunctionGuid . 
        
        SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT):RetrieveData() .        

        IF cGuid > "":U THEN 
            SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT):FindRowWhere 
                (SUBSTITUTE ("WHERE eSmartFunction.FunctionGuid = &1":U, QUOTER (cGuid))) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oLaunchForm) .		
        END FINALLY.

	END METHOD .
    &ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when detaching from the Dataset of the DatasetController.
                 Allows event unsubscription etc.                                                                      
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object detaching from the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterConsumer (poConsumer AS Progress.Lang.Object):
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Get the FunctionGuid of a Menu Item identified by the MenuGuid
        Notes:   
        @param pcMenuGuid Id of a Menu entry
        @return The FunctionGuid of the requested Menu Item. ? if not found.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetFunctionGuidOfMenuItem (pcMenuGuid AS CHARACTER):
        
        FIND FIRST eSmartMenu WHERE eSmartMenu.MenuGuid = pcMenuGuid NO-LOCK NO-ERROR.
        
        IF AVAILABLE eSmartMenu THEN 
            RETURN eSmartMenu.FunctionGuid.
        ELSE 
            RETURN ?.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Get the MenuName of a Menu Item identified by the MenuGuid
        Notes:   
        @param pcMenuGuid Id of a Menu entry
        @return The MenuName of the requested Menu Item. Blank if not found.
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GetMenuNameFromMenuGuid (pcMenuGuid AS CHARACTER):

        FIND FIRST eSmartMenu WHERE eSmartMenu.MenuGuid = pcMenuGuid NO-LOCK NO-ERROR.
        
        IF AVAILABLE eSmartMenu THEN 
            RETURN eSmartMenu.MenuName.
        ELSE 
            RETURN "":U.

    END METHOD .
        
    /*------------------------------------------------------------------------------
        Purpose: Invoked by SmartBusinessEntityAdapter and SmartDatasetChildAdapter
                 instances when attaching to the Dataset of the DatasetController.
                 Allows event subscription etc.                                                                     
        Notes:   Interface IDatasetController member                                                                        
        @param poConsumer The reference of the Object attaching to the Dataset Controller                                                                
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterConsumer (poConsumer AS Progress.Lang.Object):

    END METHOD.

&IF DEFINED (DotNetAccessible) NE 0 AND PROVERSION NE "10.2B":U &THEN
    /*------------------------------------------------------------------------------
        Purpose: Brings up a Dialog to reorder the Nodes within the same level, 
                 populates the dialog and writes back changed data into the DB
        Notes:   
        @param poForm The reference to the Form to parent the dialog to
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ReorderItemsWithinLevel (poForm AS Progress.Lang.Object):

        DEFINE VARIABLE oReorderMenuItemsForm AS ReorderMenuItemsForm              NO-UNDO .
        DEFINE VARIABLE oDialogResult         AS System.Windows.Forms.DialogResult NO-UNDO .
        
        DEFINE VARIABLE oAdapter              AS SmartBusinessEntityAdapter        NO-UNDO . 
        
        DEFINE VARIABLE cBaseKey              AS CHARACTER                         NO-UNDO .
        DEFINE VARIABLE hBuffer               AS HANDLE                            NO-UNDO .
        DEFINE VARIABLE oForm                 AS System.Windows.Forms.Form         NO-UNDO . 
		
		DEFINE BUFFER b_eSmartMenu            FOR eSmartMenu . 
		
		oAdapter = SmartBusinessEntityAdapterSettings:FromDatasetController (THIS-OBJECT) .
		
		/* Mike Fechner, Consultingwerk Ltd. 01.11.2013
           Ensure that Query and BindingSource are in sync */
		oAdapter:QueryHandle:REPOSITION-TO-ROW (oAdapter:BindingSource:Position + 1) . 
		
		IF NOT AVAILABLE (eSmartMenu) THEN 
            UNDO, THROW NEW AppError ("Please select a Menu entry from the tree!"{&TRAN}, 0).
        
		ASSIGN cBaseKey = eSmartMenu.ParentMenuGuid.
		
		oReorderMenuItemsForm = NEW ReorderMenuItemsForm ().

        FOR EACH b_eSmartMenu WHERE b_eSmartMenu.ParentMenuGuid = cBaseKey ON ERROR UNDO, THROW:
		    oReorderMenuItemsForm:AddListEntry (b_eSmartMenu.MenuGuid, b_eSmartMenu.MenuName).
		END.
		
        IF VALID-OBJECT (poForm) AND  
           TYPE-OF (poForm, System.Windows.Forms.Form) THEN 
            oForm = CAST (poForm, System.Windows.Forms.Form). 
        
        WAIT-FOR oReorderMenuItemsForm:ShowDialog (oForm) SET oDialogResult.

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET {&dataset-name}:HANDLE, TRUE) .
		
		IF oDialogResult:Equals (System.Windows.Forms.DialogResult:OK) THEN 
		DO TRANSACTION ON ERROR UNDO, THROW:
		    /* Marko Rüterbories, Consultingwerk Ltd. 15.02.2013
               Store data of reordering */
            FOR EACH b_eSmartMenu WHERE b_eSmartMenu.ParentMenuGuid = cBaseKey ON ERROR UNDO, THROW:
                b_eSmartMenu.MenuSequence = oReorderMenuItemsForm:GetIndexOfEntry (b_eSmartMenu.MenuGuid).
            END.
		END. 

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET {&dataset-name}:HANDLE, FALSE) .
		
		oAdapter:SubmitChanges ().

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET {&dataset-name}:HANDLE, TRUE) .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oReorderMenuItemsForm).
        END FINALLY.

	END METHOD .
&ENDIF

END CLASS.

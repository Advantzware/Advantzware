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
    File        : SmartGenericLookup
    Purpose     : A generic Lookup implementation useful for any allowing
                  lookups on any ISmartDataSource implementation
    Syntax      : 
    Description : 
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue Apr 07 08:29:30 CEST 2009
    Notes       : This Lookup does not provide the ability to perform 
                  lookups on partial entered keys in the lookup field.
                  However, this might be implemented in a derived type
                  with knowledge of a specific data adapter class and
                  filter syntax
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH . 
USING Consultingwerk.SmartComponents.Interfaces.* . /* FROM PROPATH is not possible here, then the .Interfaces.Design would not be accessible */ 
USING Consultingwerk.SmartComponents.Interfaces.Design.* FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.Resources.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Consultingwerk.Util.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .
USING Progress.Util.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartGenericLookup 
    INHERITS SmartLookup : 

    /*------------------------------------------------------------------------------
       Purpose: Represents the SmartDataSource property casted to a .NET Interface
       Notes:   Implementation of Interface in Consultingwerk.SmartComponents.dll
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY LinkDataSource AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource NO-UNDO 
    GET:
        IF TYPE-OF (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) THEN 
            RETURN CAST (THIS-OBJECT:SmartDataSource, Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource) . 
    END GET . 
    SET (arg AS Consultingwerk.SmartComponents.Interfaces.Design.IDesignDataSource):
        IF NOT VALID-OBJECT (arg) OR TYPE-OF (arg, SmartDataAdapter) THEN 
            ASSIGN THIS-OBJECT:SmartDataSource = CAST (arg, SmartDataAdapter) .         
    END.

    /*------------------------------------------------------------------------------
        Purpose: Get/Set the SmartDataSource of the current Class. The SmartDataSource 
                 can be set to any class implementing the Interface ISmartDataSource.
                 This Property is set by the developer when initializing a Control 
                 implementing the Interface ISmartDataTarget.
                 Enforced by Interface ISmartDataTarget.
        Notes:   
    ------------------------------------------------------------------------------*/
      DEFINE PUBLIC PROPERTY SmartDataSource AS SmartDataAdapter NO-UNDO 
    GET.
    SET.    

    /*------------------------------------------------------------------------------
        Purpose: Creates a new Instance of SmartGenericLookup and subscribes Handlers
                 to Events.
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartGenericLookup ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the lookup functioanlity, performs a call to the data source
                 to retrieve the record based on the field value the user entered
                 in the lookup control
        Notes:   Abstract method that needs to be overidden in a more specialized
                 Lookup class, public method, that always assumes that the lookup has 
                 no longer focus. Calls into PerformLookup (TRUE).
    ------------------------------------------------------------------------------*/   
    METHOD OVERRIDE PUBLIC VOID PerformLookup ():
        
        THIS-OBJECT:PerformLookup (TRUE) .

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: PerformLookup is not supported by the SmartGenericLookup
        Notes:   The SmartGenericLookup is not able to fetch data after the user 
                 enters a (partial) key value
        @param plLostFocus Not used                                                                                         
    ------------------------------------------------------------------------------*/    
    METHOD OVERRIDE PROTECTED VOID PerformLookup (plLostFocus AS LOGICAL):
        
        /* Do nothing */

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: A LookupDialog is shown which contains Data from a given 
                 SmartDataSource.
        Notes:   This Method is called from the Base Class if a user presses the 
                 LookupEditorButton in RunTime.
                 A LookupDialog is set up with a Title and Data from a 
                 SmartDataSource. The shown Columns of the resultset can be limited
                 by providing a comma delimeted list to the Property 
                 LookupBrowserFields. The LookupKeyField is the field used to find
                 the correct Data Row when comming back from the Dialog. When the 
                 OK Button was selected the related fields are recursively updated.
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID ShowLookupDialog ():
        
        DEFINE VARIABLE oLookupDialog AS SmartGenericLookupDialog          NO-UNDO.
        DEFINE VARIABLE oResult       AS System.Windows.Forms.DialogResult NO-UNDO.
        DEFINE VARIABLE i             AS INTEGER                           NO-UNDO.
        DEFINE VARIABLE oControl      AS System.Windows.Forms.Control      NO-UNDO.
        DEFINE VARIABLE oParent       AS System.Windows.Forms.Control      NO-UNDO.
        DEFINE VARIABLE oParentForm   AS System.Windows.Forms.Form         NO-UNDO.
        
        IF NUM-ENTRIES (THIS-OBJECT:LookupFields) <> NUM-ENTRIES (THIS-OBJECT:LookupControls) THEN DO: 
            
            MESSAGE SmartComponentLibraryCustomizer:SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part1 SKIP(2)
                    SmartComponentLibraryCustomizer:SmartGenericLookup_ShowLookupDialogNumberOfControlsAndFieldsNotEqual_Part2
                VIEW-AS ALERT-BOX ERROR TITLE THIS-OBJECT:Text.
                
            RETURN ERROR .
        END.
        
        IF NOT VALID-OBJECT (THIS-OBJECT:SmartDataSource) THEN DO:
            
            MESSAGE SmartComponentLibraryCustomizer:SmartGenericLookup_ShowLookupDialogNoSmartDataSource 
                VIEW-AS ALERT-BOX ERROR TITLE THIS-OBJECT:Text.
                
            RETURN ERROR .

        END.
        
        oLookupDialog = NEW SmartGenericLookupDialog (THIS-OBJECT).
        
        oLookupDialog:LookupBrowser:InitializeLayout:Subscribe (LookupBrowserInitializeLayoutHandler) .
        
        ASSIGN 
            oLookupDialog:Text                = THIS-OBJECT:LookupDialogTitle
            oLookupDialog:LookupBrowserFields = THIS-OBJECT:LookupBrowserFields
            oLookupDialog:SmartDataSource     = THIS-OBJECT:SmartDataSource
            oLookupDialog:LookupKeyField      = THIS-OBJECT:LookupKeyField
            .
        
        oParentForm = THIS-OBJECT:FindForm () .
        
        IF VALID-OBJECT (oParentForm) THEN 
            WAIT-FOR oLookupDialog:ShowDialog (oParentForm) SET oResult.
        ELSE 
            WAIT-FOR oLookupDialog:ShowDialog () SET oResult.

        IF EnumHelper:AreEqual (oResult, System.Windows.Forms.DialogResult:Ok) THEN DO:
            THIS-OBJECT:TEXT = oLookupDialog:GetLookupFieldValue (THIS-OBJECT:LookupKeyField).

            oParent = THIS-OBJECT:Parent .    

            ParentLookup: DO WHILE VALID-OBJECT(oParent):
                IF TYPE-OF (oParent, SmartViewerControl) THEN 
                    LEAVE ParentLookup .    
                    
                oParent = oParent:Parent .                
            END.

            IF VALID-OBJECT(oParent) THEN 
                DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                    ASSIGN oControl = CAST(oParent, SmartViewerControl):GetViewerField (ENTRY(i, THIS-OBJECT:LookupControls)) .           
                    
                    IF VALID-OBJECT(oControl) THEN 
                        oControl:Text = oLookupDialog:GetLookupFieldValue (ENTRY(i, THIS-OBJECT:LookupFields)) .  
                END.
            ELSE 
                DO i = 1 TO NUM-ENTRIES(THIS-OBJECT:LookupFields):
                    IF THIS-OBJECT:Parent:Controls:ContainsKey(ENTRY(i, THIS-OBJECT:LookupControls)) THEN
                         THIS-OBJECT:Parent:Controls[ENTRY(i, THIS-OBJECT:LookupControls)]:Text =
                                    oLookupDialog:GetLookupFieldValue (ENTRY(i, THIS-OBJECT:LookupFields)) .  
                END.                    
        END.

        THIS-OBJECT:OnLookupCompleted (System.EventArgs:Empty) .

        THIS-OBJECT:SelectAll() .
                
        CATCH e AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (e) .
        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oLookupDialog) THEN 
                oLookupDialog:LookupBrowser:InitializeLayout:Unsubscribe (LookupBrowserInitializeLayoutHandler) . 
            
            DELETE OBJECT oLookupDialog.
        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
        Purpose: This Method returns a Registry Key where the Lookup size and position
                 is stored for each Lookup.
        Notes:   The Name of the Key is individual buildable.
        @return The registry key where the lookup size and position is stored
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER GetLookupRegistryKey ():
        
        RETURN THIS-OBJECT:SmartDataSource:GetClass():TypeName.

    END METHOD.

END CLASS.

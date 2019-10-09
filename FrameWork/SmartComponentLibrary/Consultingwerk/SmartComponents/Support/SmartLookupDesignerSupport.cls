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
    File        : SmartLookupDesignerSupport
    Purpose     : Supports the SmartLookup Designer functionality
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 10 21:25:25 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Consultingwerk.Util.*                           FROM PROPATH .
USING Progress.Lang.*                                 FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Support.SmartLookupDesignerSupport: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Returns the Names of all Controls in the ISmartBusinessEntityLookupDesign's 
                 Container                                                                        
        Notes:            
        @param poLookup The ISmartBusinessEntityLookupDesign instance
        @return The comma delimited list of all control names                                                            
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC CHARACTER GetAllControls (poLookup AS ISmartBusinessEntityLookupDesign):
        
        IF NOT VALID-OBJECT (poLookup:GetContainer()) THEN 
            RETURN "":U . 
        ELSE 
            RETURN SmartLookupDesignerSupport:GetAllControls (CAST(CAST (poLookup:GetContainer(), 
                                                                         System.ComponentModel.Design.IDesignerHost):RootComponent, 
                                                                   System.Windows.Forms.Control):Controls) . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Names of all Controls in ControlCollection                            
        Notes:            
        @param poControls The ControlCollection containing the Controls
        @return The comma delimited list of all control names                                                            
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC CHARACTER GetAllControls (poControls AS System.Windows.Forms.Control+ControlCollection):
        
        DEFINE VARIABLE cControls AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
        
        DO i = 0 TO poControls:Count - 1:
            
            IF NOT ListHelper:EntryIsInList (poControls[i]:Name, cControls) THEN  
                 ASSIGN cControls = cControls + ",":U + poControls[i]:Name .
              
            IF poControls[i]:Controls:Count > 0 THEN 
                ASSIGN cControls = cControls + ",":U + SmartLookupDesignerSupport:GetAllControls (poControls[i]:Controls) .
        END.
        
        RETURN TRIM (cControls, ",") . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the comma delimited list of a field names in the lookup data definition                                                                        
        Notes:     
        @param poLookup The ISmartBusinessEntityLookupDesign instance
        @return The comma delimited list of a field names in the lookup data definition                                                                               
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED STATIC CHARACTER GetLookupFieldNames (poLookup AS ISmartBusinessEntityLookupDesign):
        
        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO . 
        
        DEFINE VARIABLE hQuery   AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hDataset AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cExcept  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE j        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE n        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hBuffer  AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFields  AS CHARACTER NO-UNDO.

        /* Request empty dataset from OERA Backend */
        oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .
        
        oServiceAdapter:FetchDataset ("":U, poLookup:LookupEntityName,
                                      OUTPUT DATASET-HANDLE hDataset BY-REFERENCE) . 

        ASSIGN hQuery = SmartBusinessEntityQuerySupport:PrepareBindingQuery (hDataset, 
                                                                             poLookup:LookupEntityTable,
                                                                             poLookup:LookupEntityView,
                                                                             TRIM (FILL("YES,":U, NUM-ENTRIES(poLookup:LookupEntityView)), ",":U),
                                                                             TRUE,
                                                                             OUTPUT cExcept) .

        DO i = 1 TO hQuery:NUM-BUFFERS:
            ASSIGN hBuffer = hQuery:GET-BUFFER-HANDLE (i) .
            
            DO j = 1 TO hBuffer:NUM-FIELDS:
                IF ListHelper:EntryIsInList (hBuffer:NAME + ".":U + hBuffer:BUFFER-FIELD (j):NAME, cExcept) THEN 
                    NEXT .
                    
                IF hBuffer:BUFFER-FIELD (j):EXTENT > 0 THEN 
                DO n = 1 TO hBuffer:BUFFER-FIELD (j):EXTENT:
                    ASSIGN cFields = cFields + SUBSTITUTE ("&1[&2]":U, hBuffer:BUFFER-FIELD (j):NAME, n) + ",":U .
                END.
                ELSE                       
                    ASSIGN cFields = cFields + hBuffer:BUFFER-FIELD (j):NAME + ",":U .
            END.
        END.

        RETURN TRIM (cFields, ",":U) .

        FINALLY:
            IF VALID-HANDLE (hQuery) THEN 
                DELETE OBJECT hQuery . 
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 
        END.        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects a Lookup Dialog Class Name                                                                      
        Notes:   Method is VOID because of WAIT-FOR     
        @param pcClassName OUTPUT The name of the Lookup Dialog class name                                                                  
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SelectLookupDialogClassName (OUTPUT pcClassName AS CHARACTER):

        DEFINE VARIABLE oForm         AS Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm NO-UNDO .
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult                               NO-UNDO .

        DEFINE VARIABLE oInfo         AS Infragistics.Win.Misc.UltraDesktopAlertWindowInfo               NO-UNDO . 

        oInfo = Consultingwerk.SmartComponents.Support.DesktopAlertHelper:Show ("SmartComponent Library",
                                                                                "Scanning Project Folder(s) for Lookup Dialogs.") .

        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath ("Consultingwerk.SmartComponents.Interfaces.ISmartBusinessEntityLookupDialog":U, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        oForm = NEW Consultingwerk.SmartComponents.Support.BusinessEntityPickerForm () .
        
        oForm:ReceiveBusinessEntityNames (TABLE ttClassNames) .
        
/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog(oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual(oDialogResult, 
                                             System.Windows.Forms.DialogResult:Ok) THEN DO:
                                    
            ASSIGN pcClassName = oForm:GetSelectedBusinessEntityName () .
        END.
        ELSE 
            ASSIGN pcClassName = ? .                                                

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes the Lookup Designer                                                                         
        Notes:                      
        @param poLookup The ISmartBusinessEntityLookupDesign instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowLookupDesignerSupport (poLookup AS ISmartBusinessEntityLookupDesign):
        
        DEFINE VARIABLE oForm         AS BusinessEntityLookupDesignerForm  NO-UNDO . 
        DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO .
        DEFINE VARIABLE cFieldNames   AS CHARACTER                         NO-UNDO . 
        
        IF poLookup:LookupEntityName > "":U AND 
           poLookup:LookupEntityTable > "":U THEN .
        ELSE DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage 
                ("Please select BusinessEntity and Table first!", 
                 "SmartBusinessEntityLookup Designer") .            
            
            RETURN .
        END.   
        
        ASSIGN cFieldNames = SmartLookupDesignerSupport:GetLookupFieldNames (poLookup) .
        
        oForm = NEW BusinessEntityLookupDesignerForm () .
        
        ASSIGN oForm:LookupEntityTable       = poLookup:LookupEntityTable
               oForm:LookupFields            = cFieldNames 
               oForm:ParentViewerControls    = GetAllControls (poLookup) 
               oForm:KeyField                = poLookup:LookupKeyField
               oForm:KeyValueColumn          = poLookup:LookupKeyValueColumn 
               oForm:ControlList             = poLookup:LookupControls
               oForm:FieldList               = poLookup:LookupFields
               oForm:LookupQueryString       = poLookup:LookupQueryString
               oForm:LookupDialogQueryString = poLookup:LookupDialogQueryString
               oForm:LookupDialogTitle       = poLookup:LookupDialogTitle
               oForm:FilterFields            = poLookup:LookupDialogFilterFields
               oForm:FilterOperators         = poLookup:LookupDialogFilterOperators
               oForm:BrowseColumns           = poLookup:LookupBrowserFields 
               .

/*        WAIT-FOR oForm:ShowDialog () SET oDialogResult .*/

        /* Mike Fechner, Consultingwerk Ltd. 14.07.2010
           WAIT-FOR Workaround */
        ASSIGN oDialogResult = Consultingwerk.SmartComponents.FormHelper:ShowDialog (oForm) .
        
        IF Progress.Util.EnumHelper:AreEqual (System.Windows.Forms.DialogResult:OK, 
                                              oDialogResult) THEN DO:
            
            ASSIGN poLookup:LookupKeyField              = oForm:KeyField  
                   poLookup:LookupKeyValueColumn        = oForm:KeyValueColumn                                    
                   poLookup:LookupControls              = oForm:ControlList  
                   poLookup:LookupFields                = oForm:FieldList 
                   poLookup:LookupQueryString           = oForm:LookupQueryString        
                   poLookup:LookupDialogQueryString     = oForm:LookupDialogQueryString  
                   poLookup:LookupDialogTitle           = oForm:LookupDialogTitle       
                   poLookup:LookupDialogFilterFields    = oForm:FilterFields 
                   poLookup:LookupDialogFilterOperators = oForm:FilterOperators
                   poLookup:LookupBrowserFields         = oForm:BrowseColumns         
               .
               
        END.                                                 
                
        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = FALSE . 
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .    
            Consultingwerk.Util.ErrorHelper:ErrorMessageDialogAllowed = TRUE . 
        END CATCH.
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Shows the TablePickerDialog for a Lookup                                                                         
        Notes:           
        @param poLookup The ISmartBusinessEntityLookupDesign instance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowTablePickerDialog (poLookup AS ISmartBusinessEntityLookupDesign):
        
        DEFINE VARIABLE hDataset        AS HANDLE                              NO-UNDO.
        DEFINE VARIABLE lcSchema        AS LONGCHAR                            NO-UNDO.
        DEFINE VARIABLE cTables         AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE cEntityJoin     AS CHARACTER                           NO-UNDO INIT "?":U.
        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO . 
        
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        
        oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .
        
        oServiceAdapter:FetchDataset ("":U, poLookup:LookupEntityName,
                                      OUTPUT DATASET-HANDLE hDataset) . 

        IF NOT VALID-HANDLE (hDataset) THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage 
                ("Unable to start table picker dialog without a valid Dataset.", 
                 "SmartBusinessEntityLookup Designer") .              
            
            RETURN . 
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataset,
                 TRIM (SUBSTITUTE ("&1,&2":U, poLookup:LookupEntityTable, poLookup:LookupEntityView), ",":U), 
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN DO:            
            ASSIGN poLookup:LookupEntityTable = ENTRY (1, cTables). 
            
            IF NUM-ENTRIES (cTables) > 1 THEN DO:
                ASSIGN 
                    ENTRY (1, cTables)     = "":U .
                    poLookup:LookupEntityView = TRIM (cTables, ",":U) .
            END.
            ELSE 
                poLookup:LookupEntityView = "":U .            
        END.

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 
        END FINALLY.
    END METHOD.

END CLASS. 

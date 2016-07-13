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
    File        : SmartDataObjectBindingSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 20 21:48:53 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Implementation.SmartBindingSource FROM PROPATH .
USING Consultingwerk.SmartComponents.Resources.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Progress.Data.* FROM ASSEMBLY .
USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Implementation.SmartDataObjectBindingSource 
    INHERITS SmartBindingSource :             
    
    /*------------------------------------------------------------------------------
        Purpose: If Property is set a File selection Dialog is Shown to select a
                 SmartDataObject .w file and set the property SmartDataObjectName
                 to the selected filename.
                 The Value of this Property is not changed but the Dialog is started
                 when temporarily set to TRUE.
        Notes:   Workaround to be able to start a Dialog from Propertygrid.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OpenSmartDataObject AS LOGICAL NO-UNDO INIT FALSE 
    GET:
        RETURN FALSE.         
    END GET.
    SET (arg AS LOGICAL):
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN .        
        
        IF arg = TRUE THEN .
        ELSE RETURN .       
        
        THIS-OBJECT:DesignerOpenSmartDataObject() .
    END SET.
    
    /*------------------------------------------------------------------------------
        Purpose: The Name of the SmartDataObject .
                 This Property can be set by hand (typing in a .w file) or using a
                 Dialog by setting the Property OpenSmartDataObject to TRUE.
        Notes:   
    ------------------------------------------------------------------------------*/
     DEFINE PUBLIC PROPERTY SmartDataObjectName AS CHARACTER NO-UNDO 
    GET.
    SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectBindingSource class                                                                      
        Notes:                                                                         
    ------------------------------------------------------------------------------*/
     CONSTRUCTOR PUBLIC SmartDataObjectBindingSource ():
        SUPER ().

        THIS-OBJECT:SetDesignerProperties () .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartDataObjectBindingSource class
        Notes: 
        @param poContainer The System.ComponentModel.IContainer used as the Container for this component
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartDataObjectBindingSource (poContainer AS System.ComponentModel.IContainer):
        
        SUPER (INPUT poContainer).

        THIS-OBJECT:SetDesignerProperties () .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Import the schema of a SmartDataObject defined by the Property
                 SmartDataObjectName.
        Notes:   This Method is called when the Property ImportSchema is set to 
                 TRUE.
                 The current TableSchema of the SmartDataObjectBindingSource
                 is cleared out and the new Schema out of the SmartDataObject 
                 is set.
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID DesignerImportSchema ():
        
        DEFINE VARIABLE hSDO          AS HANDLE                NO-UNDO.
        DEFINE VARIABLE hRowObject    AS HANDLE                NO-UNDO.
        DEFINE VARIABLE oRootTable    AS TableDesc             NO-UNDO.
        DEFINE VARIABLE oArrayFactory AS DatasetRelationParser NO-UNDO.
        DEFINE VARIABLE cExceptList   AS CHARACTER             NO-UNDO
            INIT "RowNum,RowIdent,RowMod,RowIdentIdx,RowUserProp":U .
        
        RUN VALUE (THIS-OBJECT:SmartDataObjectName) PERSISTENT SET hSDO 
            NO-ERROR .
            
        IF NOT VALID-HANDLE(hSDO) OR ERROR-STATUS:ERROR THEN DO:
            MESSAGE SmartComponentLibraryCustomizer:SmartDataObjectBindingSource_DesignerImportSchemaUnableToLaunchSDO SKIP(1)
                    ERROR-STATUS:GET-MESSAGE(1)
                VIEW-AS ALERT-BOX.
        END.
        
        RUN initializeObject IN hSDO . 
        
        {get RowObject hRowObject hSDO} .
                
        /* Clear the currently existing schema from the BindingSource */
        THIS-OBJECT:TableSchema = ? .
        oRootTable = NEW TableDesc("RowObject":U).
        
        oArrayFactory = NEW DatasetRelationParser().
        oRootTable:Columns = oArrayFactory:GetTableFieldArray (hRowObject, oRootTable, cExceptList).
        
        THIS-OBJECT:TableSchema = oRootTable .        
        
        MESSAGE SUBSTITUTE (SmartComponentLibraryCustomizer:SmartDataObjectBindingSource_DesignerImportSchemaSuccess, 
                            THIS-OBJECT:SmartDataObjectName)
            VIEW-AS ALERT-BOX TITLE SmartComponentLibraryCustomizer:SmartDataObjectBindingSource_DesignerImportSchemaSuccess_Title.
        
        FINALLY:
            IF VALID-HANDLE(hSDO) THEN 
                RUN destroyObject IN hSDO .
            
            IF VALID-HANDLE(hSDO) THEN
                APPLY "CLOSE":U TO hSDO .
                                
            IF VALID-HANDLE(hSDO) THEN 
                DELETE PROCEDURE hSDO . 
                         
        END FINALLY.
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Selects a SmartDataObject during design time                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID DesignerOpenSmartDataObject ():
        
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lOk       AS LOGICAL NO-UNDO.
        
        IF NOT THIS-OBJECT:DesignTime THEN 
            RETURN .        
        
        ASSIGN cFileName = THIS-OBJECT:SmartDataObjectName 
               lOk       = SmartDataObjectSupport:FileOpenSmartDataObject (INPUT-OUTPUT cFileName).

        IF NOT lOk THEN RETURN.              
        
        IF cFileName > "":U THEN          
            ASSIGN THIS-OBJECT:SmartDataObjectName = cFileName .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs                                                                       
        Notes:                            
        @param pcVerbText The text indicating the verb that was clicked                                                                       
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID OnVerbClicked (INPUT pcVerbText AS CHARACTER):
        
        SUPER:OnVerbClicked(INPUT pcVerbText).
        
        CASE pcVerbText:
            WHEN "Open SmartDataObject (SDO)":U THEN 
                DesignerOpenSmartDataObject() .                
        END CASE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets Properties to customize Visual Designer behaviour                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetDesignerProperties ():

        ASSIGN 
           THIS-OBJECT:DesignerVerbs = TRIM (THIS-OBJECT:DesignerVerbs + ",Open SmartDataObject (SDO)":U, ",":U) .

    END METHOD.    

END CLASS.

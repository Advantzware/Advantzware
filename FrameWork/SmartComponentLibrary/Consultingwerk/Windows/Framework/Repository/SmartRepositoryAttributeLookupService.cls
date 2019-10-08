/**********************************************Un************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartRepositoryAttributeLookupService
    Purpose     : Service that provides lookup capabilities for the SmartComponent
                  Library/SmartComponents.Web2 properties
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Apr 16 18:43:52 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH.
USING Consultingwerk.SmartComponents.Support.*      FROM PROPATH .
USING Consultingwerk.Util.*                         FROM PROPATH .
USING Consultingwerk.Windows.Framework.Repository.* FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*           FROM PROPATH .
USING Infragistics.Win.Misc.*                       FROM ASSEMBLY .
USING Progress.Lang.*                               FROM PROPATH .
USING System.Windows.Forms.*                        FROM ASSEMBLY.

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Repository.SmartRepositoryAttributeLookupService
    IMPLEMENTS ISmartRepositoryAttributeLookupService, IRepositoryAttributeLookupService:

    {Consultingwerk/Util/TempTables/ttClassNames.i}
    { Consultingwerk/SmartFramework/Repository/Class/dsDesignAttributeValue.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns a character attribute value
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @return The attribute value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER GetCharacterAttributeValue (pcAttributeLabel AS CHARACTER,
                                                           phDesignAttributeTable AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        CREATE BUFFER hBuffer FOR TABLE phDesignAttributeTable .

        hBuffer:FIND-FIRST (SUBSTITUTE ("where AttributeLabel = ~"&1~"", pcAttributeLabel)) NO-ERROR .

        IF hBuffer:AVAILABLE THEN
            RETURN hBuffer::CharacterValue .
        ELSE DO:
            ErrorHelper:ResetErrorStatus() .
            RETURN ? .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hBuffer) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the property Dialog
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param pcObjectTypeId The GUID of the Object Type
        @param pcObjectMasterId The GUID of the Object Master
        @param pcObjectInstanceId The GUID of the Obejct Instance
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @param poOwnerForm The reference to the Form to be used as the owner for the Dialog
        @param poValue INPUT-OUTPUT The value to update (BOX ABL primitive values)
        @return Logical value indicating if OK (true) or Cancel (false) was clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL PerformEntityNameLookup (pcAttributeLabel AS CHARACTER,
                                                   pcObjectTypeId AS CHARACTER,
                                                   pcObjectMasterId AS CHARACTER,
                                                   pcObjectInstanceId AS CHARACTER,
                                                   phDesignAttributeTable AS HANDLE,
                                                   poOwnerForm AS System.Windows.Forms.Form,
                                                   INPUT-OUTPUT poValue AS System.Object):

        DEFINE VARIABLE oForm         AS BusinessEntityPickerForm    NO-UNDO .
        DEFINE VARIABLE oInfo         AS UltraDesktopAlertWindowInfo NO-UNDO .
        DEFINE VARIABLE oDialogResult AS DialogResult                NO-UNDO .

        oInfo = DesktopAlertHelper:Show ("SmartFramework"{&TRAN},
                                         "Scanning Project Folder(s) for Business Entities."{&TRAN}) .

        ClassHelper:GetClassNamesInClassPath ("Consultingwerk.OERA.IBusinessEntity"{&TRAN},
                                              FALSE,
                                              OUTPUT TABLE ttClassNames BY-REFERENCE) .

        oInfo:Close () .

        oForm = NEW BusinessEntityPickerForm () .
        oForm:Text = "Select Business Entity"{&TRAN} .
        oForm:ReceiveBusinessEntityNames(TABLE ttClassNames) .
        oForm:Owner = poOwnerForm .

        THIS-OBJECT:ShowDialog (oForm, OUTPUT oDialogResult) .

        {Consultingwerk/check-dialogresult-ok.i oDialogResult FALSE}

        poValue = BOX (oForm:GetSelectedBusinessEntityName()) .

        RETURN TRUE .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Displays the property Dialog
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param pcObjectTypeId The GUID of the Object Type
        @param pcObjectMasterId The GUID of the Object Master
        @param pcObjectInstanceId The GUID of the Obejct Instance
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @param poOwnerForm The reference to the Form to be used as the owner for the Dialog
        @param poValue INPUT-OUTPUT The value to update (BOX ABL primitive values)
        @return Logical value indicating if OK (true) or Cancel (false) was clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL PerformEntityTableLookup (pcAttributeLabel AS CHARACTER,
                                                    pcObjectTypeId AS CHARACTER,
                                                    pcObjectMasterId AS CHARACTER,
                                                    pcObjectInstanceId AS CHARACTER,
                                                    phDesignAttributeTable AS HANDLE,
                                                    poOwnerForm AS System.Windows.Forms.Form,
                                                    INPUT-OUTPUT poValue AS System.Object):

        DEFINE VARIABLE cEntityName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityTable AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityView  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntityJoin  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hDataset     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cTables      AS CHARACTER NO-UNDO.

        IF pcAttributeLabel = "EntityTable":U THEN DO:
            ASSIGN cEntityName  = THIS-OBJECT:GetCharacterAttributeValue ("EntityName":U, phDesignAttributeTable)
                   cEntityTable = THIS-OBJECT:GetCharacterAttributeValue ("EntityTable":U, phDesignAttributeTable)
                   cEntityView  = THIS-OBJECT:GetCharacterAttributeValue ("EntityView":U, phDesignAttributeTable)
                   cEntityJoin  = THIS-OBJECT:GetCharacterAttributeValue ("EntityJoin":U, phDesignAttributeTable).
        END.
        ELSE DO:
            ASSIGN cEntityName  = THIS-OBJECT:GetCharacterAttributeValue ("LookupEntityName":U, phDesignAttributeTable)
                   cEntityTable = THIS-OBJECT:GetCharacterAttributeValue ("LookupEntityTable":U, phDesignAttributeTable)
                   cEntityView  = THIS-OBJECT:GetCharacterAttributeValue ("LookupEntityView":U, phDesignAttributeTable)
                   cEntityJoin  = THIS-OBJECT:GetCharacterAttributeValue ("LookupEntityJoin":U, phDesignAttributeTable).
        END.

        IF cEntityTable = ? THEN cEntityTable = "":U .
        IF cEntityView  = ? THEN cEntityView  = "":U .
        IF cEntityJoin  = ? THEN cEntityJoin  = "":U .

        ServiceInterface:FetchDataset (cEntityName, OUTPUT DATASET-HANDLE hDataset) .

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataset,
                 TRIM (SUBSTITUTE ("&1,&2":U, cEntityTable, cEntityView), ",":U),
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN DO:

            ASSIGN poValue = BOX (ENTRY (1, cTables)) .

            IF NUM-ENTRIES (cTables) > 1 THEN
                ASSIGN cEntityView = ListHelper:ListStartingEntry(cTables, 2) .

            IF pcAttributeLabel = "EntityTable":U THEN DO:
                THIS-OBJECT:SetAttributeValue ("EntityView":U, cEntityView, phDesignAttributeTable).
                THIS-OBJECT:SetAttributeValue ("EntityJoin":U, cEntityJoin, phDesignAttributeTable).
            END.
            ELSE DO:
                THIS-OBJECT:SetAttributeValue ("LookupEntityView":U, cEntityView, phDesignAttributeTable).
                THIS-OBJECT:SetAttributeValue ("LookupEntityJoin":U, cEntityJoin, phDesignAttributeTable).
            END.

            RETURN TRUE .
        END.

        RETURN FALSE .

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hDataset) .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets a character attribute value
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param pcAttributeValue The value for the attribute
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @return The attribute value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER SetAttributeValue (pcAttributeLabel AS CHARACTER,
                                                  pcAttributeValue AS CHARACTER,
                                                  phDesignAttributeTable AS HANDLE):

        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

        CREATE BUFFER hBuffer FOR TABLE phDesignAttributeTable .

        hBuffer:FIND-FIRST (SUBSTITUTE ("where AttributeLabel = ~"&1~"", pcAttributeLabel)) NO-ERROR .

        IF hBuffer:AVAILABLE THEN
            hBuffer::CharacterValue = pcAttributeValue .
        ELSE
            ErrorHelper:ResetErrorStatus() .

        FINALLY:
            GarbageCollectorHelper:DeleteObject(hBuffer) .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Invokes ShowDialog() on a .NET Form and returns the DialogResult         
                 as an output parameter                                                                        
        Notes:                              
        @param poForm The reference to the Form to invoke ShowDialog() on
        @param poDialogResult OUTPUT The DialogResult returned by the ShowDialog() method                                          
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ShowDialog (poForm AS System.Windows.Forms.Form,
                                      OUTPUT poDialogResult AS System.Windows.Forms.DialogResult):
        
        WAIT-FOR poForm:ShowDialog() SET poDialogResult.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the property Dialog
        Notes:
        @param pcAttributeLabel The label of the attribute
        @param pcObjectTypeId The GUID of the Object Type
        @param pcObjectMasterId The GUID of the Object Master
        @param pcObjectInstanceId The GUID of the Obejct Instance
        @param phDesignAttributeTable The handle of the temp-table with the attribute values
        @param poOwnerForm The reference to the Form to be used as the owner for the Dialog
        @param poValue INPUT-OUTPUT The value to update (BOX ABL primitive values)
        @return Logical value indicating if OK (true) or Cancel (false) was clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL ShowPropertyDialog (pcAttributeLabel AS CHARACTER,
                                              pcObjectTypeId AS CHARACTER,
                                              pcObjectMasterId AS CHARACTER,
                                              pcObjectInstanceId AS CHARACTER,
                                              phDesignAttributeTable AS HANDLE,
                                              poOwnerForm AS System.Windows.Forms.Form,
                                              INPUT-OUTPUT poValue AS System.Object):

        CASE pcAttributeLabel:
            WHEN "LookupEntityName":U OR WHEN "EntityName":U THEN
                RETURN THIS-OBJECT:PerformEntityNameLookup (pcAttributeLabel,
                                                            pcObjectTypeId,
                                                            pcObjectMasterId,
                                                            pcObjectInstanceId,
                                                            phDesignAttributeTable,
                                                            poOwnerForm,
                                                            INPUT-OUTPUT poValue) .

            WHEN "LookupEntityTable":U OR WHEN "EntityTable":U THEN
                RETURN THIS-OBJECT:PerformEntityTableLookup (pcAttributeLabel,
                                                             pcObjectTypeId,
                                                             pcObjectMasterId,
                                                             pcObjectInstanceId,
                                                             phDesignAttributeTable,
                                                             poOwnerForm,
                                                             INPUT-OUTPUT poValue) .


            OTHERWISE
                MessageFormHelper:ShowMessage("Unsupported Attribute."{&TRAN},
                                              "Attribute Lookup"{&TRAN}, MessageFormImages:ImageWarning) .
        END CASE .

        RETURN FALSE .

    END METHOD.

END CLASS.

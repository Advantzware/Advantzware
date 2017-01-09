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
    File        : ConfigurationSettingsManager
    Purpose     : Manages a set of serializable Configuration Setting objects
                  in an LRU List 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 04 22:23:47 CEST 2012
    Notes       : The configuration objects shoudl implement the ISerializable
                  interface to allow storing them in the Registry
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                    FROM PROPATH . 
USING Consultingwerk.Exceptions.*         FROM PROPATH . 
USING Consultingwerk.Framework.*          FROM PROPATH .  
USING Consultingwerk.Util.*               FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN 
USING Infragistics.Win.UltraWinToolbars.* FROM ASSEMBLY .  
&ENDIF
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Framework.ConfigurationSettingsManager: 
&IF DEFINED (DotNetAccessible) NE 0 &THEN 

    DEFINE TEMP-TABLE ttSettings NO-UNDO 
        FIELD PositionIndex AS INTEGER 
        FIELD Settings      AS Progress.Lang.Object 
        FIELD GUID          AS CHARACTER 
        INDEX PositionIndex IS PRIMARY PositionIndex 
        INDEX GUID          IS UNIQUE GUID .  

    /*------------------------------------------------------------------------------
        Purpose: Creates an instance of a settings object 
        Notes:   
        @param poClass The Progress.Lang.Class to create an instance for
        @return 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object CreateInstance (poClass AS Progress.Lang.Class):
        
        DEFINE VARIABLE oInstance AS Progress.Lang.Object NO-UNDO . 
        
        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """The Class of the Settings Object""{&TRAN}"} .
        
        /* New the instance */
        oInstance = poClass:New() .
        
        IF NOT TYPE-OF (oInstance, Consultingwerk.ISerializable) THEN 
            UNDO, THROW NEW InvalidParameterValueException ("poClass":U,
                                                            poClass:TypeName,
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .
        
        CREATE b_ttSettings.
        ASSIGN b_ttSettings.PositionIndex  = Consultingwerk.Const:MAX_INTEGER 
               b_ttSettings.GUID           = GUID 
               b_ttSettings.Settings       = oInstance .        

        RELEASE b_ttSettings . 

        THIS-OBJECT:RenumberSettings () .

        RETURN oInstance . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Get's a Setting object using a key
        Notes:   
        @param pcGUID The GUID representing a Settings Instance
        @return The Settings object represented by the GUID
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Progress.Lang.Object GetSettingByGUID (pcGUID AS CHARACTER):
        
        DEFINE BUFFER b_ttSettings FOR ttSettings .

        FIND ttSettings WHERE ttSettings.GUID = pcGUID NO-ERROR . 
        
        IF AVAILABLE ttSettings THEN 
            RETURN ttSettings.Settings .
        ELSE 
            UNDO, THROW NEW InvalidParameterValueException ("pcGUID":U,
                                                            pcGUID,
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .
                                                            
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the settings as a Dictionary with the GUID as the key and the
                 ToString() value of the setting object as the value
        Notes:   
        @return A CharacterDictionary containing the settings 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.Framework.Collections.CharacterDictionary GetSettings ():
        
        DEFINE VARIABLE oDictionary AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO.

        DEFINE BUFFER b_ttSettings FOR ttSettings .

        oDictionary = NEW Consultingwerk.Framework.Collections.CharacterDictionary ("":U, 
                                                                                    CHR (1),
                                                                                    "":U,
                                                                                    CHR (1)) .

        FOR EACH b_ttSettings BY b_ttSettings.PositionIndex:

            oDictionary:Add (b_ttSettings.GUID,
                             b_ttSettings.Settings:ToString()) .

        END.

        RETURN oDictionary .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves the given object to the beginning of the LRU list
        Notes:   
        @param poSettings The settings object to move to the beginning
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MoveToBeginningOfLru (poSettings AS Progress.Lang.Object):

        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        FIND b_ttSettings WHERE b_ttSettings.Settings = poSettings NO-ERROR .
        
        IF NOT AVAILABLE b_ttSettings THEN 
            UNDO, THROW NEW InvalidParameterValueException ("poSettings":U,
                                                            poSettings:ToString (), 
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .
        
        ASSIGN b_ttSettings.PositionIndex = Consultingwerk.Const:MIN_INTEGER . 
        
        RELEASE b_ttSettings . 
        
        THIS-OBJECT:RenumberSettings () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Moves the given object to the beginning of the LRU list
        Notes:   
        @param pcGUID The GUID of the settings instance to move to the front 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID MoveToBeginningOfLru (pcGUID AS CHARACTER):

        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        FIND b_ttSettings WHERE b_ttSettings.GUID = pcGUID NO-ERROR .
        
        IF NOT AVAILABLE b_ttSettings THEN 
            UNDO, THROW NEW InvalidParameterValueException ("pcGUID":U,
                                                            pcGUID, 
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .
        
        ASSIGN b_ttSettings.PositionIndex = Consultingwerk.Const:MIN_INTEGER . 
        
        RELEASE b_ttSettings . 
        
        THIS-OBJECT:RenumberSettings () .

    END METHOD .
        
    /*------------------------------------------------------------------------------
        Purpose: Populates a LRU Menu in an UltraToolbarsManager instance 
        Notes:   
        @param poToolsCollection The ToolsCollection to populate
        @param piItemCount The number of LRU menu items to create
        @param pcPrefix A prefix to be used for creating the Key
        @param plDeleteExistingFromRootTools Remove existing tools fomr the Root Tools collection first 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PopulateLruMenu (poToolsCollection AS ToolsCollection,
                                        piItemCount AS INTEGER,
                                        pcPrefix AS CHARACTER,
                                        plDeleteExistingFromRootTools AS LOGICAL):
        
        DEFINE VARIABLE oToolbarsManager AS UltraToolbarsManager                      NO-UNDO . 
        DEFINE VARIABLE oExisting        AS "Infragistics.Shared.IKeyedSubObject[]":U NO-UNDO . 
        DEFINE VARIABLE i                AS INTEGER                                   NO-UNDO .
        DEFINE VARIABLE cHotKey          AS CHARACTER                                 NO-UNDO .
        DEFINE VARIABLE oTool            AS ButtonTool                                NO-UNDO . 

        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolsCollection} .
        
        ASSIGN oToolbarsManager = poToolsCollection:ToolbarsManager .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i oToolbarsManager} .

        IF plDeleteExistingFromRootTools THEN DO:
            /* Copy to array first because we will be modifying the List */
            oExisting = {Consultingwerk/new-array.i Infragistics.Shared.IKeyedSubObject poToolsCollection:Count} . 
            
            poToolsCollection:CopyTo (oExisting, 0) .
            
            {Consultingwerk/foreach.i ToolBase oExistingTool in oExisting}
            
                IF oExistingTool :Key BEGINS pcPrefix THEN DO:
                    poToolsCollection:Remove (oExistingTool ) .
                    
                    oToolbarsManager:Tools:Remove (oExistingTool:SharedProps:RootTool) .
                END.
            END.
            
            DELETE OBJECT oExisting . 
        END.    
        
        FOR EACH b_ttSettings BY b_ttSettings.PositionIndex:
            ASSIGN i = i + 1 . 

            IF i < 10 THEN 
                ASSIGN cHotKey = STRING (i) .
            ELSE 
            IF i = 10 THEN 
                ASSIGN cHotKey = "0":U . 
            ELSE 
            IF i > 10 AND i <= 36 THEN 
                ASSIGN cHotKey = CHR (64 + i - 10) .
            ELSE 
                ASSIGN cHotKey = "":U . 

            oTool = NEW ButtonTool (SUBSTITUTE ("&1&2":U, pcPrefix, b_ttSettings.GUID)) .
            
            IF cHotKey > "":U THEN 
                oTool:SharedProps:Caption = SUBSTITUTE ("&&&1. &2":U, 
                                                        cHotKey,
                                                        b_ttSettings.Settings:ToString()) .
            ELSE                                                          
                oTool:SharedProps:Caption = b_ttSettings.Settings:ToString() .

            oToolbarsManager:Tools:Add (oTool) .
            poToolsCollection:AddTool (oTool:Key) .

            IF i >= piItemCount THEN LEAVE . 
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the given instance from the LRU list 
        Notes:   
        @param pcGUID The GUID of the settings object to remove from the LRU list
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveInstance  (pcGUID AS CHARACTER):

        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        FIND b_ttSettings WHERE b_ttSettings.GUID = pcGUID NO-ERROR .
        
        IF NOT AVAILABLE b_ttSettings THEN 
            UNDO, THROW NEW InvalidParameterValueException ("pcGUID":U,
                                                            pcGUID, 
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .

        DELETE b_ttSettings .       

        THIS-OBJECT:RenumberSettings () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Removes the given instance from the LRU list 
        Notes:   
        @param poSettings The settings object to remove from the LRU list
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RemoveInstance  (poSettings AS Progress.Lang.Object):

        DEFINE BUFFER b_ttSettings FOR ttSettings .
        
        FIND b_ttSettings WHERE b_ttSettings.Settings = poSettings NO-ERROR .
        
        IF NOT AVAILABLE b_ttSettings THEN 
            UNDO, THROW NEW InvalidParameterValueException ("poSettings":U,
                                                            poSettings:ToString (), 
                                                            "Consultingwerk.Framework.ConfigurationSettingsManager":U) .

        DELETE b_ttSettings .         

        THIS-OBJECT:RenumberSettings () .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Renumbers the settings from 1 on  
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID RenumberSettings ():
        
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
        DEFINE VARIABLE i      AS INTEGER NO-UNDO.

        DEFINE BUFFER b_ttSettings FOR ttSettings .

        hQuery = QueryHelper:CreatePreparedQuery (BUFFER b_ttSettings:HANDLE,
                                                  "PRESELECT EACH b_ttSettings BY PositionIndex":U) .

        DO WHILE NOT hQuery:QUERY-OFF-END:
            i = i + 1 . 
            
            ASSIGN b_ttSettings.PositionIndex = i . 
            
            hQuery:GET-NEXT () .
        END.

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (hQuery) .        
        END FINALLY.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Loads the LRU from the settings service 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RestoreFromSettingsService ():
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE iCount           AS INTEGER          NO-UNDO .
        DEFINE VARIABLE i                AS INTEGER          NO-UNDO .
        DEFINE VARIABLE oSerializable    AS ISerializable    NO-UNDO . 
        DEFINE VARIABLE lcSettings       AS LONGCHAR         NO-UNDO .

        DEFINE BUFFER b_ttSettings FOR ttSettings .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        EMPTY TEMP-TABLE ttSettings . 

        ASSIGN iCount = INTEGER (oSettingsService:GetSetting ("LRU":U,
                                                              "NumberOfProfiles":U)) .
        
        DO i = 1 TO iCount:

            ASSIGN lcSettings = oSettingsService:GetSetting ("LRU":U,
                                                             SUBSTITUTE ("Profile&1":U, i)) 
                                                             
                   /* JSON-Deserilization requires utf-8 */                                                             
                   lcSettings = CODEPAGE-CONVERT (lcSettings, "utf-8":U) .
                               
           oSerializable = Serializable:DeserializeInstance (lcSettings) . 

           CREATE b_ttSettings.
           ASSIGN b_ttSettings.PositionIndex  = i 
                  b_ttSettings.GUID           = GUID 
                  b_ttSettings.Settings       = oSerializable .             
        END.
        
    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Stores the current LRU to the settings service 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreToSettingsService ():
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE iCount           AS INTEGER          NO-UNDO .
        DEFINE VARIABLE oSerializable    AS ISerializable    NO-UNDO . 
        DEFINE VARIABLE lcSettings       AS LONGCHAR         NO-UNDO .

        DEFINE BUFFER b_ttSettings FOR ttSettings .

        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        FOR EACH b_ttSettings BY b_ttSettings.PositionIndex:
            
            ASSIGN iCount        = iCount + 1 
                   oSerializable = CAST (b_ttSettings.Settings, ISerializable) 
                   lcSettings    = oSerializable:Serialize() .

            oSettingsService:StoreSetting ("LRU":U,
                                           SUBSTITUTE ("Profile&1":U, iCount),
                                           STRING (lcSettings)) .
        END.

        oSettingsService:StoreSetting ("LRU":U,
                                       "NumberOfProfiles":U,
                                       STRING (iCount)) .
    END METHOD .
&ENDIF
END CLASS.

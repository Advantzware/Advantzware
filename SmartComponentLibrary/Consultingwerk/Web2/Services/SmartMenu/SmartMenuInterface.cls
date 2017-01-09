/**********************************************************************
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
    File        : SmartMenuInterface
    Purpose     : Service Interface for Menu Structure Requests
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jul 18 13:11:47 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                FROM PROPATH .
USING Consultingwerk.OERA.*                     FROM PROPATH .
USING Consultingwerk.Framework.*                FROM PROPATH .
USING Consultingwerk.SmartFramework.*           FROM PROPATH .
USING Consultingwerk.Util.*                     FROM PROPATH .
USING Consultingwerk.Web.*                      FROM PROPATH .
USING Consultingwerk.Web2.Services.SmartMenu.*  FROM PROPATH .
USING Consultingwerk.Web2.SmartFramework.Menu.* FROM PROPATH .
USING Progress.Json.ObjectModel.*               FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Web2.Services.SmartMenu.SmartMenuInterface:

    {Consultingwerk/SmartFramework/Menu/dsMenu.i}

    DEFINE VARIABLE oConfig AS IConfigurationProvider NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartMenuInterface class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuInterface ():

        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        SUPER ().

        FileAssert:Exists (".restapplicationsettings":U) .

        ASSIGN cFileName = FileHelper:FindFile (".restapplicationsettings":U) .

        ASSIGN oConfig = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider
                                                       "NEW ConfigurationProvider (cFileName)"} .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a menu item to the Items list
        Notes:   Processes SmartMenu records recursively
        @param poParentItem The parent menu item to add new Menus to
        @param pcParentGuid The guid of the parent menu item
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID AddSmartMenuItems (poParentItem AS SerializableMenuItem,
                                             pcParentGuid AS CHARACTER):

        DEFINE VARIABLE oItem                       AS SerializableMenuItem       NO-UNDO .
        DEFINE VARIABLE oRequest                    AS FetchDataRequest           NO-UNDO .
        DEFINE VARIABLE hDataset                    AS HANDLE                     NO-UNDO .
        DEFINE VARIABLE lcCallParameter             AS LONGCHAR                   NO-UNDO .
        DEFINE VARIABLE oCallParameter              AS IFunctionCallParameter     NO-UNDO .
        DEFINE VARIABLE oLaunchWebViewCallParameter AS LaunchWebViewCallParameter NO-UNDO .
        DEFINE VARIABLE cUrl                        AS CHARACTER                  NO-UNDO .

        DEFINE BUFFER bChild FOR eSmartMenu .
        DEFINE BUFFER eSmartMenu FOR eSmartMenu .

        FOR EACH eSmartMenu WHERE eSmartMenu.ParentMenuGuid = pcParentGuid:

            IF eSmartMenu.FunctionGuid > "":U THEN DO ON ERROR UNDO, THROW:

                oRequest = NEW FetchDataRequest ("eSmartFunction":U,
                                                 SUBSTITUTE ("FOR EACH eSmartFunction WHERE eSmartFunction.FunctionGuid = ~"&1~"":U, eSmartMenu.FunctionGuid),
                                                 1) .

                ServiceInterface:FetchData ("Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                            oRequest,
                                            OUTPUT DATASET-HANDLE hDataset) .

                hDataset::eSmartFunction:FIND-FIRST () NO-ERROR .

                IF hDataset::eSmartFunction:AVAILABLE THEN DO:

                    COPY-LOB FROM hDataset::eSmartFunction::FunctionCallParameter
                             TO lcCallParameter
                             CONVERT TARGET CODEPAGE "utf-8":U .

                    oCallParameter = CAST (Consultingwerk.Serializable:DeserializeInstance (lcCallParameter),
                                           IFunctionCallParameter) .
                END.
                ELSE
                    oCallParameter = ? .

                IF VALID-OBJECT (oCallParameter) AND TYPE-OF (oCallParameter, LaunchWebViewCallParameter) THEN DO:

                    oLaunchWebViewCallParameter = CAST (oCallParameter,
                                                        LaunchWebViewCallParameter) .

                    ASSIGN cUrl = oLaunchWebViewCallParameter:ViewUri .

                    IF oLaunchWebViewCallParameter:EntityName > "":U THEN
                        ASSIGN cUrl = cUrl + SUBSTITUTE ("?EntityName=&1&&EntityTable=&2&&EntityView=&3&&View=&4&&Title=&5&&&6":U,
                                                         oLaunchWebViewCallParameter:EntityName,
                                                         oLaunchWebViewCallParameter:EntityTable,
                                                         oLaunchWebViewCallParameter:EntityViewTables,
                                                         oLaunchWebViewCallParameter:EntityView,
                                                         WebUtilities:UrlEncode(oLaunchWebViewCallParameter:ViewTitle,
                                                                                UrlEncodeTypeEnum:Query),
                                                         oLaunchWebViewCallParameter:Parameter) .
                    ELSE IF oLaunchWebViewCallParameter:Parameter > "" THEN
                        ASSIGN cUrl = cUrl + "?":U + oLaunchWebViewCallParameter:Parameter .

                    oItem = NEW SerializableMenuItem (eSmartMenu.MenuName,
                                                      eSmartMenu.MenuSmallImage,
                                                      cUrl,
                                                      REPLACE (REPLACE (eSmartMenu.MenuGuid, "-":U, "":U), ":":U, "":U)) .

                    poParentItem:Items:Add (oItem) .
                END.

                ELSE DO:
                    oItem = NEW SerializableMenuItem (eSmartMenu.MenuName, eSmartMenu.MenuSmallImage) .

                    poParentItem:Items:Add (oItem) .
                END.

                FINALLY:
                    IF VALID-HANDLE (hDataset) THEN
                        DELETE OBJECT hDataset .

                    hDataset = ? .
                END FINALLY.
            END.
            ELSE DO:
                oItem = NEW SerializableMenuItem (eSmartMenu.MenuName, eSmartMenu.MenuSmallImage) .

                poParentItem:Items:Add (oItem) .
            END.

            IF CAN-FIND (FIRST bChild WHERE bChild.ParentMenuGuid = eSmartMenu.MenuGuid) THEN
                THIS-OBJECT:AddSmartMenuItems (oItem, eSmartMenu.MenuGuid) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returs a menu structure
        Notes:
        @param pcMenuStructureId The ID (guid) of the menu structure to return (optional)
        @param pcJSON The JSON string representing the menu structure
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    METHOD PUBLIC VOID GetMenuStructure (pcMenuStructureId AS CHARACTER,
                                         OUTPUT pcJSON AS LONGCHAR):

        pcJSON = THIS-OBJECT:GetMenuItem (pcMenuStructureId):Serialize() .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returs a menu structure
        Notes:
        @param pcMenuStructureId The ID (guid) of the menu structure to return (optional)
        @return The JSON object representing the menu structure
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC JsonObject GetMenuStructure (pcMenuStructureId AS CHARACTER):

        RETURN THIS-OBJECT:GetMenuItem (pcMenuStructureId):ToJsonObject () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returs a menu structure's root Menu Item
        Notes:
        @param pcMenuStructureId The ID (guid) of the menu structure to return (optional)
        @return The JSON object representing the menu structure
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED SerializableMenuItem GetMenuItem (pcMenuStructureId AS CHARACTER):

        DEFINE VARIABLE oRoot           AS SerializableMenuItem      NO-UNDO .
        DEFINE VARIABLE oDemo           AS SerializableMenuItem      NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER                   NO-UNDO .
        DEFINE VARIABLE cGuids          AS CHARACTER                 NO-UNDO .
        DEFINE VARIABLE cLabels         AS CHARACTER                 NO-UNDO .

        DEFINE VARIABLE oParameter AS GetMenuStructureParameter NO-UNDO .

        ASSIGN cGuids  = oConfig:GetValue ("Web2MenuGuids":U)
               cLabels = oConfig:GetValue ("Web2MenuLabels":U) .

        oRoot = NEW SerializableMenuItem () .
        oRoot:Label = "SmartComponents.Web2 Demo"{&TRAN} .
        oRoot:Image = "Consultingwerk/Demo/earth.png":U .

        DO i = 1 TO NUM-ENTRIES (cGuids):

            /* Web2 Demo */
            oDemo = NEW SerializableMenuItem () .
            oDemo:Label = ENTRY (i, cLabels) .
            oDemo:Image = "":U .

            oRoot:Items:Add (oDemo) .

            DATASET dsMenu:EMPTY-DATASET () .

            oParameter = NEW GetMenuStructureParameter () .
            oParameter:IncludeRootMenuEntry = TRUE .
            oParameter:NumberOfLevels = 0 .
            oParameter:RootMenuKey = ENTRY (i, cGuids).

            ServiceInterface:InvokeMethod ("Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U,
                                           "GetMenuStructure":U,
                                           INPUT-OUTPUT DATASET dsMenu,
                                           oParameter) .

            THIS-OBJECT:AddSmartMenuItems (oDemo, ENTRY (i, cGuids)) .
        END.

        RETURN oRoot .

    END METHOD .

END CLASS.

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
    File        : StyleLibraryHelper
    Purpose     : Supporting routines for working with Infragistics Style
                  Libraries (.isl files)
    Syntax      : Static methods only, Protected constructor to disallow
                  instance creation
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 03 11:59:03 CET 2011
    Notes       : Allows synchronization of the Progress COLOR-TABLE entries
                  with the current loaded Infragistics AppStyle (AppStylist)

                  This file is intended for GUI for .NET only
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW .

{ Consultingwerk/products.i }

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING Infragistics.Win.* .
USING Infragistics.Win.AppStyling.* .
USING Infragistics.Win.AppStyling.Runtime.* .
USING Progress.Lang.*.
&ENDIF

CLASS Consultingwerk.Util.StyleLibraryHelper:

    /*------------------------------------------------------------------------------
        Purpose: Protected default constructor.
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PROTECTED StyleLibraryHelper (  ):
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the Appearance object from the current style library for
                 the passed in UI role and state for the Default StyleSet
        Notes:
        @param pcUIRole The UI role
        @param pcUIState The RoleState
        @return The style's appearance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC AppearanceBase GetAppearance (pcUIRole AS CHARACTER,
                                                       pcUIState AS RoleState):

        RETURN GetAppearance (pcUIRole, pcUIState, ?) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Appearance object from the current style library for
                 the passed in UI role and state and the passed in StyleSet
        Notes:   Pass in ? for the StyleSet to access the Default Styles
        @param pcUIRole The UI role
        @param poUIState The RoleState
        @param pcStyleSet The style set name
        @return The style's appearance
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC AppearanceBase GetAppearance (pcUIRole AS CHARACTER,
                                                       poUIState AS RoleState,
                                                       pcStyleSet AS CHARACTER):

        DEFINE VARIABLE oStyleLibrary AS Infragistics.Win.AppStyling.Runtime.ApplicationStyleLibrary NO-UNDO.
        DEFINE VARIABLE oStyleSet     AS Infragistics.Win.AppStyling.Runtime.StyleSetSettings        NO-UNDO.
        DEFINE VARIABLE oRole         AS Infragistics.Win.AppStyling.Runtime.StyleSettings           NO-UNDO.
        DEFINE VARIABLE oNormalState  AS Infragistics.Win.AppStyling.Runtime.StateSettings           NO-UNDO.

        oStyleLibrary = NEW Infragistics.Win.AppStyling.Runtime.ApplicationStyleLibrary ().
        oStyleLibrary:LoadFromStyleManager ().

        IF pcStyleSet > "":U THEN .
        ELSE
            pcStyleSet = oStyleLibrary:DefaultStyleSet .

        ASSIGN oStyleSet = oStyleLibrary:StyleSets[pcStyleSet] NO-ERROR .

        IF NOT VALID-OBJECT (oStyleSet) THEN
            RETURN ? .

        ASSIGN
            oRole        = oStyleSet:RoleStyles:GetStyle (pcUIRole, TRUE)
            oNormalState = IF (oRole:States:Exists (poUIState)) THEN
                               oRole:States[poUIState]
                           ELSE
                               oRole:States:Add (poUIState).

         RETURN oNormalState:Appearance .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the background color of an appearance object
        Notes:   Maps Infragistics (Office 2007) named colors
        @param pcUIRole The UI role
        @param poUIState The RoleState
        @param pcStyleSet The style set name
        @return The background color of the style
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC System.Drawing.Color GetColor (pcUIRole AS CHARACTER,
                                                        poUIState AS RoleState,
                                                        pcStyleSet AS CHARACTER):

        DEFINE VARIABLE oAppearance AS Infragistics.Win.AppearanceBase NO-UNDO .
        DEFINE VARIABLE oColor      AS System.Drawing.Color            NO-UNDO .

        ASSIGN oAppearance = Consultingwerk.Util.StyleLibraryHelper:GetAppearance (pcUIRole, poUIState, pcStyleSet)
               oColor      = oAppearance:BackColor .

        IF NOT oColor:Equals (System.Drawing.Color:Black) AND
            oColor:R = 0 AND
            oColor:G = 0 AND
            oColor:B = 0 THEN

            oColor = Infragistics.Win.AppStyling.NamedColor:GetActualColor (oColor) .

        RETURN oColor .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads a StyleLibrary from a MEM-PTR (BLOB)
        Notes:   The caller is responsible for releasing the memory afterwards
        @param pmPtr The MEM-PTR pointing to the BLOB Data
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LoadFromBlob (pmPtr AS MEMPTR):

        DEFINE VARIABLE oBytes  AS "System.Byte[]":U      NO-UNDO .
        DEFINE VARIABLE oStream AS System.IO.MemoryStream NO-UNDO .

        oBytes = Consultingwerk.Util.DataTypeHelper:MemptrToByteArray(pmPtr) .
        oStream = NEW System.IO.MemoryStream (oBytes) .

        Infragistics.Win.AppStyling.StyleManager:Load (oStream) .

        oStream:Close () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads a StyleLibrary from a file
        Notes:
        @param pcFileName The file name (relative, propath or absolute) to load the style from
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID LoadFromFile (pcFileName AS CHARACTER):

        FILE-INFORMATION:FILE-NAME = pcFileName .

        IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Invalid style library file: &1"{&TRAN}, pcFileName), 0) .

        Infragistics.Win.AppStyling.StyleManager:Load (FILE-INFORMATION:FULL-PATHNAME) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Maps a Progress color table entry to a System.Drawing.Color reference
        Notes:
        @param piColor The progress color table number
        @param poColor The Color to assign to the Progress color table
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetColorTable (piColor AS INTEGER,
                                             poColor AS System.Drawing.Color):

        IF COLOR-TABLE:NUM-ENTRIES < 255 THEN
            COLOR-TABLE:NUM-ENTRIES = 255 .

        IF NOT poColor:Equals (System.Drawing.Color:Black) AND
            poColor:R = 0 AND
            poColor:G = 0 AND
            poColor:B = 0 THEN

            poColor = Infragistics.Win.AppStyling.NamedColor:GetActualColor (poColor) .

        COLOR-TABLE:SET-DYNAMIC (piColor, TRUE) .
        COLOR-TABLE:SET-RGB-VALUE (piColor, RGB-VALUE (poColor:R, poColor:G, poColor:B)) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Dynamically sets WinKit reserved color table entries to colors from
                 the AppStylist ISL file currently in use
        Notes:   Color 232:       UltraPanel                      BGCOLOR
                 Color 233:       EditorControl / Normal          BGCOLOR
                 Color 234:       EditorControl / ReadOnly        BGCOLOR
                 Color 235:       GridRow / Normal                BGCOLOR
                 Color 236:       GridColumnHeader / Normal       BGCOLOR
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetProgressColorTable ():

        SetProgressColorTable (232, "UltraPanel":U, RoleState:Normal, ?) .
        SetProgressColorTable (235, "GridRow":U, RoleState:Normal, ?) .
        SetProgressColorTable (233, "EditorControl":U, RoleState:Normal, ?) .
        SetProgressColorTable (234, "EditorControl":U, RoleState:ReadOnly, ?) .
        SetProgressColorTable (236, "GridColumnHeader":U, RoleState:Normal, ?) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Dynamically sets a color entry in the Progress color table to a
                 color defined in the AppStylist .isl file
        Notes:   piColorNum:  The Progress Color Table end to set
                 pcUIRole:    The Role in the StyleSet
                 pcRoleState: The RoleState (Enum)
                 pcStyleSet:  THe StyleSetName
        @param piColorNum The Progress color table entry
        @param pcUIRole The UI role
        @param poRoleState The RoleState
        @param pcStyleSet The style set name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetProgressColorTable (piColorNum AS INTEGER,
                                                     pcUIRole AS CHARACTER,
                                                     poRoleState AS RoleState,
                                                     pcStyleSet AS CHARACTER):

        DEFINE VARIABLE oAppearance AS AppearanceBase NO-UNDO .

        ASSIGN oAppearance = GetAppearance (pcUIRole,
                                            poRoleState,
                                            pcStyleSet) .

        IF VALID-OBJECT (oAppearance) AND
           NOT oAppearance:BackColor:IsEmpty THEN
               SetColorTable (piColorNum, oAppearance:BackColor) .

     END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Return if the currently loaded Style Library contains a StyleSet with
                 the given name
        Notes:
        @param pcStyleSetName The Styleset name to test for
        @return Logical value indicating if the given StyleSet name exists
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL StyleSetNameExists (pcStyleSetName AS CHARACTER):

        DEFINE VARIABLE oStyleLibrary AS Infragistics.Win.AppStyling.Runtime.ApplicationStyleLibrary NO-UNDO.

        oStyleLibrary = NEW Infragistics.Win.AppStyling.Runtime.ApplicationStyleLibrary ().
        oStyleLibrary:LoadFromStyleManager ().

        RETURN oStyleLibrary:StyleSets:Exists (pcStyleSetName) .

    END METHOD .

&ENDIF
END CLASS.

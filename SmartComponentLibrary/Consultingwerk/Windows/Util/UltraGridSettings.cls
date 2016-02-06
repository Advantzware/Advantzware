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
    File        : UltraGridSettings
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 07 16:47:02 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*           FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.* FROM ASSEMBLY .
USING Progress.Lang.*                 FROM ASSEMBLY .

CLASS Consultingwerk.Windows.Util.UltraGridSettings: 

    DEFINE PRIVATE STATIC TEMP-TABLE ttColumnSettings NO-UNDO
        FIELD OwningInstance AS Progress.Lang.Object   
        FIELD ColumnKey      AS CHARACTER  
        FIELD ColumnWith     AS INTEGER
        FIELD ColumnPosition AS INTEGER 
        FIELD ColumnHidden   AS LOGICAL
        INDEX ColumnPosition IS UNIQUE PRIMARY OwningInstance ColumnPosition .   
                
    /*------------------------------------------------------------------------------
        Purpose: The reference to the Grid                                                                     
        Notes:                                                              
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Grid AS UltraGrid NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Creates a new instance of the UltraGridSettings class                                                                        
        Notes:                                                              
        @param poGrid The Grid to work with          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UltraGridSettings (poGrid AS UltraGrid):
        SUPER ().
        
        THIS-OBJECT:Grid = poGrid . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Applies stored column settings to an UltraGrid instance                                                                       
        Notes:   Settings are expected in the form of a pipe (|) delimited list of
                 ColumnKey,With,Position,Hidden per column
        @param pcSettings The stored settings                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ApplyStoredSettings (pcSettings AS CHARACTER):

        DEFINE VARIABLE oColumn AS UltraGridColumn NO-UNDO .
            
        ParseColumnSettings (pcSettings) .
        
        FOR EACH ttColumnSettings WHERE ttColumnSettings.OwningInstance = THIS-OBJECT
                                     BY ttColumnSettings.ColumnPosition:
                              
            oColumn = THIS-OBJECT:Grid:DisplayLayout:Bands[0]:Columns[ttColumnSettings.ColumnKey] NO-ERROR .

            IF VALID-OBJECT (oColumn) THEN DO:
                IF ttColumnSettings.ColumnWith > 0 THEN ASSIGN oColumn:Width = ttColumnSettings.ColumnWith .  
                IF ttColumnSettings.ColumnPosition >= 0 THEN ASSIGN oColumn:Header:VisiblePosition = ttColumnSettings.ColumnPosition . 

                IF Progress.Util.EnumHelper:AreEqual (oColumn:ExcludeFromColumnChooser,
                                                      ExcludeFromColumnChooser:True) THEN 
                     oColumn:Hidden = TRUE . 
                ELSE oColumn:Hidden = ttColumnSettings.ColumnHidden .                                                     
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clears the ColumnSettings table records of the current object instance                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ClearColumnSettings ():
        
        FOR EACH ttColumnSettings WHERE ttColumnSettings.OwningInstance = THIS-OBJECT:
            DELETE ttColumnSettings .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses the ColumnSettings an stored them in the ttColumnSettings 
                 temp-table                                                                        
        Notes:                                                                        
        @param pcSettings The stored settings                                                                     
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseColumnSettings (pcSettings AS CHARACTER):

        DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO.
        
        ClearColumnSettings () .
        
        DO i = 1 TO NUM-ENTRIES (pcSettings, "|":U):
            ASSIGN cEntry = ENTRY (i, pcSettings, "|":U) .
        
            IF NUM-ENTRIES (cEntry) < 4 THEN NEXT .        
        
            CREATE ttColumnSettings .
            ASSIGN ttColumnSettings.OwningInstance = THIS-OBJECT    
                   ttColumnSettings.ColumnKey      = ENTRY (1, cEntry)   
                   ttColumnSettings.ColumnWith     = INTEGER (ENTRY (2, cEntry))
                   ttColumnSettings.ColumnPosition = INTEGER (ENTRY (3, cEntry))
                   ttColumnSettings.ColumnHidden   = (ENTRY (4, cEntry) = "yes":U) 
                NO-ERROR .
        END . 

        ERROR-STATUS:ERROR = FALSE . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the settings string that can be used to restore the grid layout                                                                       
        Notes:      
        @return The settings string that can be used to restore the grid layout                                                               
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER StoreColumnSettings ():
        
        DEFINE VARIABLE cSettings AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lHidden   AS LOGICAL   NO-UNDO.
    
        {Consultingwerk/foreach.i UltraGridColumn oColumn in THIS-OBJECT:Grid:DisplayLayout:Bands[0]:Columns}
            
            IF oColumn:IsGroupByColumn THEN
                ASSIGN lHidden = FALSE .
            ELSE 
                ASSIGN lHidden = oColumn:Hidden . 
                        
            ASSIGN cSettings = cSettings + "|":U + 
                               SUBSTITUTE ("&1,&2,&3,&4":U,
                                           oColumn:Key,
                                           oColumn:Width,
                                           oColumn:Header:VisiblePosition,
                                           lHidden) .
        END.
        
        RETURN TRIM (cSettings, "|":U) .
        
    END METHOD.

END CLASS.

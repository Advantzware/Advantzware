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
    File        : SmartComboEditorSupport
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Mar 1 21:25:25 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Support.*        FROM PROPATH .
USING Progress.Lang.*                                 FROM ASSEMBLY .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Support.SmartComboEditorSupport: 

    {Consultingwerk/Util/TempTables/ttClassNames.i &ACCESS="PRIVATE STATIC"}

    /*------------------------------------------------------------------------------
        Purpose: Shows the TablePickerDialog for a SmartComboEditor                                                                         
        Notes:                            
        @param poCombo The reference to the SmartCompoEditor                                             
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID ShowTablePickerDialog (poCombo AS SmartComboEditor):
        
        DEFINE VARIABLE hDataset        AS HANDLE                              NO-UNDO.
        DEFINE VARIABLE lcSchema        AS LONGCHAR                            NO-UNDO.
        DEFINE VARIABLE cTables         AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE cEntityJoin     AS CHARACTER                           NO-UNDO INIT "?":U.
        DEFINE VARIABLE oServiceAdapter AS Consultingwerk.OERA.IServiceAdapter NO-UNDO . 
        
        DEFINE VARIABLE i        AS INTEGER NO-UNDO.
        
        oServiceAdapter = Consultingwerk.Framework.FrameworkSettings:ServiceAdapter .
        
        oServiceAdapter:FetchDataset ("":U, 
                                      poCombo:EntityName,
                                      OUTPUT DATASET-HANDLE hDataset) . 

        IF NOT VALID-HANDLE (hDataset) THEN DO:
            Consultingwerk.Util.MessageFormHelper:ShowMessage 
                ("Unable to start table picker dialog without a valid Dataset.", 
                 "SmartComboEditor Designer") .              
            
            RETURN . 
        END.

        RUN Consultingwerk/SmartComponents/Support/SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
                (hDataset,
                 poCombo:EntityTable, 
                 OUTPUT cTables,
                 INPUT-OUTPUT cEntityJoin) .

        IF cTables > "":U THEN 
            ASSIGN poCombo:EntityTable = ENTRY (1, cTables). 

        FINALLY:
            IF VALID-HANDLE (hDataset) THEN 
                DELETE OBJECT hDataset . 
        END FINALLY.
    END METHOD.

END CLASS. 

/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : Changes
    Purpose     : Returns information about change lists
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Nov 13 11:50:52 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Studio.P4.*             FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH.

CLASS Consultingwerk.Studio.P4.Changes: 

    { Consultingwerk/Studio/P4/ttChangeLists.i }

    /*------------------------------------------------------------------------------
        Purpose: Returns a temp-table of change lists
        Notes:   
        @param poStatus The ChangeListStatusEnum with the change list status to return or ? for any status
        @param piMax The Maximum number of Change lists
        @param ttChangeLists OUTPUT The temp-table with the change list descriptions
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetChanges (poStatus AS ChangeListStatusEnum,
                                   piMax AS INTEGER,
                                   OUTPUT TABLE ttChangeLists):
        
        DEFINE VARIABLE cCommand   AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oTokenizer AS Tokenizer     NO-UNDO . 
        DEFINE VARIABLE cLine      AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oList      AS CharacterList NO-UNDO.
        
        EMPTY TEMP-TABLE ttChangeLists . 
        
        ASSIGN cCommand = "p4 changes -t ":U .
        
        IF VALID-OBJECT (poStatus) THEN 
            ASSIGN cCommand = cCommand + SUBSTITUTE ("-s &1 ":U, LC (STRING (poStatus))) .

        IF piMax > 0 THEN 
            ASSIGN cCommand = cCommand + SUBSTITUTE ("-m &1 ":U, piMax) .

        oTokenizer = NEW Tokenizer () . 

        INPUT THROUGH VALUE (cCommand) .

        REPEAT:

            IMPORT UNFORMATTED cLine . 

            IF cLine > "":U THEN DO: 
                oList = oTokenizer:Tokenize (cLine) .            

                CREATE ttChangeLists .
                ASSIGN ttChangeLists.ChangeList        = INTEGER (oList:GetValue (2)) 
                       ttChangeLists.DateString        = oList:GetValue (4)
                       ttChangeLists.TimeString        = oList:GetValue (5)
                       ttChangeLists.UserName          = oList:GetValue (7)
                       ttChangeLists.ChangeDescription = oList:GetValue (8) .
            END.
        END.

    END METHOD .

END CLASS.

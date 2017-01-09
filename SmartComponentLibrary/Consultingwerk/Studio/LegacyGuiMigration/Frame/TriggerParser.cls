/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : TriggerParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 15 23:21:50 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.Frame.* FROM PROPATH .  
USING Consultingwerk.Studio.Proparse.*                 FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH . 
USING Progress.Lang.*                                  FROM PROPATH .
USING org.prorefactor.refactor.*                       FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                     FROM ASSEMBLY .
USING org.prorefactor.core.*                           FROM ASSEMBLY .
USING com.joanju.proparse.NodeTypes                    FROM ASSEMBLY .


CLASS Consultingwerk.Studio.LegacyGuiMigration.Frame.TriggerParser: 

    DEFINE VARIABLE lInitialized AS LOGICAL NO-UNDO INIT FALSE .

    {Consultingwerk/Studio/LegacyGuiMigration/Frame/dsTrigger.i}
    
    DEFINE VARIABLE lcCode AS LONGCHAR NO-UNDO . 

    /*------------------------------------------------------------------------------
        Purpose: Raised when the status of the class has changed
        Notes:   
        @param sender The object that raised the StatusChanged event
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT StatusChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                      e AS StatusChangedEventArgs).
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Text of the ID Node under the given Node  
        Notes:   
        @param poNode The parent node to locate the ID under
        @return The ID value
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER FindId (poNode AS JPNode):
        
        DEFINE VARIABLE oNode AS JPNode NO-UNDO . 
        
        ASSIGN oNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "ID":U) .
        
        IF VALID-OBJECT (oNode) THEN 
            RETURN oNode:getText () . 
            
        RETURN ?. 

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Initializes the Proparse environment 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID Initialize ():
        
        IF lInitialized THEN  
            RETURN . 

        ASSIGN lInitialized = TRUE . 
        
        THIS-OBJECT:PublishStatus ("Initializing Proparse environment.":U) . 

        ProparseHelper:SetProparseEnvironment() . 

        THIS-OBJECT:PublishStatus ("Processing database schema.":U) . 

        ProparseHelper:ExportDatabaseSchema () .
        
        THIS-OBJECT:PublishStatus ("Initializing Proparse.":U) . 
        
        ProparseHelper:Initialize() .     

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the given file for the trigger subscriptions and returns a dataset 
                 containing all triggers including their source code
        Notes:   
        @param pcFileName The name of the file to parse
        @param dsTrigger OUTPUT DATASET The ProDataset with the Trigger subscriptions and source code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetTriggerCode (pcFileName AS CHARACTER,
                                       OUTPUT DATASET dsTrigger):
                               
        DEFINE VARIABLE oNumericFormat    AS NumericFormat   NO-UNDO . 
        DEFINE VARIABLE oParseUnit        AS ParseUnit       NO-UNDO . 
        DEFINE VARIABLE oNodeWrapper      AS JPNodesWrapper  NO-UNDO . 
                           
        COPY-LOB FROM FILE pcFileName TO lcCode .                             
                                           
        DATASET dsTrigger:EMPTY-DATASET () .                                           

        oNumericFormat = SessionHelper:GetNumericFormat() .
        SESSION:NUMERIC-FORMAT = "AMERICAN":U . 
        
        THIS-OBJECT:Initialize () .
        
        THIS-OBJECT:PublishStatus ("Parsing source code.":U) . 
        
        oParseUnit = ProparseHelper:ParseFile (pcFileName) .
        
        oNodeWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) . 
        oNodeWrapper:NodeTypeFilter = "ON":U  .
   
        {Consultingwerk/foreachABL.i JPNode oNode in oNodeWrapper}

            THIS-OBJECT:ParseTriggerNode (oNode) .

        END .

        FINALLY:
            SessionHelper:SetNumericFormat (oNumericFormat) .
            
            GarbageCollectorHelper:DeleteObject (oNodeWrapper) .
            GarbageCollectorHelper:DeleteObject (oParseUnit) .      
            
            ASSIGN lcCode = "":U .                                            
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Raises the StatusChanged
        Notes:   
        @param e The StatusChangedEventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnStatusChanged (e AS StatusChangedEventArgs):
            
        Consultingwerk.Assertion.EventArgsAssert:IsValid (e, "StatusChanged":U) .
         
        THIS-OBJECT:StatusChanged:Publish (THIS-OBJECT, e) .
    
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a JPNode representing a trigger (ON Statement) 
        Notes:   Creates records in the temp-tables   
        @param poNode The JPNode representing the ON Statement
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseTriggerNode (poNode AS JPNode):
        
        DEFINE VARIABLE cEvent  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cWidget AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cFrame  AS CHARACTER NO-UNDO .
        
        DEFINE VARIABLE oDoNode AS JPNode NO-UNDO . 
        DEFINE VARIABLE oNode   AS JPNode NO-UNDO . 
        DEFINE VARIABLE oChild  AS JPNode NO-UNDO . 
        
        DEFINE VARIABLE lcTriggerCode AS LONGCHAR NO-UNDO .  
        
        /* Locate Trigger source */
        ASSIGN oDoNode = ProparseHelper:FindChildNodeOfNodeType (poNode, "DO":U) .
        
        IF NOT VALID-OBJECT (oDoNode) THEN 
            RETURN . 
            
        CREATE ttTriggerCode.
        ASSIGN ttTriggerCode.TriggerCodeGuid = GUID 
               ttTriggerCode.TriggerCode     = oDoNode:toStringFulltext() .  
        
        ASSIGN oNode = poNode:firstChild () .
        
        DO WHILE VALID-OBJECT (oNode):
            
            IF NodeTypes:getTypeName (oNode:getType()) = "Event_list":U THEN DO:
                oChild = oNode:firstChild () . 
                
                ASSIGN cEvent = oChild:getText () . 
                
            END.
            
            IF NodeTypes:getTypeName (oNode:getType()) = "Widget_ref":U THEN DO:
                
                oChild = ProparseHelper:FindChildNodeOfNodeType (oNode, "Field_ref":U) . 
                
                IF VALID-OBJECT (oChild) THEN 
                    ASSIGN cWidget = THIS-OBJECT:FindId (oChild) . 

                oChild = ProparseHelper:FindChildNodeOfNodeType (oNode, "IN":U) . 
                
                IF VALID-OBJECT (oChild) THEN 
                    ASSIGN cFrame = THIS-OBJECT:FindId (oChild) . 
            
                IF cEvent > "":U AND cWidget > "":U THEN DO:

                    CREATE ttTriggerEvent . 
                    ASSIGN ttTriggerEvent.TriggerEvent    = cEvent 
                           ttTriggerEvent.TriggerWidget   = cWidget
                           ttTriggerEvent.TriggerFrame    = cFrame
                           ttTriggerEvent.TriggerCodeGuid = ttTriggerCode.TriggerCodeGuid .
                END.
            
                ASSIGN cEvent  = "":U 
                       cWidget = "":U
                       cFrame  = "":U . 
            END.

            oNode = oNode:nextSibling () .             
        END. 

        FINALLY:
            RELEASE ttTriggerCode . 
            RELEASE ttTriggerEvent . 
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Publishes the a status message 
        Notes:   
        @param pcStatus The status to publish
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PublishStatus (pcStatus AS CHARACTER):
        
        THIS-OBJECT:OnStatusChanged (NEW StatusChangedEventArgs (pcStatus)) .

    END METHOD .

END CLASS.

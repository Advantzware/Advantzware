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
    File        : JPNodesEnumerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 10 15:23:23 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Base.*  FROM PROPATH .
USING Consultingwerk.Studio.Proparse.* FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING org.prorefactor.refactor.*       FROM ASSEMBLY.
USING org.prorefactor.treeparser.*     FROM ASSEMBLY.
USING org.prorefactor.core.*           FROM ASSEMBLY.
USING com.joanju.proparse.NodeTypes    FROM ASSEMBLY .
&ENDIF

CLASS Consultingwerk.Studio.Proparse.JPNodesEnumerator 
    IMPLEMENTS IEnumerator: 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE VARIABLE oCurrentNode AS JPNode NO-UNDO . 
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the current JPNode                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Current AS Progress.Lang.Object NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the JPNodesWrapper this Enumerator works on                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY JPNodesWrapper AS JPNodesWrapper NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the filter for the NodeType used by the JPNodesEnumerator                                                                       
        Notes:   For example, when set for the Code_block node of a Class, setting the 
                 filter to METHOD controls that methods are to be returned only.
                 Leave empty to retrieve all nodes
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NodeTypeFilter AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the JPNodesEnumerator  class                                                                       
        Notes:                                               
        @param poJPNodesWrapper The JPNodesWrapper to enumerate
        @param pcNodeTypeFilter The file for the NodeType                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JPNodesEnumerator (poJPNodesWrapper AS JPNodesWrapper,
                                          pcNodeTypeFilter AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:JPNodesWrapper = poJPNodesWrapper 
               THIS-OBJECT:NodeTypeFilter = pcNodeTypeFilter .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Moves the enumerator to the next entry                                                                      
        Notes:   Reset positioned on the first child already. So this method will 
                 always leave positioned to the next record for oCurrentNode instead 
                 of the Current. This CurrentNode will be returned by Current
        @return Logical value indicating if there was a next item                                                                          
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL MoveNext ():
        
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        IF NOT VALID-OBJECT (oCurrentNode) THEN 
            RETURN FALSE . 
        
        IF THIS-OBJECT:NodeTypeFilter > "":U THEN DO:
            DO WHILE NodeTypes:getTypeName (oCurrentNode:getType()) <> THIS-OBJECT:NodeTypeFilter:
                
                oCurrentNode = oCurrentNode:nextSibling () .
                
                IF NOT VALID-OBJECT (oCurrentNode) THEN RETURN FALSE . 
            END.
        END.    

        THIS-OBJECT:Current = oCurrentNode .
        oCurrentNode = oCurrentNode:nextSibling () .
        
        RETURN TRUE .  
&ELSE
        RETURN FALSE . 
&ENDIF
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose:                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Reset ():
&IF DEFINED (DotNetAccessible) NE 0 &THEN
        ASSIGN oCurrentNode = THIS-OBJECT:JPNodesWrapper:ParentNode:firstChild () .
&ENDIF
    END METHOD.

END CLASS.
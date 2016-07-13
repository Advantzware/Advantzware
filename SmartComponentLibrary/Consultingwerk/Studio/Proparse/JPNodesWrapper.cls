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
    File        : JPNodesWrapper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu May 10 15:13:33 CEST 2012
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

CLASS Consultingwerk.Studio.Proparse.JPNodesWrapper 
    IMPLEMENTS IEnumerable:
     
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the filter for the NodeType used by the JPNodesEnumerator                                                                       
        Notes:   For example, when set for the Code_block node of a Class, setting the 
                 filter to METHOD controls that methods are to be returned only.
                 Leave empty to retrieve all nodes
                 Effective for the next call to GetEnumerator only                                                                         
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NodeTypeFilter AS CHARACTER NO-UNDO 
    GET.
    SET. 

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the parent JPNode of this nodes collection                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ParentNode AS JPNode NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the JPNodesWrapper class                                                                       
        Notes:                                           
        @param poParentNode The reference to the parent JPNode                               
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC JPNodesWrapper (poParentNode AS JPNode):
        SUPER ().
        
        Consultingwerk.Assertion.ObjectAssert:IsValid (poParentNode) .
        
        THIS-OBJECT:ParentNode = poParentNode .
        
    END CONSTRUCTOR.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the IEnumerator for this JPNodesWrapper                                                                      
        Notes:
        @return The reference to the IEnumerator                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC Consultingwerk.Framework.Base.IEnumerator GetEnumerator ():
        
        RETURN NEW JPNodesEnumerator (THIS-OBJECT, 
                                      THIS-OBJECT:NodeTypeFilter) . 

    END METHOD.

END CLASS.
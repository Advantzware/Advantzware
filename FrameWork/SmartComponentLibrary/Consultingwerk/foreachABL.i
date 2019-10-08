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
    File        : foreachABL.i
    Purpose     : Simplify the usage of Consultingwerk Enumerators in the ABL, similar 
                  to the foreach statement in C# 

    Syntax      : {foreachABL.i <itemtype> <itemvariable> in <list>}
                  Consultingwerk/foreachABL.i System.Collections.DictionaryEntry oEntry in THIS-OBJECT:Model:Shapes
                  
                  The third parameter "in" should always be "in", to simulate the C# syntax.
            
                  The fifth parameter may be set as "nodefine" to avoid the creation 
                  of the variables
                  
    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri May 06 13:32:39 CEST 2011
    Notes       : See http://msdn.microsoft.com/en-us/library/ttw7t8t6(v=vs.71).aspx
                  for a reference of the C# foreach statement
  ----------------------------------------------------------------------*/

&IF "{5}" NE "nodefine" &THEN
    DEFINE VARIABLE {2}           AS {1} NO-UNDO . 
    DEFINE VARIABLE {2}Enumerator AS Consultingwerk.Framework.Base.IEnumerator NO-UNDO . 
&ENDIF    
    
    ASSIGN {2}Enumerator = CAST({4}, Consultingwerk.Framework.Base.IEnumerable):GetEnumerator() .
    
    {2}Enumerator:Reset() .
    
    DO WHILE {2}Enumerator:MoveNext() ON ERROR UNDO, THROW:
        ASSIGN {2} = CAST({2}Enumerator:Current, {1}) .  

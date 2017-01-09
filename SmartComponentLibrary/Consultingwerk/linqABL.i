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
    File        : linqABL.i
    Purpose     : Simplify the usage of Consultingwerk Enumerators in the ABL, similar 
                  to the foreach statement in C#. Extension to foreachABL.i with
                  additional support for up to 5 comparison operators to filter
                  list entries when iterating the list 

    Syntax      : {linqABL.i <itemtype> <itemvariable> in <list> where <property1> <operator1> <value1> and <property2> <operator2> <value2>}
                  
                  The third parameter "in" should always be "in", to simulate the C# syntax.
            
                  linqABL.i does NOT support the nodefine parameter as foreachABL.i 
                  Supports up to 5 comparisons
                  
    Description : linqABL.i 
                  {1}  <itemtype> 
                  {2}  <itemvariable> 
                  {3}  in 
                  {4}  <list> 
                  {5}  where 
                  {6}  <property1> 
                  {7}  <operator1> 
                  {8}  <value1> 
                  {9}  and 
                  {10} <property2> 
                  {11} <operator2> 
                  {12} <value2>
                  {13} and 
                  {14} <property3> 
                  {15} <operator3> 
                  {16} <value3>
                  {17} and 
                  {18} <property4> 
                  {19} <operator4> 
                  {20} <value4>
                  {21} and 
                  {22} <property5> 
                  {23} <operator5> 
                  {24} <value5>

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 02 26:16:07 CEST 2015
    Notes       : See http://msdn.microsoft.com/en-us/library/ttw7t8t6(v=vs.71).aspx
                  for a reference of the C# foreach statement
  ----------------------------------------------------------------------*/

    DEFINE VARIABLE {2}           AS {1} NO-UNDO . 
    DEFINE VARIABLE {2}Enumerator AS Consultingwerk.Framework.Base.IEnumerator NO-UNDO . 
    
    ASSIGN {2}Enumerator = CAST({4}, Consultingwerk.Framework.Base.IEnumerable):GetEnumerator() .
    
    {2}Enumerator:Reset() .
    
    DO WHILE {2}Enumerator:MoveNext() ON ERROR UNDO, THROW:
        ASSIGN {2} = CAST({2}Enumerator:Current, {1}) .  

        &IF "{5}" NE "" &THEN
        IF NOT ({2}:{6} {7} {8} 
        &ENDIF
        
        &IF "{9}" NE "" &THEN
            {9} {2}:{10} {11} {12} 
        &ENDIF

        &IF "{13}" NE "" &THEN
            {13} {2}:{14} {15} {16} 
        &ENDIF

        &IF "{17}" NE "" &THEN
            {17} {2}:{18} {19} {20} 
        &ENDIF

        &IF "{21}" NE "" &THEN
            {21} {2}:{22} {23} {24} 
        &ENDIF

        &IF "{5}" NE "" &THEN
            ) THEN NEXT .
        &ENDIF
                
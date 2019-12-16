
/*------------------------------------------------------------------------
    File        : GetNextTagTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Wed Nov 27 12:12:09 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cNewLoadtag AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE cItemfg AS CHARACTER NO-UNDO INITIAL "10x10x10".
DEFINE BUFFER bf-loadtag FOR loadtag.

DISABLE TRIGGERS FOR LOAD OF bf-loadtag.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND LAST loadtag
   where loadtag.i-no eq cItemfg
     and item-type = NO.
     
MESSAGE "Next Loadtag, traditional:  " loadtag.tag-no
VIEW-AS ALERT-BOX. 


RUN loadtags\GetNextTag.p('001',cItemfg, OUTPUT cNewLoadtag).

MESSAGE "Result of GetNextTag, traditional : " cNewLoadtag
VIEW-AS ALERT-BOX.


    create bf-loadtag.
    buffer-copy loadtag except tag-no to bf-loadtag.
    bf-loadtag.tag-no = STRING(CAPS(loadtag.i-no),"x(15)") + STRING(99999,"99999").

RUN loadtags\GetNextTag.p('001',cItemfg, OUTPUT cNewLoadtag).

MESSAGE "Result of GetNextTag first max: " cNewLoadtag
VIEW-AS ALERT-BOX.


    create bf-loadtag.
    buffer-copy loadtag except tag-no to bf-loadtag.
    bf-loadtag.tag-no = STRING(CAPS(loadtag.i-no),"x(14)") + STRING(99999 + 1,"999999").

RUN loadtags\GetNextTag.p('001',cItemfg, OUTPUT cNewLoadtag).

MESSAGE "Result of GetNextTag first after max: " cNewLoadtag
VIEW-AS ALERT-BOX.

    create bf-loadtag.
    buffer-copy loadtag except tag-no to bf-loadtag.
    bf-loadtag.tag-no = STRING(CAPS(loadtag.i-no),"x(14)") + STRING(123456,"999999").

RUN loadtags\GetNextTag.p('001',cItemfg, OUTPUT cNewLoadtag).

MESSAGE "Result of GetNextTag well after max: " cNewLoadtag
VIEW-AS ALERT-BOX.

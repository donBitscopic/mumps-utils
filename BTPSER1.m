BTPSER1 ; BTP - JSON to M API; 07/17/2015
 ;;0.1;BTP TOOLS;**1**;07/17/15;Build 1
 ; JSNTOM is the entry point for Deserialization
 ; Ex Call: D JSNTOM(.RETURN,"{"myJsonStringKey":"myJsonStringVal"}","^MYGlOBAL()") W ^MYGLOBAL("myJsonKey") --> myJsonStringVal
 ; Saves OBJ under the NAME global root
 ;  |-------------------  Variables KEY  -------------------|
 ;  |                                                       |
 ;  |   OBJ --> The json string passed in                   |
 ;  |   NAME --> The global root passed in                  |
 ;  |   RETURN --> The return object for the RPC broker     |
 ;  |   LEN --> The length of the json string               |
 ;  |   SPOT --> The index position in the json string      |
 ;  |   SCOPE --> The global root and current subscript     |
 ;  |   CHAR --> The charachter at SPOT in the json string  |
 ;  |   RET --> The return value of the function            |
 ;  |                                                       |
 ;  |-------------------------------------------------------|
 ;
 ;
JSNTOM(RETURN,OBJ,NAME)
 N LEN,CHAR,SPOT,SCOPE,PASS,ARR,COUNT,LSCOPE 
 S LEN=$LENGTH(NAME)
 I (")"=$E(NAME,LEN)),("("=$E(NAME,LEN-1)) S NAME=$E(NAME,0,LEN-2) ;handles subscripts being passed with the name ie. EXAMPLE("EXSUB1","EXSUB2")
 S RETURN=NAME,ARR=0,COUNT=0,LSCOPE=0
 S SCOPE=NAME ;scope will be used as the current global and subscript path as we parse
 S SPOT=1 ;Our current index of the string
 S LEN=$LENGTH(OBJ)
 F i=1:1:LEN D ;no real need for loop hear bracket or getarr should only be called once, the loop helps with white space
 . S CHAR=$EXTRACT(OBJ,SPOT)
 . S SPOT=SPOT+1
 . I CHAR="{" S i=LEN D BRACKET(OBJ,.SPOT,SCOPE,LEN) 
 . I CHAR="[" S i=LEN D GETARR(OBJ,.SPOT,LEN,SCOPE,LSCOPE)
 . I CHAR=" "
 Q
 ;
 ; BRACKET is a recursicve function that handles dictionaries on the JSON String
 ; Ex Call: D BRACKET("{"myJsonString":"myJsonString"}",1,LEN,"^MYGLOBAL","^MYGLOBAL(MYSUB)")
 ; Returns once it has finsihed reading the entire dictionary that it was called on 
BRACKET(OBJ,SPOT,SCOPE,LEN) 
 N CHAR,COMMA,LSCOPE,DICT,FLAG ;
 S COMMA=1,LSCOPE="",CHAR="",FLAG=0 
 F i=1:1:LEN D  Q:FLAG
 . I (COMMA=1) D
 . . S DICT=$$KEY(OBJ,.SPOT,LEN),COMMA=0 ; Set DICT equal to the name of the next key in the JSON string
 . S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR=":" D COLON(OBJ,.SPOT,LEN,.SCOPE,DICT) S CHAR=$EXTRACT(OBJ,SPOT) ;get the value after the colon unless it is a dictionary or array
 . I CHAR="{" S SPOT=SPOT+1 S LSCOPE=$NAME(@SCOPE@(DICT)) D BRACKET(OBJ,.SPOT,LSCOPE,LEN) S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR="[" S SPOT=SPOT+1 S LSCOPE=$NAME(@SCOPE@(DICT)) D GETARR(OBJ,.SPOT,LEN,LSCOPE,LSCOPE) S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR="}" S FLAG=1
 . I CHAR="," S COMMA=1
 . S SPOT=SPOT+1
 Q
 ;
 ; GETARR is a recursive function that handles arrays on the JSON String
 ; Ex Call: D GETARR("{"myJsonString":"myJsonString"}",1,LEN,"^MYGLOBAL","^MYGLOBAL(MYSUB)")
 ; Returns once it has finsihed reading the entire dictionary that it was called on 
GETARR(OBJ,SPOT,LEN,SCOPE,LSCOPE)
 N DIST,COUNT,FLAG,CDIST,BDIST,OBJTWO,PART,SQDIST,SDIST
 S COUNT=0,FLAG=0,COMMA=1
 F i=SPOT:1:LEN D  Q:FLAG
 . S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR=" " S SPOT=SPOT+1 S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR="[" S SPOT=SPOT+1 S LSCOPE=$NAME(@SCOPE@(COUNT)) D GETARR(OBJ,.SPOT,LEN,LSCOPE,LSCOPE) S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR="{" S SPOT=SPOT+1 S LSCOPE=$NAME(@SCOPE@(COUNT)) D BRACKET(OBJ,.SPOT,LSCOPE,LEN) S CHAR=$EXTRACT(OBJ,SPOT)
 . I CHAR="]" S FLAG=1
 . I CHAR="," S COMMA=1,COUNT=COUNT+1,SPOT=SPOT+1
 . I (COMMA=1) D ;if comma is true we set the global at SCOPE(COUNT) equal to the next value in the JSON string
 . . S OBJTWO=$E(OBJ,SPOT,LEN)
 . . S CDIST=$F(OBJTWO,","),BDIST=$F(OBJTWO,"{"),SQDIST=$F(OBJTWO,"]"),SDIST=$F(OBJTWO,"[") ;this is for handling all types of arrays
 . . I (CDIST<BDIST)!(BDIST=0),(CDIST'=0),((CDIST<SQDIST)!(SQDIST=0)),(CDIST<SDIST)!(SDIST=0) D ;if a comma is the
 . . . S PART=$P(OBJTWO,",",1)
 . . . I (PART="") S PART=$P(OBJTWO,",",2)
 . . . I (1=$F(PART,"""")) D
 . . . . S PART=$P(PART,"""",2)
 . . . S SPOT=SPOT+$L(PART)
 . . . D TRIMQ(.PART)
 . . . S @SCOPE@(COUNT)=PART 
 . . I (SQDIST<CDIST)!(CDIST=0),(SQDIST'=0),(SQDIST<SDIST)!(SDIST=0) D
 . . . S PART=$P(OBJTWO,"]",1)
 . . . S SPOT=SPOT+$L(PART)
 . . . D TRIMQ(.PART)
 . . . S @SCOPE@(COUNT)=PART 
 . . S COMMA=0
 . E  S SPOT=SPOT+1
 Q
 ;
 ; COLON is a function that is called when BRACKET encounters a ":".
 ;  It handles getting the corresponding value after the ":"
 ;  and also setting the global root at the current subscript equal to that value
 ; Ex Call: D COLON("{"myJsonKey":"myJsonVal"}",16,LEN,"^MYGLOBAL","myJsonKey") W ^MYGLOBAL("myJsonKey") --> myJsonVal
 ; Returns once it gets the value and adjust spot accorddingly 
COLON(OBJ,SPOT,LEN,SCOPE,DICT) 
 N CHEK,RET
 S RET=$$POSTCOL(OBJ,.SPOT,LEN)
 I (RET=1) D ;QUOTE CAME UP NEXT
 . S OBJTWO=$EXTRACT(OBJ,SPOT+1,LEN)
 . D RMVWS(.OBJTWO)
 . S CHEK=$$CHECK(OBJTWO)
 . I (CHEK=1) D
 . . S PART=$PIECE(OBJTWO,"""",1)
 . . I PART="" S PART=$PIECE(OBJTWO,"""",2)
 . . D RMVWS(.PART)
 . . S @SCOPE@(DICT)=PART
 . . S SPOT=SPOT+$LENGTH(PART)+1
 . I (CHEK=2) D
 . . S PART=$PIECE(OBJTWO,"}",1)
 . . I PART="" S SPOT=SPOT+1 S OBJTWO=$EXTRACT(OBJ,SPOT+1,LEN) S PART=$PIECE(OBJTWO,"}",1)
 . . D RMVWS(.PART)
 . . S @SCOPE@(DICT)=PART
 . . S SPOT=SPOT+$LENGTH(PART)+1
 . I (CHEK=3) D
 . . S PART=$PIECE(OBJTWO,",",1)
 . . I PART="" S SPOT=SPOT+1 S OBJTWO=$EXTRACT(OBJ,SPOT+1,LEN) S PART=$PIECE(OBJTWO,",",1)
 . . D RMVWS(.PART)
 . . S @SCOPE@(DICT)=PART
 . . S SPOT=SPOT+$LENGTH(PART)+1
 Q
 ;
 ; KEY is a function that is called in BRACKET when BRACKET needs to retrieve the next key from the json string
 ; Ex Call: S MYKEY=KEY("{"myJsonKey":"myJsonVal"}",16,LEN) W MYKEY --> "myJsonKey"
 ; Returns the DICT value and updates the SPOT
KEY(OBJ,SPOT,LEN)
 S OBJTWO=$EXTRACT(OBJ,SPOT,LEN)
 S DICT=$PIECE(OBJTWO,":",1)
 S SPOT=SPOT+$LENGTH(DICT)
 D TRIMQ(.DICT)
 Q DICT
 ;
 ; POSTCOL is a function that is called in COLON when COLON is looking for what type of value the key has. 
 ; Ex Call: S RETVAL=CHECK("{""myJsonKey"":""myJsonVal""}",16,LEN) W RETVAL --> 1
 ; Returns a 1 if the value is not a Dictionary or an Array, otherwise 0
POSTCOL(OBJ,SPOT,LEN)
 N RET
 S RET=0
 S CHAR=$EXTRACT(OBJ,SPOT)
 F x=1:1:LEN
 . I CHAR'=" " S i=LEN Q
 . S SPOT=SPOT+1
 . S CHAR=$EXTRACT(OBJ,SPOT)
 S QUOTE=$F(OBJ,"""",SPOT)
 S BRACK=$F(OBJ,"{",SPOT)
 S ARRB=$F(OBJ,"[",SPOT)
 S COMMA=$F(OBJ,",",SPOT)
 S RBRACK=$F(OBJ,"}",SPOT)
 I QUOTE'=0 D
 . I ((QUOTE<BRACK)!(BRACK=0)),((QUOTE<ARRB)!(ARRB=0)) D
 . . S RET=1
 I (COMMA'=0) D
 . I ((COMMA<QUOTE)!(QUOTE=0)),((COMMA<BRACK)!(BRACK=0)),((COMMA<ARRB)!(ARRB=0)) D
 . . S RET=1
 I (RBRACK'=0) D
 . I ((RBRACK<QUOTE)!(QUOTE=0)),((RBRACK<BRACK)!(BRACK=0)),((RBRACK<ARRB)!(ARRB=0)) D
 . . S RET=1
 Q RET
 ;
 ; CHECK is a function that is called when COLON is looking for where the value its retrieving ends 
 ; Ex Call: S RETVAL=CHECK(":""myJsonVal""}") W RETVAL --> 1
 ; Returns a 1 if value surronded by quotes, 2 if brackets and 3 if a comma
CHECK(OBJ)
 N RET
 S RET=0
 S BRACK=$F(OBJ,"}")
 S QUOTE=$F(OBJ,"""")
 S COMMA=$F(OBJ,",")
 I QUOTE'=0 D
 . I ((QUOTE<BRACK)!(BRACK=0)),((QUOTE<COMMA)!(COMMA=0)) D
 . . S RET=1
 I BRACK'=0 D
 . I ((BRACK<QUOTE)!(QUOTE=0)),((BRACK<COMMA)!(COMMA=0)) D
 . . S RET=2
 I COMMA'=0 D
 . I ((COMMA<BRACK)!(BRACK=0)),((COMMA<QUOTE)!(QUOTE=0)) D
 . . S RET=3
 Q RET
 ;
 ; TRIMQ is called in GETARR and KEY, it trims the quotes off the front and back of DICT
 ; Ex Call: S DICT="""dict""" D TRIMQ(DICT) W DICT -->  "dict"
 ; Modfiies DICT string without any leading or trailing quotes or whitespace 
TRIMQ(DICT)
 N CHAR,FRONT,BACK,LEN
 S LEN=$LENGTH(DICT)
 S FRONT=1
 S BACK=LEN
 F z=1:1:LEN D
 . S CHAR=$E(DICT,z)
 . I (CHAR="""") ! (CHAR=" ") S FRONT=z+1
 . E  S z=LEN
 F z=LEN:-1:1 D
 . S CHAR=$E(DICT,z)
 . I (CHAR="""") ! (CHAR=" ") S BACK=z-1
 . E  S z=1
 S DICT=$EXTRACT(DICT,FRONT,BACK)
 Q
 ;
 ; RMVWS is called in COLON and remoces leading and trailing whitespace
 ; Ex Call: D RMVWS("  myquotesurronedval ") --> "myquotesurronedval"
 ; Modifies DICT without any leading or trailing quotes or whitespace
RMVWS(DICT)
 N CHAR,FRONT,BACK,LEN
 S LEN=$LENGTH(DICT)
 S FRONT=1
 S BACK=LEN
 F z=1:1:LEN D
 . S CHAR=$E(DICT,z)
 . I CHAR=" " S FRONT=z+1
 . I CHAR'=" " S z=LEN Q
 F z=LEN:-1:1 D
 . S CHAR=$E(DICT,z)
 . I CHAR=" " S BACK=z-1
 . I CHAR'=" " S z=1 Q
 S DICT=$EXTRACT(DICT,FRONT,BACK)
 Q
 ;
 ; MTOJSN is the entry point for Serialization
 ; Ex Call: D MTOJSN(.RETURN,"^MYGlOBAL()") 
 ; Parses the object ^MYGLOBAL into a JSON string and stores it in RETURN
MTOJSN(RETURN,OBJ)
 N LAST,PASS,SUBTWO,LEN,JSN,DATVAL,RETCNT,TESTT,NUMSUB
 S LEN=$L(OBJ)
 S RETURN(0)="" ;set up return obj incase Mump obj is empty
 I ")"'=$E(OBJ,LEN) S OBJ=OBJ_"()" ;Handling ^EXGLOBAL as opposed to ^EXGLOBAL() syntax
 S JSN="{",RETCNT=0,LEN=$L(OBJ),LEN=LEN-1,SUBTWO=$E(OBJ,1,LEN) ;get rid of last ) ie ^EXGLOBAL() --> ^EXGLOBAL(
 I "("'=$E(OBJ,LEN) S SUBTWO=SUBTWO_"," ;add comma if subscript present ie ^EXGLOBAL("sub" --> ^EXGLOBAL("sub",
 S PASS=SUBTWO_""""")" ;Pass is what we will pass into our $ORDER to get the next subscript, we add an empty quote ie ^EXGLOBAL("sub", --> ^EXGLOBAL("sub",""
 S LAST=$O(@PASS,1) ;get next subscript
 I LAST="" S RETURN(RETCNT)="{"""_$RE($P($RE(SUBTWO),"""",2))_""":"""_$GET(@OBJ)_"""}" Q  ;quit and return value of OBJ if no subscript on next level
 S PASS=SUBTWO_""""_LAST_""")" ;if subscript not empty add on SUB
 D DOWNLVL(.RETURN,PASS,LAST,.JSN,OBJ,.RETCNT) ;start recursive work horse
 S RETURN(RETCNT)=JSN ;RPC is extecing an array, this puts the last JSON from JSN into the return object
 Q
 ;
 ; DOWNLVL is driver function in MTOJSN 
 ; 
DOWNLVL(RETURN,SUB,LAST,JSN,OLDSUB,RETCNT)
 N PASS,SUBTWO,LEN,NEXTLVL,NEXT,STOP,FIRST,LEFT,RIGHT,VAL,QHELP,CHAHELP
 S STOP=0,FIRST=0,DATVAL=0
 S LEN=$L(JSN)
 I LEN>7000  S RETURN(RETCNT)=JSN S JSN="" S RETCNT=RETCNT+1 ;Handles max string length of mump value
 F i=0:0:1 D  I (STOP=1) Q  ;keeps going until stop is flipped
 . S LEN=($L(SUB)-1)  ;to eliminate the ) at the end of the script
 . S SUBTWO=$E(SUB,1,LEN) ;get the global and subs without the last )
 . S SUBTWO=SUBTWO_","""")" ; add on the empty string to get the next subscript
 . S DATVAL=$D(@SUB) ;$D returns 1 if there is data at the subscript
 . I 1=DATVAL  D
 . . I (FIRST=1) D ;if first time coming through at this recursive call add comma to JSN string
 . . . S JSN=JSN_","
 . . I ($F(LAST,"\")) D
 . . . S QHELP=""
 . . . S LEN=$L(LAST)
 . . . FOR I=1:1:LEN D
 . . . . S CHAHELP=$E(LAST,I)
 . . . . I (CHAHELP="\") D
 . . . . . S QHELP=QHELP_"\"_CHAHELP
 . . . . E  D
 . . . . . S QHELP=QHELP_CHAHELP
 . . . S LAST=QHELP
 . . . S LEN=$L(JSN)
 . . I ($F(LAST,"""")) D ;handles qoutes by inserting \in front of them for the C# layer
 . . . S LEN=$L(LAST)
 . . . S QHELP=$E(LAST,0,$F(LAST,"""")-1)
 . . . S QHELP=QHELP_"\"
 . . . S QHELP=QHELP_$E(LAST,$F(LAST,"""")-1,LEN)
 . . . S LAST=QHELP
 . . . S LEN=$L(JSN)
 . . S JSN=JSN_""""_LAST_""":"
 . . S VAL=$GET(@SUB)
 . . I ($F(VAL,"\")) D
 . . . S QHELP=""
 . . . S LEN=$L(VAL)
 . . . FOR I=1:1:LEN D
 . . . . S CHAHELP=$E(VAL,I)
 . . . . I (CHAHELP="\") D
 . . . . . S QHELP=QHELP_"\"_CHAHELP
 . . . . E  D
 . . . . . S QHELP=QHELP_CHAHELP
 . . . S VAL=QHELP
 . . . S LEN=$L(JSN)
 . . I ($F(VAL,"""")) D
 . . . S QHELP=""
 . . . S LEN=$L(VAL)
 . . . FOR I=1:1:LEN D
 . . . . S CHAHELP=$E(VAL,I)
 . . . . I (CHAHELP="""")  ; S CHAHELP="\"_CHAHELP
 . . . . S QHELP=QHELP_CHAHELP
 . . . S VAL=QHELP
 . . . S LEN=$L(JSN)
 . . I (VAL=+VAL) S JSN=JSN_VAL
 . . E  S JSN=JSN_""""_VAL_""""
 . . S FIRST=1
 . S NEXTLVL=$O(@SUBTWO,1) ;get subscript below our current level
 . I (NEXTLVL'="") D ;if subscript is not empty add it on to our current one and make the recursive call
 . . S PASS=$E(SUB,1,LEN)
 . . I +NEXTLVL=NEXTLVL S PASS=PASS_","_NEXTLVL_")"
 . . E  S PASS=PASS_","""_NEXTLVL_""")"
 . . I (FIRST=1) D
 . . . S JSN=JSN_","
 . . I ($F(LAST,"\")) D
 . . . S QHELP=""
 . . . S LEN=$L(LAST)
 . . . FOR I=1:1:LEN D
 . . . . S CHAHELP=$E(LAST,I)
 . . . . I (CHAHELP="\") D
 . . . . . S QHELP=QHELP_"\"_CHAHELP
 . . . . E  D
 . . . . . S QHELP=QHELP_CHAHELP
 . . . S LAST=QHELP
 . . . S LEN=$L(JSN)
 . . I ($F(LAST,"""")) D
 . . . S QHELP=""
 . . . S LEN=$L(LAST)
 . . . FOR I=1:1:LEN D
 . . . . S CHAHELP=$E(LAST,I)
 . . . . I (CHAHELP="""") S CHAHELP="\"_CHAHELP
 . . . . S QHELP=QHELP_CHAHELP
 . . . S LAST=QHELP
 . . . S LEN=$L(JSN)
 . . S JSN=JSN_""""_LAST_""":"
 . . S JSN=JSN_"{"
 . . S FIRST=1
 . . D DOWNLVL(.RETURN,PASS,NEXTLVL,.JSN,SUB,.RETCNT) ; recursive call on our newly added subscript
 . S NEXT=$O(@SUB,1) ;get next subscript at current level for next loop
 . I NEXT'="" D
 . . S LEN=($L(OLDSUB)-1)  ;to elimitae the ) at the end of the script
 . . S SUBTWO=$E(OLDSUB,1,LEN)
 . . S LEFT=$F(OLDSUB,"(")
 . . S RIGHT=$F(OLDSUB,")")
 . . I (RIGHT-LEFT=1) D
 . . . I +NEXT=NEXT S SUBTWO=SUBTWO_NEXT_")"
 . . . E  S SUBTWO=SUBTWO_""""_NEXT_""")"
 . . I (RIGHT-LEFT'=1) D  
 . . . I +NEXT=NEXT S SUBTWO=SUBTWO_","_NEXT_")"
 . . . E  S SUBTWO=SUBTWO_","""_NEXT_""")"
 . . S SUB=SUBTWO
 . . S LAST=NEXT
 . I NEXT="" S STOP=1 Q ;$Order returns an empty string we have traversed all subscripts at this level so end loop and quit
 S JSN=JSN_"}" ; if leaving close dictionary for that level
 Q
 ;
SET(RETURN,GLOBAL,VALUE)
 S @GLOBAL=VALUE
 Q VALUE
 ;
KILL(RETURN,GLOBAL)
 K @GLOBAL
 S RETURN="Success"
 Q 
 ;
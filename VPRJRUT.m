VPRJRUT ;SLC/KCM -- Utilities for HTTP communications;2017-12-28  2:05 PM
 ;
LOW(X) Q $TR(X,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz")
 ;
UP(X) Q $TR(X,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 ;
LTRIM(%X) ; Trim whitespace from left side of string
 ; derived from XLFSTR, but also removes tabs
 N %L,%R
 S %L=1,%R=$L(%X)
 F %L=1:1:$L(%X) Q:$A($E(%X,%L))>32
 Q $E(%X,%L,%R)
 ;
URLENC(X) ; Encode a string for use in a URL
 ; Q $ZCONVERT(X,"O","URL")  ; uncomment for fastest performance on Cache
 ; =, &, %, +, non-printable
 ; {, } added JC 7-24-2012
 N I,Y,Z,LAST
     S Y=$P(X,"%") F I=2:1:$L(X,"%") S Y=Y_"%25"_$P(X,"%",I)
 S X=Y,Y=$P(X,"&") F I=2:1:$L(X,"&") S Y=Y_"%26"_$P(X,"&",I)
 S X=Y,Y=$P(X,"=") F I=2:1:$L(X,"=") S Y=Y_"%3D"_$P(X,"=",I)
 S X=Y,Y=$P(X,"+") F I=2:1:$L(X,"+") S Y=Y_"%2B"_$P(X,"+",I)
 S X=Y,Y=$P(X,"{") F I=2:1:$L(X,"{") S Y=Y_"%7B"_$P(X,"{",I)
 S X=Y,Y=$P(X,"}") F I=2:1:$L(X,"}") S Y=Y_"%7D"_$P(X,"}",I)
 S Y=$TR(Y," ","+")
 S Z="",LAST=1
 F I=1:1:$L(Y) I $A(Y,I)<32 D
 . S CODE=$$DEC2HEX($A(Y,I)),CODE=$TR($J(CODE,2)," ","0")
 . S Z=Z_$E(Y,LAST,I-1)_"%"_CODE,LAST=I+1
 S Z=Z_$E(Y,LAST,$L(Y))
 Q Z
 ;
URLDEC(X,PATH) ; Decode a URL-encoded string
 ; Q $ZCONVERT(X,"I","URL")  ; uncomment for fastest performance on Cache
 ;
 N I,OUT,FRAG,ASC
 S:'$G(PATH) X=$TR(X,"+"," ") ; don't convert '+' in path fragment
 F I=1:1:$L(X,"%") D
 . I I=1 S OUT=$P(X,"%") Q
 . S FRAG=$P(X,"%",I),ASC=$E(FRAG,1,2),FRAG=$E(FRAG,3,$L(FRAG))
 . I $L(ASC) S OUT=OUT_$C($$HEX2DEC(ASC))
 . S OUT=OUT_FRAG
 Q OUT
 ;
REFSIZE(ROOT) ; return the size of glvn passed in ROOT
 Q:'$D(ROOT) 0 Q:'$L(ROOT) 0
 N SIZE,I
 S SIZE=0
 I $D(@ROOT)#2 S SIZE=$L(@ROOT)
 I $D(@ROOT)>1 S I=0 F  S I=$O(@ROOT@(I)) Q:'I  S SIZE=SIZE+$L(@ROOT@(I))
 Q SIZE
 ;
VARSIZE(V) ; return the size of a variable
 Q:'$D(V) 0
 N SIZE,I
 S SIZE=0
 I $D(V)#2 S SIZE=$L(V)
 I $D(V)>1 S I="" F  S I=$O(V(I)) Q:'I  S SIZE=SIZE+$L(V(I))
 Q SIZE
 ;
PAGE(ROOT,START,LIMIT,SIZE,PREAMBLE,RETCNTS) ; create the size and preamble for a page of data
 QUIT:'$D(ROOT)  QUIT:'$L(ROOT)
 N I,J,KEY,KINST,COUNT,TEMPLATE,PID,HASDATA
 K:$D(@ROOT@($J)) @ROOT@($J)
 S SIZE=0,COUNT=0,TEMPLATE=$G(@ROOT@("template"),0) ;,PID=$G(@ROOT@("pid"))
 I $L(TEMPLATE) D LOADSPEC^VPRJCT1(.TEMPLATE)
 F I=START:1:(START+LIMIT-1) Q:'$D(@ROOT@("data",I))  S COUNT=COUNT+1 D
 . S KEY="" F  S KEY=$O(@ROOT@("data",I,KEY)) Q:KEY=""  D
 . . S KINST="" F  S KINST=$O(@ROOT@("data",I,KEY,KINST)) Q:KINST=""  D
 . . . S PID=^(KINST)  ; null if non-pt data
 . . . D TMPLT(ROOT,.TEMPLATE,I,KEY,KINST,PID)
 . . . S J="" F  S J=$O(@ROOT@($J,I,J)) Q:'J  S SIZE=SIZE+$L(@ROOT@($J,I,J))
 S HASDATA=$D(@ROOT@($J))
 S RETCNTS=$G(RETCNTS,0)
 S PREAMBLE=$S('$D(^VPRCONFIG("store",$G(HTTPREQ("store")),"global")):$$BLDHEAD(@ROOT@("total"),COUNT,START,LIMIT),1:$$GDSHEAD(HASDATA,RETCNTS,@ROOT@("total"),COUNT))
 ; for vpr or data stores add 3 for "]}}", add COUNT-1 for commas
 ; for other data stores add 1 for "}" if no data, 2 for "]}" if data present, add COUNT-1 for commas
 S SIZE=$S('$D(^VPRCONFIG("store",$G(HTTPREQ("store")),"global")):SIZE+$L(PREAMBLE)+3+COUNT-$S('COUNT:0,1:1),1:SIZE+$L(PREAMBLE)+$S(HASDATA:2,1:1)+COUNT-$S('COUNT:0,1:1))
 QUIT
 ;
TMPLT(ROOT,TEMPLATE,ITEM,KEY,KINST,PID) ; set template
 I HTTPREQ("store")="vpr"  G TLT4VPR
 I HTTPREQ("store")="data" G TLT4DATA
 I HTTPREQ("store")="xvpr" G TLT4XVPR
 I HTTPREQ("store")'="" G TLT4GDS
 ; otherwise trigger error and quit
 Q
TLT4XVPR ;
 ; set PID for this object unless just getting UID
 I TEMPLATE'="uid" N PID S PID=$O(^VPRPTJ("KEY",KEY,0))
 ; then drop thru to regular VPR template
TLT4VPR ;
 ; called from PAGE
 N STAMP,JPID
 S JPID=$$JPID4PID^VPRJPR(PID)
 ; JPID is required in order to get the latest stamp, otherwise we have to bail
 I JPID="" Q
 I TEMPLATE="uid" S @ROOT@($J,ITEM,1)="{""uid"":"""_KEY_"""}" Q
 I $E(TEMPLATE,1,4)="rel;" D RELTLTP^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE,PID) Q
 I $E(TEMPLATE,1,4)="rev;" D REVTLTP^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE,PID) Q
 ; query time template
 I $D(TEMPLATE)>1 D APPLYTLT Q
 ; saved template
 I $L(TEMPLATE),$D(^VPRPTJ("TEMPLATE",JPID,PID,KEY,TEMPLATE)) M @ROOT@($J,ITEM)=^(TEMPLATE) Q
 ; else full object
 ; Add the item to the return
 ; Get the bottom of the tree (latest record)
 S STAMP=$O(^VPRPTJ("JSON",JPID,PID,KEY,""),-1)
 M @ROOT@($J,ITEM)=^VPRPTJ("JSON",JPID,PID,KEY,STAMP)
 Q
TLT4DATA ;
 ; called from PAGE
 N STAMP
 I $G(TEMPLATE)="uid" S @ROOT@($J,ITEM,1)="{""uid"":"""_KEY_"""}" Q
 I $E(TEMPLATE,1,4)="rel;" D RELTLTD^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE) Q
 I $E(TEMPLATE,1,4)="rev;" D REVTLTD^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE) Q
 ; query time template
 I $D(TEMPLATE)>1 D APPLYTLT Q
 ; other template
 I $L(TEMPLATE),$D(^VPRJDJ("TEMPLATE",KEY,TEMPLATE)) M @ROOT@($J,ITEM)=^(TEMPLATE) Q
 ; else full object
 ; get based on stamp
 S STAMP=$O(^VPRJDJ("JSON",KEY,""),-1)
 M @ROOT@($J,ITEM)=^VPRJDJ("JSON",KEY,STAMP)
 Q
TLT4GDS ; Apply templates for GDS data stores to returned data
 ; called from PAGE
 ;
 N GLOBAL,GLOBALJ
 ; Parsed JSON
 S GLOBAL="^"_$G(^VPRCONFIG("store",$G(HTTPREQ("store")),"global"))
 ; Raw JSON
 S GLOBALJ="^"_$G(^VPRCONFIG("store",$G(HTTPREQ("store")),"global"))_"J"
 ;
 I $G(TEMPLATE)="uid" S @ROOT@($J,ITEM,1)="{""uid"":"""_KEY_"""}" QUIT
 I $E(TEMPLATE,1,4)="rel;" D RELTLTD^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE) QUIT
 I $E(TEMPLATE,1,4)="rev;" D REVTLTD^VPRJCT1($NA(@ROOT@($J,ITEM)),KEY,.TEMPLATE) QUIT
 ;
 ; query time template
 ; not supported
 ;I $D(TEMPLATE)>1 D APPLYTLT Q
 ;
 ; If there is a template apply it
 I $L(TEMPLATE),$D(@GLOBALJ@("TEMPLATE",KEY,TEMPLATE)) D  QUIT
 . ; GDS data stores require a lock on each data item before it is added to the result
 . L +@GLOBAL@(KEY):$G(^VPRCONFIG("timeout","gds"),5) E  D SETERROR^VPRJRER(502) Q
 . M @ROOT@($J,ITEM)=@GLOBALJ@("TEMPLATE",KEY,TEMPLATE)
 . L -@GLOBAL@(KEY)
 ;
 ; Else return the full object
 ; GDS data stores require a lock on each data item before it is added to the result
 L +@GLOBAL@(KEY):$G(^VPRCONFIG("timeout","gds"),5) E  D SETERROR^VPRJRER(502) QUIT
 M @ROOT@($J,ITEM)=@GLOBALJ@("JSON",KEY)
 L -@GLOBAL@(KEY)
 QUIT
 ;
APPLYTLT ; apply query time template
 ; called from TLT4VPR, TLT4XVPR, TLT4DATA
 ; expects TEMPLATE, KEY, KINST, PID, ROOT, ITEM
 ; no PID means use data store
 N OBJECT,JSON,CLTN,SPEC,JPID
 ;
 S JPID=$$JPID4PID^VPRJPR(PID)
 I $L(PID),JPID'="" D
 . N STAMP S STAMP=$O(^VPRPT(JPID,PID,KEY,""),-1) M OBJECT=^VPRPT(JPID,PID,KEY,STAMP) S CLTN=$P(KEY,":",3) I 1
 E  N STAMP S STAMP=$O(^VPRJD(KEY,""),-1) M OBJECT=^VPRJD(KEY,STAMP) S CLTN=$P(KEY,":",3)
 M SPEC=TEMPLATE("collection",CLTN)
 I '$D(SPEC) D  QUIT  ; return whole object if template missing
 . ; Add support for metastamps
 . I $L(PID),JPID'="" N STAMP S STAMP=$O(^VPRPTJ("JSON",JPID,PID,KEY,""),-1) M @ROOT@($J,ITEM)=^VPRPTJ("JSON",JPID,PID,KEY,STAMP) I 1
 . E  N STAMP S STAMP=$O(VPRJDJ("JSON",KEY,"",-1)) M @ROOT@($J,ITEM)=^VPRJDJ("JSON",KEY,STAMP)
 D APPLY^VPRJCT(.SPEC,.OBJECT,.JSON,KINST)
 M @ROOT@($J,ITEM)=JSON
 Q
 ;
GDSHEAD(HASDATA,RETCNTS,TOTAL,COUNT) ; Build object header for generic data stores
 N X
 S X="{"
 I RETCNTS S X=X_"""totalItems"":"_TOTAL_",""currentItemCount"":"_COUNT
 I RETCNTS,HASDATA S X=X_","
 I HASDATA S X=X_"""items"":["
 QUIT X
 ;
BLDHEAD(TOTAL,COUNT,START,LIMIT) ; Build the object header
 N X,UPDATED
 S UPDATED=$$CURRTIME
 S X="{""data"":{""updated"":"_UPDATED_","
 S X=X_"""totalItems"":"_TOTAL_","
 S X=X_"""currentItemCount"":"_COUNT_","
 I LIMIT'=999999 D  ; only set this if paging
 . S X=X_"""itemsPerPage"":"_LIMIT_","
 . S X=X_"""startIndex"":"_START_","
 . ; If LIMIT is 0, make sure to change to 1 so we get back the right info, but don't get a divide by zero error
 . S X=X_"""pageIndex"":"_(START\$S(LIMIT=0:1,1:LIMIT))_","
 . S X=X_"""totalPages"":"_(TOTAL\$S(LIMIT=0:1,1:LIMIT)+$S(TOTAL#$S(LIMIT=0:1,1:LIMIT):1,1:0))_","
 S X=X_"""items"":["
 Q X
 ;
UUID() ;
 N R,I,J,N
 S N="",R="" F  S N=N_$R(100000) Q:$L(N)>64 
 F I=1:2:64 S R=R_$E("0123456789abcdef",($E(N,I,I+1)#16+1)) 
 Q $E(R,1,8)_"-"_$E(R,9,12)_"-4"_$E(R,14,16)_"-"_$E("89ab",$E(N,17)#4+1)_$E(R,18,20)_"-"_$E(R,21,32) 
 ;
 ;
 ; Cache specific functions
 ;
LCLHOST() ; return TRUE if the peer connection is localhost
 I $E($I,1,5)'="|TCP|" Q 0
 N VER,ADDR
 S VER=$P($P($ZV,") ",2),"(")
 I VER<2011 S ADDR=$ZU(111,0),ADDR=$A(ADDR,1)_"."_$A(ADDR,2)_"."_$A(ADDR,3)_"."_$A(ADDR,4) I 1
 E  S ADDR=$SYSTEM.TCPDevice.PeerAddr(0)
 I ADDR="127.0.0.1" Q 1
 I ADDR="0:0:0:0:0:0:0:1" Q 1
 I ADDR="::1" Q 1
 Q 0
 ;
HASH(X) ; return CRC-32 of string contained in X
 Q $$CRC32(X) ; support both GT.M and Cache
 ;
GMT() ; return HTTP date string (this is really using UTC instead of GMT)
 ; from Sam Habiel
 N TM,DAY
 I $$UP($ZV)["CACHE" D  Q $P(DAY," ")_", "_$ZDATETIME(TM,2)_" GMT"
 . S TM=$ZTIMESTAMP,DAY=$ZDATETIME(TM,11)
 ;
 N OUT
 I $$UP($ZV)["GT.M" D  Q OUT
 . N D S D="datetimepipe"
 . N OLDIO S OLDIO=$I
 . O D:(shell="/bin/sh":comm="date -u +'%a, %d %b %Y %H:%M:%S %Z'|sed 's/UTC/GMT/g'")::"pipe"
 . U D R OUT:1 
 . U OLDIO C D
 ;
 QUIT "UNIMPLEMENTED"
 ;
SYSID() ; return a likely unique system ID
 N X
 S X=$SYSTEM_":"_$G(^VPRHTTP("port"),9080) ; from Sam Habiel
 Q $$CRC16HEX(X) ; return CRC16 in HEX
 ;
 ; Begin from Sam Habiel
CRC16HEX(X) ; return CRC-16 in hexadecimal
 QUIT $$BASE($$CRC16(X),10,16) ; return CRC-16 in hex
 ;
CRC32HEX(X) ; return CRC-32 in hexadecimal
 QUIT $$BASE($$CRC32(X),10,16) ; return CRC-32 in hex
 ;
DEC2HEX(NUM) ; return a decimal number as hex
 Q $$BASE(NUM,10,16)
 ;Q $ZHEX(NUM)
 ;
HEX2DEC(HEX) ; return a hex number as decimal
 Q $$BASE(HEX,16,10)
 ;Q $ZHEX(HEX_"H")
 ;
WR4HTTP ; open file to save HTTP response
 I $$UP($ZV)["CACHE" O "VPRJT.TXT":"WNS"
 I $$UP($ZV)["GT.M" O "VPRJT.TXT":(newversion)
 U "VPRJT.TXT"
 Q
RD4HTTP() ; read HTTP body from file and return as value
 N X
 I $$UP($ZV)["CACHE" O "VPRJT.TXT":"RSD" ; read sequential and delete afterwards
 I $$UP($ZV)["GT.M" O "VPRJT.TXT":(readonly:rewind) ; read sequential from the top.
 U "VPRJT.TXT"
 F  R X:1 S X=$TR(X,$C(13)) Q:'$L(X)  ; read lines until there is an empty one ($TR for GT.M)
 R X:2              ; now read the JSON object
 I $$UP($ZV)["GT.M" C "VPRJT.TXT":(delete) U $P
 I $$UP($ZV)["CACHE" D C4HTTP
 Q X
 ;
 ; End from Sam Habiel
C4HTTP ; close file used for HTTP response
 C "VPRJT.TXT"
 U $P
 Q
LOADFILE(FILE,BODY) ; Read from file & put into ARY(line)
 N I,LINE,EOF
 K ARY
 O FILE
 S EOF=0,I=0
 F  D READLN(.LINE) Q:EOF  S I=I+1 S BODY(I)=LINE
 C FILE
 Q
READLN(LINE) ; Read file into array
 ; expects FILE as the file handle
 ; called from LOADFILE
 N $ES,$ET
 S $ET="D CHKEOF^VPRJRUT Q"
 U FILE R LINE:5
 Q
CHKEOF   ; Check for EOF
 I $ZE["ENDOFFILE" S EOF=1,$EC=""
 Q
 ;
CURRTIME() ; Get last access time
 N TIME,LEN
 S TIME=$P($TR($$FMTHL7^XLFDT($$NOW^XLFDT),"-","+"),"+")
 S LEN=14-$L(TIME)
 S TIME=TIME_$E("00",1,LEN)
 Q TIME
 ; 
 ; From Sam Habiel
CRC32(string,seed) ;
 ; Polynomial X**32 + X**26 + X**23 + X**22 +
 ;          + X**16 + X**12 + X**11 + X**10 +
 ;          + X**8  + X**7  + X**5  + X**4 +
 ;          + X**2  + X     + 1
 N I,J,R
 I '$D(seed) S R=4294967295
 E  I seed'<0,seed'>4294967295 S R=4294967295-seed
 E  S $ECODE=",M28,"
 F I=1:1:$L(string) D
 . S R=$$XOR($A(string,I),R,8)
 . F J=0:1:7 D
 . . I R#2 S R=$$XOR(R\2,3988292384,32)
 . . E  S R=R\2
 . . Q
 . Q
 Q 4294967295-R
XOR(a,b,w) N I,M,R
 S R=b,M=1
 F I=1:1:w D
 . S:a\M#2 R=R+$S(R\M#2:-M,1:M)
 . S M=M+M
 . Q
 Q R
BASE(%X1,%X2,%X3) ;Convert %X1 from %X2 base to %X3 base
 I (%X2<2)!(%X2>16)!(%X3<2)!(%X3>16) Q -1
 Q $$CNV($$DEC(%X1,%X2),%X3)
DEC(N,B) ;Cnv N from B to 10
 Q:B=10 N N I,Y S Y=0
 F I=1:1:$L(N) S Y=Y*B+($F("0123456789ABCDEF",$E(N,I))-2)
 Q Y
CNV(N,B) ;Cnv N from 10 to B
 Q:B=10 N N I,Y S Y=""
 F I=1:1 S Y=$E("0123456789ABCDEF",N#B+1)_Y,N=N\B Q:N<1
 Q Y
CRC16(string,seed) ;
 ; Polynomial x**16 + x**15 + x**2 + x**0
 N I,J,R
 I '$D(seed) S R=0
 E  I seed'<0,seed'>65535 S R=seed\1
 E  S $ECODE=",M28,"
 F I=1:1:$L(string) D
 . S R=$$XOR($A(string,I),R,8)
 . F J=0:1:7 D
 . . I R#2 S R=$$XOR(R\2,40961,16)
 . . E  S R=R\2
 . . Q
 . Q
 Q R
 ; End from Sam Habiel

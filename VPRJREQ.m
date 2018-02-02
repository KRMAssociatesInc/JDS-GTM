VPRJREQ ;SLC/KCM -- Listen for HTTP requests
 ;
 ; Listener Process ---------------------------------------
 ;
START(TCPPORT) ; set up listening for connections
 Q:$G(^VPRHTTP(0,"updating"))        ; don't allow starting during upgrade
 ;
 N FLG
 S TCPPORT=$G(TCPPORT,9080)
 ;
 ; Setup for different M implementations/Virtual machines
 N OS S OS=$S(+$SY=47:"GT.M",+$SY=50:"MV1",1:"CACHE") ; Get Mumps Virtual Machine
 I OS="GT.M" D  ; for GT.M, set interrupt.
 . I $T(JOBEXAM^ZSY)]"" S $ZINT="I $$JOBEXAM^ZSY($ZPOS),$$JOBEXAM^VPRJREQ($ZPOS)"
 . E  S $ZINTERRUPT="I $$JOBEXAM^VPRJREQ($ZPOSITION)" 
 ;
 ; Device ID
 S TCPPORT=+$G(TCPPORT)
 I OS="CACHE" S TCPIO="|TCP|"_TCPPORT
 I OS="GT.M" S TCPIO="SCK$"_TCPPORT
 ;
 ; Open Code
 I OS="CACHE" O TCPIO:(/ACCEPT=1:/CRLF=1:/TMODE=1:/PORT=TCPPORT:/NOXY=1:/CONNECTIONS=1000:/IBUFSIZE=$G(^VPRCONFIG("HTTP","inputBuffer"),1048576):/OBUFSIZE=$G(^VPRCONFIG("HTTP","outputBuffer"),1048576)):15 E  U 0 W !,"error" QUIT
 I OS="GT.M" O TCPIO:(LISTEN=TCPPORT_":TCP":delim=$C(13,10):attach="server"):15:"socket" E  U 0 W !,"error cannot open port "_TCPPORT Q
 ;
 ; Now we are really really listening.
 S ^VPRHTTP(TCPPORT,"listener")="running"
 ;
 ; This is the same for GT.M and Cache
 U TCPIO
 ;
 I OS="GT.M" W /LISTEN(5) ; Listen 5 deep - sets $KEY to "LISTENING|socket_handle|portnumber"
 N PARSOCK S PARSOCK=$P($KEY,"|",2)  ; Parent socket
 N CHILDSOCK  ; That will be set below; Child socket
 ;
LOOP ; wait for connection, spawn process to handle it
 S FLG=0
 I $D(^VPRHTTP(TCPPORT,"listener"))#2,$E(^VPRHTTP(TCPPORT,"listener"),1,4)="stop" D
 . C TCPIO
 . S ^VPRHTTP(TCPPORT,"listener")="stopped"
 . S FLG=1
 I FLG QUIT
 D CHRON
 ;
 ; ---- CACHE CODE ----
 I OS="CACHE" D  G LOOP
 . R *X:10
 . E  QUIT  ; Loop back again when listening and nobody on the line
 . J CHILD:(:4:TCPIO:TCPIO):10 ; Send off the device to another job for input and output.
 . I $ZA\8196#2=1 W *-2  ; job failed to clear bit
 ; ---- END CACHE CODE ----
 ;
 ; ----- GT.M CODE ----
 ; In GT.M $KEY is "CONNECT|socket_handle|portnumber" then "READ|socket_handle|portnumber"
 ; N GTMDONE S GTMDONE=0  ; To tell us if we should loop waiting or process HTTP requests ; don't need this anymore
 ; I OS="GT.M" D  G LOOP:'GTMDONE,CHILD:GTMDONE
 I OS="GT.M" D  G LOOP
 . ;
 . ; Wait until we have a connection (inifinte wait). 
 . ; Stop if the listener asked us to stop.
 . FOR  W /WAIT(10) Q:$KEY]""  Q:($E(^VPRHTTP(TCPPORT,"listener"),1,4)="stop")
 . ;
 . ; We have to stop! When we quit, we go to loop, and we exit at LOOP+1
 . I $E(^VPRHTTP(TCPPORT,"listener"),1,4)="stop" QUIT
 . ; 
 . ; At connection, job off the new child socket to be served away.
 . ; I $P($KEY,"|")="CONNECT" QUIT ; before 6.1
 . I $P($KEY,"|")="CONNECT" D  ; >=6.1
 . . S CHILDSOCK=$P($KEY,"|",2)
 . . U TCPIO:(detach=CHILDSOCK)
 . . N Q S Q=""""
 . . N ARG S ARG=Q_"SOCKET:"_CHILDSOCK_Q
 . . N J S J="CHILD:(input="_ARG_":output="_ARG_")"
 . . J @J
 . ;
 . ; GT.M before 6.1:
 . ; Use the incoming socket; close the server, and restart it and goto CHILD
 . ; USE TCPIO:(SOCKET=$P($KEY,"|",2))
 . ; CLOSE TCPIO:(SOCKET="server")
 . ; JOB START^VPRJREQ(TCPPORT):(IN="/dev/null":OUT="/dev/null":ERR="/dev/null"):5
 . ; SET GTMDONE=1  ; Will goto CHILD at the DO exist up above
 . ; ---- END GT.M CODE ----
 ; 
 QUIT
 ;
JOBEXAM(%ZPOS) ; Interrupt framework for GT.M.
 ZSHOW "*":^VPRHTTP("processlog",+$H,$P($H,",",2),$J)
 QUIT 1
 ;
CHRON ; handle events related to passage of time
 ; TODO: start job every n seconds to handle logging check, review xrefs, etc.
 ; turn off logging after 10 minutes
 I $$ESECS($G(^VPRHTTP(0,"logging","start")))>600 D SLOG^VPRJRCL(0)
 Q
ESECS(TS) ; return elapsed seconds since TS (in $H format)
 ; assumes we don't care about magnitude of larger differences
 N D,S
 S D=$H,S=$P(D,",",2),D=$P(D,",")
 S D=D-TS I D>11 Q 999999  ; just return 999999 if >11 days
 S S=S-$P(TS,",",2),S=S+(D*86400)
 Q S
 ;
GTMLNX  ;From Linux xinetd script; $P is the main stream
 S ^VPRHTTP(0,"listener")="starting"
 I $T(JOBEXAM^ZSY)]"" S $ZINT="I $$JOBEXAM^ZSY($ZPOS),$$JOBEXAM^VPRJREQ($ZPOS)"
 E  S $ZINT="I $$JOBEXAM^VPRJREQ($ZPOS)"
 X "U $P:(nowrap:nodelimiter:ioerror=""ETSOCK"")"
 S %="",@("%=$ZTRNLNM(""REMOTE_HOST"")") S:$L(%) IO("IP")=%
 G CHILD
 ;
 ; Child Handling Process ---------------------------------
 ;
 ; The following variables exist during the course of the request
 ; HTTPREQ contains the HTTP request, with subscripts as follow --
 ; HTTPREQ("method") contains GET, POST, PUT, HEAD, or DELETE
 ; HTTPREQ("path") contains the path of the request (part from server to ?)
 ; HTTPREQ("query") contains any query params (part after ?)
 ; HTTPREQ("header",name) contains a node for each header value
 ; HTTPREQ("body",n) contains as an array the body of the request
 ; HTTPREQ("location") stashes the location value for PUT, POST
 ; HTTPREQ("store") stashes the type of store (vpr or data)
 ;
 ; HTTPRSP contains the HTTP response (or name of global with the response)
 ; HTTPLOG indicates the logging level
 ; HTTPLOG("this") is logging level for this process only
 ; HTTPLOG("path") is a url pattern to match
 ; HTTPERR non-zero if there is an error state
 ;
CHILD ; handle HTTP requests on this connection
 N TCP S TCP=$GET(TCPIO,$PRINCIPAL) ; TCP Device
 N OS S OS=$S(+$SY=47:"GT.M",+$SY=50:"MV1",1:"CACHE") ; Get Mumps Virtual Machine
 S HTTPLOG("DT")=+$H
 N $ET S $ET="G ETSOCK^VPRJREQ"
 ;
NEXT ; begin next request
 K HTTPREQ,HTTPRSP,HTTPERR
 K ^TMP($J),^TMP("HTTPERR",$J) ; TODO: change the namespace for the error global
 S HTTPLOG=$S($D(HTTPLOG("this")):HTTPLOG("this"),1:$G(^VPRHTTP(0,"logging"),0))
 I HTTPLOG=2,'$D(HTTPLOG("path")) S HTTPLOG("path")=$G(^VPRHTTP(0,"logging","path"))
 ;
WAIT ; wait for request on this connection
 X:OS="CACHE" "U TCP:(::""CT"")" ;VEN/SMH - Cache Only line; Terminators are $C(10,13)
 X:OS="GT.M" "U TCP:(delim=$C(13,10))" ; VEN/SMH - GT.M Delimiters
 R TCPX:10 I '$T G WAIT
 I '$L(TCPX) G WAIT
 ;
 ; -- got a request and have the first line
 ; only increment the logger if we have logging enabled
 D:HTTPLOG INCRLOG ; set unique request id
 I HTTPLOG>3 D LOGRAW(TCPX)
 S HTTPREQ("line1")=TCPX
 S HTTPREQ("method")=$P(TCPX," ")
 S HTTPREQ("path")=$P($P(TCPX," ",2),"?")
 S HTTPREQ("query")=$P($P(TCPX," ",2),"?",2,999)
 ; TODO: time out connection after N minutes of wait
 ; TODO: check format of TCPX and raise error if not correct
 I $E($P(TCPX," ",3),1,4)'="HTTP" G NEXT
 ;
 ; -- read the rest of the lines in the header
 F  S TCPX=$$RDCRLF() Q:'$L(TCPX)  D ADDHEAD(TCPX)
 ;
 ; -- decide how to read body, if any
 X:OS="CACHE" "U TCP:(::""S"")" ; Stream mode
 X:OS="GT.M" "U TCP:(nodelim)" ; VEN/SMH - GT.M Delimiters
 ; TODO: handle chunked input of body
 I $$LOW^VPRJRUT($G(HTTPREQ("header","transfer-encoding")))="chunked" D RDCHNKS
 ; handle regular input of body
 I $G(HTTPREQ("header","content-length"))>0 D RDLEN(HTTPREQ("header","content-length"),99)
 ;
 ; -- build response (map path to routine & call, otherwise 404)
 S $ETRAP="G ETCODE^VPRJREQ"
 S HTTPERR=0
 D RESPOND^VPRJRSP
 S $ETRAP="G ETSOCK^VPRJREQ"
 ; TODO: restore HTTPLOG if necessary
 ;
 ; -- write out the response (error if HTTPERR>0)
 X:OS="CACHE" "U TCP:(::""S"")" ; Stream mode
 X:OS="GT.M" "U TCP:(nodelim)" ; VEN/SMH - GT.M Delimiters
 I $G(HTTPERR) D RSPERROR^VPRJRSP ; switch to error response
 D SENDATA^VPRJRSP
 I HTTPLOG D LOGGING
 ;
 ; -- exit on Connection: Close
 I $$LOW^VPRJRUT($G(HTTPREQ("header","connection")))="close" D  Q
 . K ^TMP($J),^TMP("HTTPERR",$J)
 . C TCP
 ;
 ; -- otherwise get ready for the next request
 G NEXT
 ;
RDCRLF() ; read a header line
 ; fixes a problem where the read would terminate before CRLF
 ; (on a packet boundary or when 1024 characters had been read)
 N X,LINE,RETRY
 S LINE=""
 F RETRY=1:1 R X:1 D:HTTPLOG>3 LOGRAW(X) S LINE=LINE_X Q:$A($ZB)=13  Q:RETRY>10
 Q LINE
 ;
RDCHNKS ; read body in chunks
 Q  ; still need to implement
 ;
RDLEN(REMAIN,TIMEOUT) ; read L bytes with timeout T
 N X,LINE,LENGTH
 S LINE=0
RDLOOP ;
 ; read until L bytes collected
 ; quit with what we have if read times out
 S LENGTH=REMAIN I LENGTH>4000 S LENGTH=4000
 R X#LENGTH:TIMEOUT
 I '$T D:HTTPLOG>3 LOGRAW("timeout:"_X) S LINE=LINE+1,HTTPREQ("body",LINE)=X Q
 I HTTPLOG>3 D LOGRAW(X)
 S REMAIN=REMAIN-$L(X),LINE=LINE+1,HTTPREQ("body",LINE)=X
 G:REMAIN RDLOOP
 Q
 ;
ADDHEAD(LINE) ; add header name and header value
 ; expects HTTPREQ to be defined
 N NAME,VALUE
 S NAME=$$LOW^VPRJRUT($$LTRIM^VPRJRUT($P(LINE,":")))
 S VALUE=$$LTRIM^VPRJRUT($P(LINE,":",2,99))
 I LINE'[":" S NAME="",VALUE=LINE
 I '$L(NAME) S NAME=$G(HTTPREQ("header")) ; grab the last name used
 I '$L(NAME) Q  ; no header name so just ignore this line
 I $D(HTTPREQ("header",NAME)) D
 . S HTTPREQ("header",NAME)=HTTPREQ("header",NAME)_","_VALUE
 E  D
 . S HTTPREQ("header",NAME)=VALUE,HTTPREQ("header")=NAME
 Q
 ;
ETSOCK ; error trap when handling socket (i.e., client closes connection)
 D LOGERR
 C TCP
 HALT  ; exit because connection has been closed
 ;
ETCODE ; error trap when calling out to routines
 S $ETRAP="G ETBAIL^VPRJREQ"
 I $TLEVEL TROLLBACK ; abandon any transactions
 L                   ; release any locks
 ; Set the error information and write it as the HTTP response.
 D LOGERR
 D SETERROR^VPRJRER(501,"Log ID:"_HTTPLOG("ID")) ; sets HTTPERR
 D RSPERROR^VPRJRSP  ; switch to error response
 D SENDATA^VPRJRSP
 ; Leave $ECODE as non-null so that the error handling continues.
 ; This next line will 'unwind' the stack and got back to listening
 ; for the next HTTP request (goto NEXT).
 S $ETRAP="Q:$ESTACK&$QUIT 0 Q:$ESTACK  S $ECODE="""" G NEXT"
 Q
ETBAIL ; error trap of error traps
 U TCP
 W "HTTP/1.1 500 Internal Server Error",$C(13,10),$C(13,10),!
 C TCP H 1
 K ^TMP($J),^TMP("HTTPERR",$J)
 HALT  ; exit because we can't recover
 ;
INCRLOG ; get unique log id for each request
 N DT,ID
 S DT=HTTPLOG("DT")
 S ID=$I(^VPRHTTP("log",DT))
 S HTTPLOG("ID")=ID
 Q
LOGGING ; log non-error information based on log level
 ; HTTPLOG=0:  log hard errors only
 ; HTTPLOG=1:  log HTTP errors
 ; HTTPLOG=2:  log requests matching path specified by HTTPLOG("path")
 ; HTTPLOG=3:  log all requests
 ; HTTPLOG=4:  log raw reads
 ;
 I HTTPLOG=1,'$G(HTTPERR) Q
 I HTTPLOG=2,'$$MATCH(HTTPREQ("path"),HTTPLOG("path")) Q
 N DT,ID
 S DT=HTTPLOG("DT"),ID=HTTPLOG("ID")
 S ^VPRHTTP("log",DT,$J,ID)=$$HTE^XLFDT($H)_"  $J:"_$J_"  $P:"_$P
 S ^VPRHTTP("log",DT,$J,ID,"type")=$S($D(^VPRHTTP("log",DT,$J,ID,"error"))=1:0,1:HTTPLOG)
 S ^VPRHTTP("log",DT,$J,ID,"line1")=HTTPREQ("line1")
 M ^VPRHTTP("log",DT,$J,ID,"header")=HTTPREQ("header")
 M ^VPRHTTP("log",DT,$J,ID,"body")=HTTPREQ("body")
 I $L($G(HTTPLOG("name"))) D
 . S ^VPRHTTP("log",DT,$J,ID,"name")=HTTPLOG("name")
 . S ^VPRHTTP("log","names",HTTPLOG("name"),DT,$J,ID)="" ; xref by name
 I $G(HTTPERR) M ^VPRHTTP("log",DT,$J,ID,"error","http")=^TMP("HTTPERR",$J)
 M ^VPRHTTP("log",DT,$J,ID,"response","body")=HTTPRSP
 M ^VPRHTTP("log",DT,$J,ID,"response","request")=HTTPREQ
 Q
LOGRAW(X) ; log raw lines read in
 N DT,ID,LN
 S DT=HTTPLOG("DT"),ID=HTTPLOG("ID")
 S LN=$G(^VPRHTTP("log",DT,$J,ID,"raw"),0)+1
 S ^VPRHTTP("log",DT,$J,ID,"raw")=LN
 S ^VPRHTTP("log",DT,$J,ID,"raw",LN)=X
 S ^VPRHTTP("log",DT,$J,ID,"raw",LN,"ZB")=$A($ZB)
 Q
LOGERR ; log error information
 ; Always increment the log id for errors
 D INCRLOG
 N %D,%I
 S %D=HTTPLOG("DT"),%I=HTTPLOG("ID")
 N ISGTM S ISGTM=$P($SYSTEM,",")=47
 S ^VPRHTTP("log",%D,$J,%I,"error")=$S(ISGTM:$ZSTATUS,1:$ZERROR_"  ($ECODE:"_$ECODE_")")
 N %LVL,%TOP,%N
 S %TOP=$STACK(-1),%N=0
 F %LVL=0:1:%TOP S %N=%N+1,^VPRHTTP("log",%D,$J,%I,"error","stack",%N)=$STACK(%LVL,"PLACE")_":"_$STACK(%LVL,"MCODE")
 N %X,%Y
 S %X="^VPRHTTP(""log"",%D,$J,%I,""error"",""symbols"","
 ;
 S %Y="%" F  M:$D(@%Y) @(%X_"%Y)="_%Y) S %Y=$O(@%Y) Q:%Y=""
 ;
 D LOGGING
 Q
MATCH(X,P,D) ;
 N I,OK,MAX
 S D=$G(D,"/")
 I '$L(P) Q 0 ; no pattern returns false
 F I=1:1:$L(X,D) S X(I)=$P(X,D,I),MAX=I
 F I=1:1:$L(P,D) S P(I)=$P(P,D,I) S:I>MAX MAX=I
 S OK=1
 F I=1:1:MAX Q:$G(P(I))="..."  D  Q:'OK
 . I $D(P(I))'=1 S OK=0 Q
 . I P(I)="*" Q
 . I $D(X(I))'=1 S OK=0 Q
 . I P(I)'=X(I) S OK=0
 Q OK
 ;

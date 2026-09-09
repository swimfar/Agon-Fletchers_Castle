10 MODE 1
11 VDU 23, 0, &C0, 1
12 PRINT TAB(11,11);CHR$141; CHR$(133); "FLETCHER'S CASTLE"
14 PRINT TAB(11,12);CHR$141; CHR$(133); "FLETCHER'S CASTLE"
20 PRINT TAB(0,23);CHR$(130);"When you are ready to continue press C";:I=FNPRESS("C")
30 ON ERROR GOTO 16000
40 DY%=0                        REM Day number (1 to 10)
41 SO%=89+RND(9)                REM Number of soldiers available
42 WO%=84+RND(11)               REM Number of workers available
43 DIM WK%(5,10)
44 DIM UN%(5,5)   REM UN%(site number, )
45 DIM SJ%(5,10)
46 DIM WS%(5,10)
47 DIM CM%(5)
50 S1$=CHR$(88+(RND(2)-1)*2)    REM Site option 1 (x or x)
51 S2$=CHR$(84+(RND(2)-1)*3)    REM Location x or x
52 S3$=CHR$(86+(RND(2)-1)*3)    REM Location x or x
60 FOR I=0 TO 5
61   FOR J=0 TO 5
62     READ UN%(J,I)
63     WK%(J,I)=0
64     WK%(J,I+5)=0
65   NEXT J
66 NEXT I
70 DATA 400,500,400,200,400,250
71 DATA 0,0,0,0,0,0
72 DATA 300,300,300,500,400,450
73 DATA 150,150,150,150,150,150
74 DATA 250,250,250,250,250,250
75 DATA 0,0,0,0,0,0
75 READ HIGH%:DATA 400
80 DIM LS%(5),GS%(5),LW%(5),GW%(5)  REM site-based data
81 DIM TASK$(5),job$(5)
82 FOR I=0 TO 5
83   READ LS%(I)
84   READ GS%(I)
85   READ LW%(I)
86   READ GW%(I)
87   READ TASK$(I)     REM Task string
88   READ job$(I)      REM Task action string
89 NEXT
90 DATA 10,5,10,5,"Prepare ditch and mound","preparing the ground"
91 DATA 1,2,3,2,"Set guards",""
94 DATA 6,4,0,4,"Fell and haul trees","collecting timber"
95 DATA 6,4,6,4,"Build bailey fence","building the bailey fence"
96 DATA 1,2,3,2,"Build tower and fence","building the tower and fence"
97 DATA 8,4,8,4,"Obtain food",""
100 DIM LGTH%(15,2),ST%(15,2),FR%(15,2),T%(15,2),RAT(15,2)
110 FOR J=0 TO 2
111   FOR I=0 TO 15
112     READ LGTH%(I,J)
113     READ ST%(I,J)
114     READ FR%(I,J)
115     READ T%(I,J)
116     READ RAT(I,J)
117   NEXT I
118 NEXT J
120 DATA 63,512,1088,1120,0.5
121 DATA 95,448,1120,1152,-0.33333
122 DATA -159,1120,384,352,-0.2
123 DATA -127,960,352,320,0
124 DATA -191,832,352,320,0.16666
125 DATA 63,384,640,672,0.5
126 DATA 95,320,672,704,-0.33333
127 DATA -159,672,256,224,-0.2
128 DATA -127,512,224,192,0
129 DATA -127,384,224,192,0
130 DATA -127,256,224,192,0.25
131 DATA 63,512,352,384,-0.5
132 DATA -159,352,480,448,-0.2
133 DATA -95,192,448,416,-0.33333,63,416,96,128,0,127,352,96,128,0.25
140 DATA -95,608,352,288,-0.33333,-127,512,320,256,0,-127,384,320,256,0,-63,256,320,256,0.5,-31,192,352,288,2,-31,608,352,288,3,-31,576,448,384,0.25,-31,480,472,408,0.25,-95,448,480,416,0,-95,352,480,416,0,-95,256,480,416,-0.66667
150 DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
160 DATA -95,864,704,640,-1,-95,768,608,544,-1,-95,672,512,448,-0.66667,-63,608,704,640,-1.5,-63,544,608,544,-1,-63,480,544,480,-1,-95,960,704,640,0,-63,864,704,640,0,-63,672,704,640,0,-95,608,704,640,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
170 DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
300 MODE 4:PROCOFF:CLS
305 PRINT TAB(0,3);"Your Task"
306 GCOL 0,1:MOVE 0,888:DRAW 288,888
307 PRINT TAB(0,7);"In this program you will be making"''"decisions about building a castle"''"in early Norman England."''''"You will need:"
310 PRINT''"-A list of jobs to be done."'''"-Some rough note paper."'
315 PROCC
320 FOR K=0 TO 5
321   WK%(K,0)=UN%(0,K)
322 NEXT K
323 DY=0
324 SI%=0             REM Site number T:0, U:0, V:1, W:2, X:3, Y:4, Z:5
329 REM 330 MODE 5              REM BBC 40cx25r
330 MODE 1                  REM Agon 64cx48r
335 PROCCASTLE(FALSE)
340 PRINT TAB(13,28);"     ";TAB(0,27);"This is"''"Fletcher's Castle."''"To continue press C";
350 I=FNPRESS("C")
360 MODE 4:PROCOFF
400 CLS:PRINT TAB(0,4);"Background":GCOL0,1:MOVE 0,856:DRAW 320,856:PRINT TAB(0,7);"You are Simon Fletcher, a Norman Lord."''"Your loyalty and courage at Hastings"''"have been rewarded by William with a"''"grant of land which you must defend."
410 PRINT'''"Your first task is to build a castle"''"as quickly as possible in order that"''"you can defend yourself against"''"the organised Saxon raiders."
420 PROCC
800 CLS
801 PRINT TAB(0,2);"The Castle"
802 GCOL0,1:MOVE 0,920:DRAW 320,920
803 PRINT TAB(0,6);"The type of castle for your purpose"''"is a wooden motte and bailey which"''"can be built by local workmen."''';SO%;" soldiers can be used for the work."
810 PRINT'';WO%;" workers have been found locally"''"but they cannot be trusted alone."'''"First you must decide where to build."
820 PROCC
1000 CLS:PRINT''"The Site"
1001 GCOL0,1:MOVE 0,920:DRAW 256,920
1002 PRINT TAB(0,9);"You have drawn a rough map of the area"'''"and have marked three possible sites."
1010 PRINT''"All have advantages and disadvantages.":PROCC
1050 MODE 5:PROCMAP
1051 COLOUR 2:PRINT TAB(0,7);"Press"''S1$","S2$''"or "S3$"."
1070 CH$=S1$+S2$+S3$:CH$=MID$(CH$,FNPRESS(CH$),1):SI%=ASC(CH$)-85:IF SI%<0 THEN SI%=0
1080 MODE 4:PROCOFF                  REM 80cx32r
1085 PRINT TAB(0,15);"You have chosen site "CH$"."
1090 PRINT TAB(0,25);"To change your mind press A."''"If this is correct press C."
1091 IF FNPRESS("AC")=1 GOTO 1000
1100 CLS:PRINT TAB(0,3);"Jobs to be done":GCOL0,1:MOVE 0,888:DRAW 480,888:PRINT TAB(0,7);"Various jobs will have to be done."''"Some jobs can be done by soldiers"''"and workers but only soldiers can"''"stand guard or forage for food."
1110 PRINT''"If you fail to set enough soldiers"''"for these tasks each day, then you"''"may lose men in attacks or because"''"soldiers and men are short of food.":PROCC
1300 CLS:PRINT TAB(0,3);"The Workers":GCOL0,1:MOVE 0,888:DRAW 352,888:PRINT TAB(0,7);"Remember that ";SO%;" soldiers can be used."'''"So far they have found ";WO%;" men suitable"''"to use as workers on this task."'''"Is this enough men?"
1310 PRINT''"Should you look for more?"'''"You have ten days to build the Castle."''''"If you wish to find more men press M."'''"If you wish to start building press S."
1320 IF FNPRESS("MS")=2 GOTO 4740
1400 CLS:PRINT TAB(0,2);"Extra Workers"
1401 GCOL 0,1:MOVE 0,920:DRAW 416,920
1402 PRINT TAB(0,9);"How many soldiers would you like"''"to send out in search of workers?"
1403 PROCON:CH$=FNINPUT(0,14,""):PROCOFF:SE%=VAL(CH$):IF SE%<1 GOTO 1400
1410 IF SE%>SO% THEN PRINT TAB(0,18);"You do not have enough soldiers.":PROCC:GOTO 1400
REM EX: Extra workers found, AM: Soldiers lost
1500 IF SE%<10 THEN AM%=RND(6)-1:EX%=5+RND(13):DY%=1 ELSE IF SE%>19 THEN AM%=9+RND(11):EX%=15+RND(21):DY%=2 ELSE AM%=5+RND(5):EX%=10+RND(9):DY%=1
1510 IF AM%>=SE% THEN AM%=SE%:EX%=0
1520 SO%=SO%-SE%:GOTO 4740
1600 S%=S%+SE%-AM%:W%=W%+EX%
1610 CLS:PRINT''"The Workforce"
1611 GCOL0,1:MOVE 0,920:DRAW 416,920
1612 PRINT TAB(0,6);"There is news about the search party."''''"You have found ";EX%;" more workers."''"You now have ";W%;" workmen."
1620 PRINT'''"You lost ";AM%;" soldiers in an ambush."''"You now have ";S%;" soldiers."''''"Finding more workers has taken time."''"You now have just ";10-DY%;" days left."
1630 PROCC:GOTO 5200
4740 REM LINE TO JUMP TO 
4790 FOR K=0 TO 5
4791   WK%(K,0)=UN%(SI%,K)
4792 NEXT K
4793 DY=0
4794 MODE 5:PROCCASTLE(TRUE)
4796 PRINT TAB(0,31);"To continue press C";:I=FNPRESS("C")
4797 MODE 4:PROCOFF:FOR K=0 TO 5:WK%(K,0)=0:NEXTK
4800 CLS:PRINT TAB(0,2);"The Tasks":GCOL0,1:MOVE 0,920:DRAW 288,920:CH$=" units.":PRINT TAB(0,5);"For each job one soldier or worker"''"can do one unit of work each day."'''"These are the jobs to be done:-"
4810 PRINT''TASK$(2);TAB(28);UN%(SI%,2);CH$''TASK$(0);TAB(28);UN%(SI%,0);CH$''TASK$(4);TAB(28);UN%(SI%,4);CH$''TASK$(3);TAB(28);UN%(SI%,3);CH$''TASK$(5);TAB(28);"daily."''TASK$(1);TAB(28);"daily.":PROCC
5000 CLS:DY=1:F1%=RND(7)+2:A1%=RND(8)+1
5040 F2%=RND(7)+2:IF F1%=F2% GOTO 5040
5050 A2%=RND(8)+1:IF A1%=A2% GOTO 5050
5060 A3%=RND(8)+1:IF A1%=A3% OR A2%=A3% GOTO 5060
5190 S%=SO%:W%=WO%
5200 CLS:I1%=S%:I2%=W%:PROCT
5220 PRINT TAB(32,5);"S   W":FOR K=0TO 5:CM%(K)=0:NEXT K
5230 FOR K=0TO 5:CM%(K)=(UN%(SI%,K)-WK%(K,DY-1))=0:NEXTK:CM%(1)=FALSE:CM%(5)=FALSE
5240 FOR K=0TO 5:IF NOT CM%(K)THEN PRINT TAB(0,7+K*2);K+1;". ";TASK$(K)
5250 NEXT K
5260 PRINT TAB(36,9);"#";TAB(36,17);"#"
5310 PROCCLPS(20)
5320 PROCON:CH$=FNINPUT(0,20,"Type in job number "):PROCOFF
5330 I=VAL(CH$):IF I<>INT(I) OR I<1 OR I>6 GOTO 5310
5340 I=I-1:J=UN%(SI%,I)-WK%(I,DY-1)
5345 IF I=1 OR I=5 GOTO 5360
5350 PRINT TAB(23,20);"Units needed ";INT(J)
5355 PROCCLPS(22)
5360 PROCON:CH$=FNINPUT(0,22,"How many soldiers for this job?  "):PROCOFF:SJ%(I,DY)=VAL(CH$)
5365 IF SJ%(I,DY)<0 GOTO 5355
5370 L=0:FOR K=0 TO 5:L=L+SJ%(K,DY):NEXT K
5380 IF L<=S% GOTO 5400
5390 PROCCLPS(24):PRINT TAB(0,24);"You do not have enough soldiers.":PROCC:PROCCLPS(22):GOTO 5360
5400 PROCNUM(SJ%(I,DY)):PRINT TAB(30,7+I+I);CH$
5410 IF I=1 OR I=5 GOTO 5480
5415 PROCCLPS(24)
5420 PROCON:CH$=FNINPUT(0,24,"How many workers?  "):PROCOFF:WS%(I,DY)=VAL(CH$)
5425 IF WS%(I,DY)<0 GOTO 5415
5430 L=0:FOR K=0 TO 5:L=L+WS%(K,DY):NEXT K
5440 IF L<=W% GOTO 5470
5450 PRINT TAB(0,26);"You do not have enough workers.":PROCC:PROCCLPS(24):GOTO 5415
5470 PROCNUM(WS%(I,DY)):PRINT TAB(34,7+I+I);CH$
5480 PRINT TAB(0,28);"To change this press A."''"To continue press C."
5490 IF FNPRESS("AC")=1 THEN SJ%(I,DY)=0:WS%(I,DY)=0:CM%(I)=0:PRINT TAB(30,7+I+I);"       ":PROCCLPS(20):GOTO 5310
5510 CM%(I)=TRUE:L=0:FOR K=0 TO 5:L=L-CM%(K):NEXT K
5520 IF L=6 GOTO 5530
5521 I1%=S%:I2%=W%:FOR K=0 TO 5:I1%=I1%-SJ%(K,DY):I2%=I2%-WS%(K,DY):NEXT K
5522 PROCT
5523 IF I1%>0 OR I2%>0 GOTO 5310
5530 PROCCLPS(20):PROCC:CLS
5540 I1%=0:I2%=0
5550 FOR K=0 TO 5:CM%(K)=SJ%(K,DY)+WS%(K,DY):NEXT K
5600 IF SJ%(5,DY)>=S%/5 GOTO 5630
5610 I2%=W%*0.15
5620 CLS:PRINT TAB(0,10);"Lack of food has cost you ";I2%;" workers.":PROCC
5630 IF SJ%(1,DY)>9 GOTO 5670
5640 I1%=S%/5:FOR K=0 TO 5:CM%(K)=0:NEXT K
5650 CLS:PRINT TAB(0,10);"You have not posted enough guards."'''"You lose ";I1%;" soldiers in an enemy attack."'''''"No work is done because"''"of this attack."
5660 PROCC:GOTO 5730
5670 PROCtest(3)
5671 PROCtest(4)
5672 PROCtest(0)
5673 PROCtest(2)
5679 IF WK%(0,DY-1)+CM%(0)<UN%(SI%,0)THEN CM%(4)=0
5680 I=(WK%(2,DY-1)+CM%(2))*(UN%(SI%,3)+UN%(SI%,4))/(UN%(SI%,2))
5690 J=I-WK%(3,DY-1)-WK%(4,DY-1):IF CM%(3)>J THEN CM%(3)=J
5700 J=J-CM%(3):IF CM%(4)>J THEN CM%(4)=J
5730 FOR K=0TO 5:WK%(K,DY)=WK%(K,DY-1)+CM%(K)
5740   IF WK%(K,DY)>UN%(SI%,K)THEN WK%(K,DY)=UN%(SI%,K)
5750 NEXTK
5760 IF DY<>F1%ANDDY<>F2%GOTO 5820
5770 I3%=S%*(RND(GS%(SI%))+LS%(SI%))/100:I4%=W%*(RND(GW%(SI%))+LW%(SI%))/100
5780 CLS:PRINT TAB(0,8);"The enemy launch a surprise attack."
5790 PRINT'';I3%;" soldiers are killed."
5800 PRINT'';I4%;" workers escape."
5810 PROCC:I1%=I1%+I3%:I2%=I2%+I4%
5820 IF DY<>A1% AND DY<>A2% AND DY<>A3% GOTO 5870
5830 I4%=W%*((RND(15)+5)/100)
5840 CLS:PRINT TAB(0,10);"Accidents at work mean that ";I4%;" workers"'''"are injured and are sent home."
5860 PROCC:I2%=I2%+I4%
5870 S%=S%-I1%:W%=W%-I2%
5880 IF S%>29GOTO 5940
5890 CLS:PRINT TAB(0,3);"You now have only ";S%;" soldiers left."'''"This is not enough to continue the"'''"building and you must escape to avoid"'''"being captured."
5891 PROCC:GOTO 9000
5940 IF W%>29 GOTO 6000
5950 CLS:PRINT TAB(0,3);"You now have only ";W%;" workers left."'''"You cannot continue building with"'''"this number so you must abandon"'''"the work and try again at a later date."
5960 PROCC:GOTO 9000
6000 CLS
6010 PRINT TAB(0,11);"You will now see the result"'''"of the work you have done so"'''"far in graphic form.":PROCC
6100 MODE 5:PROCCASTLE(TRUE):PRINT TAB(0,31);"To continue press C";:I=FNPRESS("C"):MODE 4:PROCOFF
6130 IF DY=10GOTO 6400
6140 IF WK%(4,DY)=UN%(SI%,4) AND WK%(3,DY)=UN%(SI%,3)GOTO 6200
6150 IF DY=DY%THEN DY=DY+1:GOTO 1600
6160 DY=DY+1:GOTO 5200
6200 CLS:PRINT TAB(0,2);"Fletcher's Castle":GCOL0,1:MOVE 0,920:DRAW 544,920:PRINT TAB(0,8);"Well done."'''''"You have finished building your Castle"''"and you are well on the way to being"''"the Lord of the Manor."
6210 PROCC:GOTO 9000
6400 IF WK%(3,DY)=UN%(SI%,3)GOTO 6600
6410 W%=W%/2:S%=S%/2:CLS
6420 PRINT TAB(0,7);"You have failed to finish the fence"'''"in the time allowed."'''"This means you lose many men through"'''"enemy attacks."'''"You flee to safety."
6430 PROCC:GOTO 9000
6600 IF WK%(4,DY)=UN%(SI%,4)GOTO 6200
6610 CLS
6620 S%=S%*0.8:W%=W%*0.8
6630 PRINT TAB(0,4);"You have built your fence and this"'''"will protect you while you finish"'''"your tower."'''"You will lose men because of your"'''"slowness but you should be able to"'''"complete your task and become Lord"
6640 PRINT''"of the district."
6650 PROCC:GOTO 9000
9000 MODE 7:PROCOFF:CH$=CHR$(130)+CHR$(141)+"Game Over":PRINT TAB(13,10);CH$'TAB(13);CH$
9010 PRINT''''; CHR$(133); "To play again press A."'''; CHR$(129); "To end the program press E."
9020 IF FNPRESS("AE")=1 CLS:CLEAR:GOTO 50
9030 REM 9998PRINT''''':FOR I=1TO 2:PRINT TAB(11); CHR$141; CHR$(136); CHR$(131); "SWITCH OF F":NEXT
9997 PRINT'''''
9998 FOR I=1 TO 2:PRINT TAB(11); CHR$141; CHR$(136); CHR$(131); "SWITCH OFF":NEXT
9999 REPEAT UNTIL 0
10000 DEF PROCC
10010 LOCAL I
10020 PRINT TAB(0,30);"When you are ready to continue press C."
10030 I=FNKEY:IF I<>67 THEN GOTO 10010 ELSE ENDPROC
10499 :
10500 DEF FNPRESS(K$):LOCAL I,J,C$:I=LEN(K$)
10510 C$=CHR$(FNKEY):J=0
10520 J=J+1:IF J>I GOTO 10510
10530 IF C$<>MID$(K$,J,1)THEN GOTO 10520ELSE =J
10899 :
10900 DEF FNKEY
10901 REM Return ASCII value of upper case key
10905 REPEAT UNTIL INKEY(0) = -1
10910 LOCAL I:I=GET:IF I>96 AND I<123 THEN I=I-32
10920 IF I=38 THEN STOP ELSE =I
12000 DEF PROCDELAY(I):LOCAL J:J=TIME+I*100:REPEAT:UNTIL J<TIME:ENDPROC
15000 DEF PROCtest(task%)
15010 IF WS%(task%,DY)=0 GOTO 15100
15020 IF SJ%(task%,DY)>0:IF WS%(task%,DY)DIV(7+RND(3))<SJ%(task%,DY)GOTO 15100
15030 CLS:VDU 7,7,7:PRINT TAB(0,10);"There were not enough soldiers assigned"''"to "job$(task%)" to make"''"sure the workers did their jobs."''"No work is done on this task today."
15040 PROCC
15050 CM%(task%)=0
15100 ENDPROC
16000 MODE 7:PROCOFF
16010 REPORT:PRINT" at line ";ERL
16020 IF ERR<>17 GOTO 9998
16030 STOP
20000 DEF PROCOFF:VDU 23, 1,0:ENDPROC
20100 DEF PROCON:VDU 23, 1,1:REPEAT UNTIL INKEY(0)=-1
20110 ENDPROC
20200 DEF PROCT:PRINT TAB(0,2);"Day ";DY;". You have ";I1%;" soldiers ";I2%;" workers";TAB(39);:ENDPROC
20300 DEF PROCCLPS(L):VDU 28,0,31,39,L:CLS:VDU 28,0,31,39,0:ENDPROC
20400 DEF PROCNUM(I):CH$=RIGHT$("   "+STR$(I),3):ENDPROC
20500 DEF FNL(L):IF L>1 THEN =1 ELSE =L
20999 :
21000 DEF PROCCASTLE(ST%)
21001 PROCOFF
REM 21002 VDU 19,1,0, 0,0,0
REM 21003 VDU 19,2,0, 0,0,0
21002 VDU 19,0,6,0;0 :               REM Logical color 0 to cyan
21003 VDU 19,1,8,0,0 :               REM Logical color 1 to green
21004 VDU 19,2,3,0,0 :               REM 2 to yellow
21005 VDU 19,3,1,0;0 :               REM 3 to red
21010 REM Set logical colors 1, 2, 3 to black, black, red
21030 LOCAL I,J,K,K%,L
REM 21040 GCOL 0,1              REM Draw black on black to hide drawing process
21040 GCOL 0,1                  REM Draw green
21049 PRINT "Draw grass"
21050 MOVE 0,0:MOVE 1272,0:PLOT 85,0,512:PLOT 85,1272,512
21055 J=512+(HIGH%-UN%(SI%,0))/3
21056 PRINT "Draw 2"
21060 FOR I=512 TO J STEP 4:MOVE I-128,I:DRAW 1600-I,I:NEXT I
REM 21070 VDU 19,1,8,0,0 :               REM Logical color 1 to green
REM 21071 VDU 19,0,6,0;0 :               REM Logical color 0 to cyan
21072 PROCDELAY(3)
21080 K=(678-J)/(UN%(SI%,0))
21081 L=J+K*WK%(0,DY)
21089 PRINT "Draw Mound"
21090 FOR I=J TO L:MOVE I-128,I:DRAW 1600-I,I:NEXT I:IF WK%(0,DY)=0 GOTO 21200
21099 PRINT "Draw moat"
21100 K=16*WK%(0,DY)/UN%(SI%,0)
21110 GCOL 0,0              REM Draw cyan
21120 K%=K:PROCCASA(0)
21200 IF ST% COLOUR 2:COLOUR 129:PRINT TAB(13,28);"Trees":PROCDELAY(1.5)
21205 IF WK%(2,DY)=0 OR NOT ST% GOTO 21300
21210 K=(160*WK%(2,DY))/UN%(SI%,2):GCOL0,3:FOR I=144TO K+144:MOVE 768,I:DRAW 1216,I:NEXT I
21300 PROCDELAY(1.5)
21305 IF WK%(3,DY)=0 GOTO 21400
21310 IF ST%K=(160*WK%(3,DY))/(UN%(SI%,3)+UN%(SI%,4)):GCOL 0,1
21315 FOR I=144 TO K+144:MOVE 768,I:DRAW 1216,I:NEXT I
21320 K=11*WK%(3,DY)/UN%(SI%,3):GCOL0,3:K%=K:PROCCASA(1)
21400 PROCDELAY(1.5):IF WK%(4,DY)=0 GOTO 21999
21410 IF ST%K=(160*(WK%(3,DY)+WK%(4,DY)))/(UN%(SI%,3)+UN%(SI%,4)):GCOL 0,1
21415 FOR I=144 TO K+144:MOVE 768,I:DRAW 1216,I:NEXT I
21420 K=14*WK%(4,DY)/UN%(SI%,4):GCOL 0,3:M=-1:IF K>10 THEN M=K-10:K=10
21430 K%=K:PROCCASA(2):IF K%<10 GOTO 21999
21440 GCOL 0,2:IF M>3 THEN K=3:M=M-3 ELSE K=M:M=-1
21450 K=K*32:M=M*32
21455 FOR I=0 TO K STEP 4:MOVE 704,672+I:DRAW 768,672+I:NEXT I
21456 IF M<0 GOTO 21999
21460 FOR I=0 TO M STEP 4:MOVE 704+I,768+I:DRAW 768-I,768+I:NEXT I
21999 COLOUR 2:COLOUR 129:ENDPROC
22000 DEF PROCCASA(T)
22070 FOR J=0 TO 15:L=FNL(K-J):IF LGTH%(J,T)<0 GOTO 22090
22080   FOR I=0TO LGTH%(J,T)*L STEP 4:MOVE FR%(J,T)+I*RAT(J,T),ST%(J,T)-I:DRAW T%(J,T)+I*RAT(J,T),ST%(J,T)-I:NEXT I
22085 GOTO 22100
22090   FOR I=0 TO (-LGTH%(J,T))*L STEP 4:MOVE ST%(J,T)-I,FR%(J,T)+I*RAT(J,T):DRAW ST%(J,T)-I,T%(J,T)+I*RAT(J,T):NEXT
22100   IF K%<=J THEN J=15
22110 NEXT J
22120 ENDPROC
22999 :
23000 DEF PROCMAP
23001 PROCOFF
23002 VDU 19,0,0,0,0,0,19,1,6,0,0,0,19,2,2,0,0,0,19,3,3,0,0,0,23,224,16,16,84,84,56,56,254,0,23,225,16,56,84,146,16,56,84,146,23,226,16,56,84,146,16,16,16,16
23010 M$=CHR$(224):T$=CHR$(225)+CHR$(8)+CHR$(10)+CHR$(226)
23080 GCOL 1,3:COLOUR 3:PROCDELAY(3)
23085 PRINT TAB(2,29);"Small"TAB(3,31);"Hill";
23086 MOVE 328,0:DRAW 310,32:DRAW 320,64:DRAW 476,128:DRAW 590,132:DRAW 780,48:DRAW 796,0:MOVE 550,20:DRAW 448,32:DRAW 432,48:DRAW 512,90:DRAW 610,86:DRAW 670,48:DRAW 670,32:DRAW 550,20
23090 PROCDELAY(3):PRINT TAB(0,21);"High"TAB(1,23);"Land"
23095 MOVE 100,380:DRAW 160,370:DRAW 220,384:DRAW 200,420:DRAW 144,432:DRAW 96,400:DRAW 100,380:MOVE 60,370:DRAW 72,310:DRAW 144,300:DRAW 288,320:DRAW 330,384:DRAW 300,440:DRAW 180,464:DRAW 70,430
23100 DRAW 60,370:MOVE 0,288:DRAW 60,264:DRAW 356,320:DRAW 464,352:DRAW 450,390:DRAW 320,484:DRAW 192,520:DRAW 120,510:DRAW 0,430:MOVE 0,200:DRAW 70,210:DRAW 390,260:DRAW 510,320:DRAW 530,360:DRAW 480,470:DRAW 384,550:DRAW 250,610:DRAW 130,610:DRAW 0,566
23190 PROCDELAY(3)
23200 COLOUR1:IF S1$="X"THEN PRINT TAB(2,19);"X" ELSE PRINT TAB(8,30);"Z"
23210 COLOUR3:IF S2$="W"THEN PRINT TAB(15,3);"W" ELSE PRINT TAB(16,29);"T"
23220 COLOUR2:IF S3$="Y"THEN PRINT TAB(10,14);"Y" ELSE PRINT TAB(7,2);"V"
23990 ENDPROC
23999 :
24000 DEF FNINPUT(X%,Y%,P$)
24010 PRINT TAB(X%,Y%);P$;
24020 LOCAL C$,K$:C$=""
24030 REPEAT
24040   K$=GET$
24050   IF K$=CHR$(127) OR K$=CHR$(8) THEN IF LEN(C$)>0 THEN C$=LEF T$(C$,LEN(C$)-1):PRINT CHR$(8);" ";CHR$(8);
24060   IF K$>=CHR$(32) AND K$<=CHR$(126) THEN C$=C$+K$:PRINT K$;
24070 UNTIL K$=CHR$(13)  REM CR seen
24080 =C$
24999 :
32000 *TAPE
32005 FOR I%=0 TO TOP-PAGE STEP 4:I%!&E00=I%!PAGE:NEXT
32010 ?&13=?&13-(PAGE-&E00)DIV 256:PAGE=&E00
32020 RUN

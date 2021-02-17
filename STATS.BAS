   KEY 6,"auto,5"+CHR$(13):KEY 1,"list "
   KEY 4,"SAVE"+CHR$(34)+"project"+CHR$(34)+CHR$(13)
   KEY 5,"list 29560-"+CHR$(13):KEY 9,"key on"+CHR$(13):KEY 10,"EDIT "
   '            ***********************************
   '            *                                 *
   '            *       STATISTICS PROJECT        *
   '            *                                 *
   '            *          MAIN PROGRAM           *
   '            *                                 *
    '            *        Bryan Duggan WMT 1       *
    '            *                                 *
    '            ***********************************
    '
    '
    '
    ' DIMention arrays
    '
    '
    DIM DAT(16)
    DIM SORTEDDATA(16)
    DIM CPI(8)
    DIM PRICE(8)
    DIM TEMP(16)
    '
    '
    ' Set Up Screen
    '
    '
    SCREEN 2
    KEY OFF
    CLS
    '
    '
    ' Install default values
    '
    '
    FOR T=1 TO 8
    READ X
    CPI(T)=X
    NEXT
    FOR T=1 TO 8
    READ X
    PRICE(T)=X
    NEXT
    FOR T=1 TO 16
    READ X
    DAT(T)=X
    NEXT
    DATA 100,106,112,117,120,122,127,115
    DATA 4200,3400,2900,1700,1300,900,770,700
    DATA 2,8,10,12,17,26,38,43,56,62,60,65,70,75,66,72
520 '
    '
    '
    ' *********    Main Menu   *********
    '
    '
    '
    GOSUB 6800:' music
    PRINT:PRINT
    PRINT"                               MAIN MENU"
    PRINT"                               ========="
    PRINT
    PRINT
    PRINT
    PRINT
    PRINT"                            1 : Input a file"
    PRINT
    PRINT
    PRINT
    PRINT"                            2 : Graph data"
    PRINT
    PRINT
    PRINT
    PRINT"                            3 : Analyse data"
    PRINT
    PRINT
    PRINT
    PRINT"                          ENTER => Exit program"
    '
    '   choice
    '
830 A$=INKEY$
    IF A$="" THEN GOTO 830
    IF A$=CHR$(13) THEN CLS:SYSTEM
    A=VAL(A$)
    ON A GOSUB 890,2240,5050
    GOTO 830
890 '
    '
    '          OPENING A FILE
    '
    '
    CLS
    GOSUB 6800
    PRINT"                             Inputting a file"
    PRINT"                             ================"
    PRINT
    PRINT
     PRINT
     PRINT
     PRINT"                        1 : Load a file from disk"
     PRINT
     PRINT
     PRINT
     PRINT"                        2 : Enter data from keyboard"
     PRINT
     PRINT
     PRINT
     PRINT"                        3 : Generate random data"
     PRINT
     PRINT
     PRINT
     PRINT"                        4 : Save data"
     PRINT
     PRINT
     PRINT
     PRINT"                       ENTER => Return to main menu"
     '
     ' selection
     '
1220 A$=INKEY$
     IF A$="" THEN 1220
     IF A$=CHR$(13) THEN GOSUB 2170
     A=VAL(A$)
     ON A GOSUB 1280,1510,1760,1950
     GOTO 1220
1280 '
     '
     ' input from disk
     '
     '
     '
     CLS
     GOSUB 6800
     PRINT"Please enter the file name,
     PRINT"or press RETURN to return to menu :"
     PRINT
     INPUT A$
     IF A$="" THEN RETURN 890
     '
     OPEN"e:"+A$ FOR INPUT AS 1
     FOR T=1 TO 16
     INPUT#1,DAT(T)
     NEXT T
     CLOSE #1
     CLS
     PRINT"Data successfully loaded :"
     GOSUB 6820
     RETURN 890
1510 '
     '
     '  Option 2 Enter data from keyboard
     '
     '
     CLS
     GOSUB 6800
     PRINT"Type in the values, pressing ENTER"
     PRINT"after each one :"
     PRINT
     YEAR=83
     FOR T=1 TO 16 STEP 2
1630 PRINT"Term 1 19";USING"##";YEAR;
     INPUT DAT(T)
     IF DAT(T)>100 OR DAT(T)<0 THEN PRINT:PRINT"DATA must be between 0 and 100.Please retry.":PRINT:GOTO 1630
1660 PRINT"     2 19";USING"##";YEAR;
     INPUT DAT(T+1)
     IF DAT(T+1)>100 OR DAT(T+1)<0 THEN PRINT:PRINT"DATA must be between 0 and 100.Please retry.":PRINT:GOTO 1660
     YEAR=YEAR+1
     PRINT
     NEXT T
     CLS
     PRINT"These are the new values :"
     GOSUB 6820
     RETURN 890
1760 '
     '
     ' Option 3 Generate random data
     '
     '
     CLS
     GOSUB 6800
     RANDOMIZE (TIMER)
     LOCATE 15,20
     PRINT" Press SPACE to begin,or ENTER to return"
1860 A$=INKEY$
     IF A$=" " THEN 1880 ELSE IF A$=CHR$(13) THEN RETURN 890 ELSE GOTO 1860
1880 FOR T=1 TO 16
     DAT(T)=INT(RND(1)*100)
     NEXT
     CLS
     PRINT"The new values are :"
     GOSUB 6820
     RETURN 890
1950 '
     '
     ' Option 4 Save data
     '
     '
     CLS
     GOSUB 6800
     PRINT"Please enter the file name,"
     PRINT"or press ENTER to return to menu :"
     PRINT
     INPUT A$
     IF A$="" THEN RETURN 890
     OPEN "e:"+A$ FOR OUTPUT AS 1
     FOR T=1 TO 16
     PRINT#1,DAT(T)
     NEXT
     CLOSE 1
     CLS
     '
     PRINT"Data successfully saved :
     GOSUB 6820
     RETURN 890
2170 '
     '
     ' Option 5  Return to menu
     '
     '
     CLS
     RETURN 520
2240 '
     '
     '          GRAPH PLOTTING
     '
     '
     CLS
     GOSUB 6800
     PRINT"                            Graph Plotting"
     PRINT"                            =============="
     PRINT
     PRINT
     PRINT
     PRINT
     PRINT
     PRINT"                           1 : Trend Diagram"
     PRINT
     PRINT
     PRINT
     PRINT"                           2 : Scatter Diagram"
     PRINT
     PRINT
     PRINT
     PRINT"                           3 : Bar Chart"
     PRINT
     PRINT
     PRINT
     PRINT"                     ENTER => Return to main menu"
     '
2520 A$=INKEY$
     IF A$="" THEN 2520
     IF A$=CHR$(13) THEN GOSUB 2170
     A=VAL(A$)
     ON A GOSUB 2580,3430,4220
     GOTO 2520
2580 '
     '
     ' Option 1 Trend Diagram
     '
     '
     CLS
     GOSUB 6800
     LINE (35,5 )-(35,190)
     LINE (20,170)-(290,170)
     '
     FOR X=35 TO 220 STEP 24
     PSET (X,171)
     LINE(X+12,171)-(X+12,173)
     NEXT
     PSET(227,171)
     '
     YEAR=83
     FOR X=6 TO 28 STEP 3
     LOCATE 23,X
     PRINT USING "##";YEAR
     YEAR=YEAR+1
     NEXT
     LOCATE 23,31
     PRINT"-  Year"
     '
     FOR Y=155 TO 5 STEP -16
     LINE(33,Y)-(35,Y)
     NEXT
     '
     PERCENT=10
     FOR Y=20 TO 2 STEP-2
     LOCATE Y,2
     PRINT USING"###";PERCENT
     PERCENT=PERCENT+10
     NEXT
     LOCATE 12,1:PRINT"%"
     '
     T=1:TEMPX=0:TEMPY=0
     FOR X=47 TO 232 STEP 12
     Y=(170-(DAT(T)*1.6))
     IF TEMPX<>0 AND TEMPY<>0 THEN LINE(TEMPX,TEMPY)-(X,Y),2
     CIRCLE(X,Y),1,8
     TEMPX=X:TEMPY=Y
     T=T+1
     NEXT
     '
     FOR T=1 TO 16
     TEMP(T)=T
     NEXT
     GOSUB 7110
     X=170-(((1-A)/B)*1.6)
     XX=170-(((16-A)/B)*1.6)
     LINE(46,X)-(226,XX)
     '
     LOCATE 1,14
     PRINT"TREND-DIAGRAM"
     LOCATE 9,37
     PRINT"PRESS"
     LOCATE 11,37
     PRINT"ENTER"
     LOCATE 15,37
     PRINT"RETURN"
     LOCATE 13,37
     PRINT"TO"
     LINE(280,60)-(341,125),,B
     '
     '
     LOCATE 3,55
     PRINT"Data used :"
     PRINT
     YEAR=83
     FOR T=1 TO 16 STEP 2
     LOCATE ,55
     PRINT"Term 1 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T)
     LOCATE ,55
     PRINT"     2 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T+1)
     YEAR=YEAR+1
     NEXT
     LINE(420,10)-(630,170),,B
     LOCATE 23,57
     PRINT "------ Best Fit Line"
3410 A$=INPUT$(1)
     IF A$=CHR$(13) THEN 2240 ELSE 3410
3430 '
     '
     ' Option 2   Scatter Diagram
     '
     '
     CLS
     GOSUB 6800
     LINE (35,5 )-(35,190)
     LINE (20,170)-(250,170)
     '
     FOR X=35 TO 220 STEP 24
     PSET (X,171)
     LINE(X+12,171)-(X+12,173)
     NEXT
     PSET(227,171)
     '
     FOR Y=155 TO 5 STEP -16
     LINE(33,Y)-(35,Y)
     NEXT
     '
     PERCENT=10
     FOR Y=20 TO 2 STEP-2
     LOCATE Y,2
     PRINT USING"###";PERCENT
     PERCENT=PERCENT+10
     NEXT
     LOCATE 12,1:PRINT"%"
     YEAR=83
     FOR X=6 TO 28 STEP 3
     LOCATE 23,X
     PRINT USING "##";YEAR
     YEAR=YEAR+1
     NEXT
     LOCATE 23,31
     PRINT"-  Year"
     '
     T=1:TEMPX=0:TEMPY=0
     FOR X=47 TO 232 STEP 12
     Y=(170-(DAT(T)*1.6))
     CIRCLE(X,Y),2
     PAINT(X,Y),8
     T=T+1
     NEXT
     '
     GOSUB 7110
     X=170-(((1-A)/B)*1.6)
     XX=170-(((16-A)/B)*1.6)
     LINE(46,X)-(226,XX)
     '
     LOCATE 1,14
     PRINT"SCATTER DIAGRAM"
     LOCATE 9,33
     PRINT"PRESS"
     LOCATE 11,33
     PRINT"ENTER"
     LOCATE 15,33
     PRINT"RETURN"
     LOCATE 13,33
     PRINT"TO"
     LINE(250,60)-(310,125),,B
     LOCATE 3,55
     PRINT"Data used :"
     PRINT
     YEAR=83
     FOR T=1 TO 16 STEP 2
     LOCATE ,55
     PRINT"Term 1 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T)
     LOCATE ,55
     PRINT"     2 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T+1)
     YEAR=YEAR+1
     NEXT
     LINE(420,10)-(630,170),,B
     LOCATE 23,57
     PRINT "------ Best Fit Line"
4190 A$=INKEY$
     IF A$<>CHR$(13) THEN 4190 ELSE 2240
     RETURN 2240
4220 '
     '
     ' Option 3   Bar Chart
     '
     '
     CLS
     GOSUB 6800
     LINE (35,5 )-(35,190)
     LINE (20,170)-(250,170)
     '
     FOR X=35 TO 220 STEP 24
     PSET (X,171)
     LINE(X+12,171)-(X+12,173)
     NEXT
     PSET(227,171)
     '
     FOR Y=155 TO 5 STEP -16
     LINE(33,Y)-(35,Y)
     NEXT
     '
     YEAR=83
     FOR X=6 TO 28 STEP 3
     LOCATE 23,X
     PRINT USING "##";YEAR
     YEAR=YEAR+1
     NEXT
     LOCATE 23,31
     PRINT"-  Year"
     '
     PERCENT=10
     FOR Y=20 TO 2 STEP-2
     LOCATE Y,2
     PRINT USING"###";PERCENT
     PERCENT=PERCENT+10
     NEXT
     LOCATE 12,1:PRINT"%"
     '
     T=1
     FOR X=47 TO 232 STEP 12
     Y=(170-(DAT(T)*1.6))
     LINE(X,Y)-(X+10,170),,BF
     T=T+1
     NEXT
     '
     SUM=0
     FOR T=1 TO 16
     SUM=SUM+DAT(T)
     NEXT
     MEAN=SUM/16
     Y=(170-(MEAN*1.6))
     LINE(45,Y)-(240,Y)
     '
     LOCATE 1,14
     PRINT"Bar Chart"
     LOCATE 9,37
     PRINT"PRESS"
     LOCATE 11,37
     PRINT"ENTER
     LOCATE 15,37
     PRINT"RETURN"
     LOCATE 13,37
     PRINT"TO"
     LINE(280,60)-(341,125),,B
     '
     '
     LOCATE 3,55
     PRINT"Data used :"
     PRINT
     YEAR=83
     FOR T=1 TO 16 STEP 2
     LOCATE ,55
     PRINT"Term 1 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T)
     LOCATE ,55
     PRINT"     2 ... 19";USING"##";YEAR;
     PRINT" ... ";DAT(T+1)
     YEAR=YEAR+1
     NEXT
     LOCATE 23,57
5010 PRINT "------ Mean Line"
     LINE(420,10)-(630,170),,B
     A$=INPUT$(1)
     IF A$=CHR$(13) THEN 2240 ELSE 5010
5050 '
     '
     '            ANALYSE DATA
     '
     CLS
     GOSUB 6770
     PRINT"Please wait,calculating..."
     ' mean
     SUM=0
     FOR T=1 TO 16
     SUM=SUM+DAT(T)
     NEXT
     MEAN=SUM/16
     ' mean increace/decreace
     SUM=0
     FOR T=1 TO 15
     SUM=SUM+(DAT(T+1)-DAT(T))
     NEXT
     INCMEAN=SUM/15
     ' range
     RMAX=0
     RMIN=100
     FOR T=1 TO 16
     IF DAT(T)>RMAX THEN RMAX=DAT(T)
     IF DAT(T)<RMIN THEN RMIN=DAT(T)
     NEXT
     '     Quartile mean
     FOR T=1 TO  16
     SORTEDDATA(T)=DAT(T)
     NEXT
     FOR PASS=1 TO 15
     MIN=PASS
     FOR I=PASS+1 TO 16
     IF SORTEDDATA(I)<SORTEDDATA(MIN) THEN MIN=I
     IF MIN<>PASS THEN SWAP SORTEDDATA(PASS),SORTEDDATA(MIN)
     NEXT I,PASS
     SUM=0
     FOR T=5 TO 12
     SUM=SUM+SORTEDDATA(T)
     NEXT
     QUART=SUM/8
     '
     '      mean deviation
     SUM=0
     FOR T=1 TO 16
     SUM=SUM+(DAT(16)-MEAN)
     NEXT
     MDEV=SUM/16
     '      corrilation 1: price and data
     X=.5
     FOR T=1 TO 16
     TEMP(T)=PRICE(X)
     X=X+.5
     NEXT
     GOSUB 7110
     PDCORR=P
     PDCORR$=TEMPCORR$
     '
     X=.5
     FOR T=1 TO 16
     TEMP(T)=CPI(X)
     X=X+.5
     NEXT
     GOSUB 7110
     CDCORR=P
     CDCORR$=TEMPCORR$
5710 CLS:GOSUB 6760
     PRINT"                       DISPLAY ANALYSIS OF RESULTS"
     PRINT"                       ==========================="
     PRINT
     PRINT
     PRINT
     PRINT"                             1 : Means"
     PRINT
     PRINT
     PRINT
     PRINT"                             2 : Corrollations"
     PRINT
     PRINT
     PRINT
     PRINT"                             3 : All data
     PRINT
     PRINT
     PRINT
     PRINT"                         ENTER => Return to main menu"
     '
5910 A$=INKEY$
     IF A$="" THEN 5910
     IF A$=CHR$(13) THEN GOSUB 2170
     A=VAL(A$)
     ON A GOSUB 5970,6280,6530
     GOTO 5910
5970 '
     CLS
     GOSUB 6770
     PRINT"                                 MEANS"
     PRINT"                                 ====="
     PRINT
     PRINT
     PRINT
     PRINT"                           Arithmetic Mean  :";MEAN;"%"
     PRINT
     PRINT
     PRINT"                       Mean Increase/Decrease :";INCMEAN;"%"
     PRINT
     PRINT
     PRINT"                            Range :";USING"##";RMIN;
     PRINT"% To ";USING"##";RMAX;:PRINT"%"
     PRINT
     PRINT
     PRINT"                           Quartile Mean :";QUART;"%"
     PRINT
     PRINT
     PRINT"                    Mean Deviation from Norm :";MDEV;"%"
     PRINT
     PRINT
     PRINT
     PRINT"                    >>>  Press ENTER to return  <<<"
     LINE(155,162)-(415,180),,B
6240 A$=INKEY$
     IF A$="" THEN 6240
     IF A$=CHR$(13) THEN RETURN 5710
     GOTO 6240
6280 '
     '      Option 2 : Output Corrollations"
     '
     CLS
     GOSUB 6770
     PRINT"                               CORRELATIONS"
     PRINT"                               ============"
     PRINT
     PRINT
     PRINT"                Formula for least squares line : y =";A;"+";B;"x"
     PRINT:PRINT
     PRINT"  There is a";PDCORR$" corrollation between Price increase/decrease in PC's "
     PRINT:PRINT
     PRINT"           and the increase/decrease in the % owned,(p =";PDCORR;")"
     PRINT:PRINT
     PRINT"           There is a";CDCORR$" corrollation between the annual C.P.I."
     PRINT:PRINT
     PRINT"           and the increase/decrease in the % owned,(p =";CDCORR;")"
     PRINT:PRINT:PRINT:PRINT
     PRINT"                    >>>  Press ENTER to return  <<<"
     LINE(155,162)-(415,180),,B
6490 A$=INKEY$
     IF A$="" THEN 6490
     IF A$=CHR$(13) THEN RETURN 5710
     GOTO 6490
6530 '
     '
     '           Option 3 : Output all data
     '
     '
     CLS
     GOSUB 6750
     PRINT"                                  DATA"
     PRINT"                                  ===="
     PRINT
     PRINT TAB(10);"Term";TAB(23);"Year";TAB(34);"% Access";TAB(50);"C.P.I";TAB(60);"Price"
     FOR T=1 TO 80:PRINT"-";:NEXT
     PRINT
     X=1
     YEAR=83
     FOR T=1 TO 16 STEP 2
     PRINT TAB(10);"1";TAB(24);YEAR;TAB(36);DAT(T);TAB(50);CPI(X);TAB(60);PRICE(X)
     PRINT TAB(10);"2";TAB(36);DAT(T+1)
     X=X+1:YEAR=YEAR+1
     NEXT
     LOCATE 23,49:PRINT" -> Press ENTER <-"
     GOTO 6240
6750 '
6760 '
6770 ' subroutine to play music
     '
     '
6800 PLAY "l12o2gc"
     RETURN
6820 '
     '
     ' subroutine to output data
     '
     '
     PRINT
     PRINT
     YEAR=83
     FOR T=1 TO 16 STEP 2
     '
     PRINT"                             Term 1 19";USING"##";YEAR;
     PRINT"   ";DAT(T)
     PRINT"                                  2 19";USING"##";YEAR;
     PRINT"   ";DAT(T+1)
     YEAR=YEAR+1
     NEXT
     PRINT
     PRINT
     PRINT
     PRINT"Press ENTER to return..."
     A$=INKEY$
     IF A$=CHR$(13) THEN RETURN ELSE 7020
     '
     DIM DAT(16)
     OPEN "e:data" FOR OUTPUT AS 1
     FOR T=1 TO 16
     DAT(T)=INT(RND(1)*100)
     PRINT#1,DAT(T)
     NEXT
7110 '
     '
     ' subroutine to calculate a,b
     ' and Pearson's corrilation.
     '
     '
     ' sums
     SUMX=0
     SUMY=0
     SUMX2=0
     SUMY2=0
     SUMXY=0
     '
     FOR T=1 TO 16
     SUMX=SUMX+DAT(T)
     SUMY=SUMY+TEMP(T)
     SUMX2=SUMX2+(DAT(T)^2)
     SUMY2=SUMY2+(TEMP(T)^2)
     SUMXY=SUMXY+(DAT(T)*TEMP(T))
     NEXT T
     N=16
     '     Calculate B
     B=((N*SUMXY)-(SUMX*SUMY))/((N*SUMX2)-SUMX^2)
     '     Calculate A
     A=(SUMY/N)-((B*SUMX)/N)
     '     Calculate Pearson's
     PTOP=(N*SUMXY)-(SUMX*SUMY)
     PBOT=(((N*SUMX2)-SUMX^2)*((N*SUMY2)-SUMY^2))^.5
     P=PTOP/PBOT
     IF P<0 THEN PPOS=-P ELSE PPOS=P
     IF PPOS>=0 AND PPOS<.25 THEN TEMPCORR$=" negligible"
     IF PPOS>=.25 AND PPOS<.5 THEN TEMPCORR$=" poor"
     IF PPOS>=.5 AND PPOS<.75 THEN TEMPCORR$=" good"
     IF PPOS>=.75 AND PPOS<=1 THEN TEMPCORR$="n excelent"
     '
     RETURN

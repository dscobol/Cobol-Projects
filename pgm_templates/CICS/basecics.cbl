       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    BASECICS.                                                 
      *REMARKS        THIS PROGRAM WILL DEFINE A BASE STRUCTURE                 
      *               FOR CICS PROGRAMS.                                        

       DATA DIVISION.                                                           

       WORKING-STORAGE SECTION.                                                 

       01  WS-MAP-VALUES.                                                       
           12 WS-TRANS-ID              PIC X(04) VALUE '$001'.                  
           12 WS-MAPSET-NAME           PIC X(08) VALUE '$001M   '.              
           12 WS-MAP-NAME              PIC X(08) VALUE 'CALCMAP '.              

       01  WS-CICS-PGM-STORAGE-AREA.                                            
           12 WS-RESPONSE-CODE         PIC S9(8) BINARY.                        
           12 WS-COMMUNICATION-AREA    PIC X(01).                               
           12 WS-END-MESSAGE           PIC X(60) VALUE                          
            'QUITTING AS REQUESTED - USE CESF LOGOFF TO END CICS'.              

       01  WS-STATUS-FLAGS.                                                     
           12 WS-DATA-GOOD-BAD-FLAG    PIC X(01) VALUE ' '.                     
              88 WS-DATA-GOOD                    VALUE 'G'.                     
              88 WS-DATA-BAD                     VALUE 'B'.                     
           12 WS-FIRST-TIME-FLAG       PIC S9(4) BINARY.                        
              88 WS-FIRST-TIME                   VALUE '0'.                     

       01  WS-ERROR-MESSAGES.                                                   
           12 PIC X(35) VALUE '** CICS ERROR -- CONTACT HELP DESK'.             
           12 PIC X(35) VALUE '-- REPORT THIS INFORMATION **     '.             
           12 PIC X(09) VALUE SPACES.
           12 WS-ERROR-LINE-1.                                                  
              15             PIC X(14) VALUE '   EIBTRNID = '.                  
              15 WS-EIBTRNID PIC X(4).                                          
              15             PIC X(61) VALUE SPACES.                            

           12 WS-ERROR-LINE-2.                                                  
              15             PIC X(14) VALUE '   EIBRSRCE = '.                  
              15 WS-EIBRSRCE PIC X(8).                                          
              15             PIC X(57) VALUE SPACES.                            

           12 WS-ERROR-LINE-3.                                                  
              15             PIC X(14) VALUE '   EIBRESP  = '.                  
              15 WS-EIBRESP  PIC ZZZZZZZ9.                                      
              15             PIC X(57) VALUE SPACES.                            

           12 WS-ERROR-LINE-4.                                                  
              15             PIC X(14) VALUE '   EIBRESP2 = '.                  
              15 WS-EIBRESP2 PIC ZZZZZZZ9.                                      
              15             PIC X(57) VALUE SPACES.                            
           12                PIC X(79) VALUE ALL '*'.                           

      *COPY MANUALLY CODED MAP HERE.                                            

      *COPY DFHEIBLK COPY BOOK HERE.                                            

      *COPY ATTRIBUTES COPY BOOK HERE.                                          

       LINKAGE SECTION.                                                         

       01  DFHCOMMAREA                 PIC X.                                   

       PROCEDURE DIVISION.                                                      

       0000-MAINLINE.                                                           

           PERFORM 3333-ALWAYS-TEST.

           EVALUATE TRUE
              WHEN FIRST-TIME
                 PERFORM 9000-FIRST-TIME                                        
              WHEN PF3-KEY                                                      
                 PERFORM 9100-CLEAR-THE-SCREEN                                  
              WHEN ENTER-KEY
                 PERFORM 0500-NORMAL-PROCEDURE
              WHEN OTHER     
                 PERFORM 9200-INVALID-KEY-MESSAGE                               
           END-EVALUATE.                                                        

           PERFORM 8000-RETURN-WITH-TRANS-ID.                                   

       0500-NORMAL-PROCESSING.                                                  

           PERFORM 1000-RECIEVE-MAP.                                            

           SET WS-DATA-GOOD TO TRUE.                                            
           PERFORM 5000-VALIDATE-FIELDS.                                        

           IF WS-DATA-GOOD
              PERFORM 5500-COMPUTE-RESULTS                                      
           END-IF.

           PERFORM 1200-SEND-MAP-DATAONLY.

       1000-RECIEVE-MAP.                                                        

           EXEC CICS                                                            
              RECIEVE MAP     (WS-MAP-NAME)                                     
                      MAPSET  (WS-MAPSET-NAME)                                  
                      INTO    (**MAN-MAP**)                                     
                      RESP    (WS-RESPONSE-CODE)                                
           END-EXEC.                                                            

           EVALUATE WS-RESPONSE-CODE                                            
              WHEN DFHRESP(NORMAL)                                              
                 CONTINUE                                                       
              WHEN DFHRESP(MAPFAIL)                                             
      * INSERT FIRST 'L' FIELD BELOW                                            
                 MOVE -1 TO                                                     
                 MOVE 'NO DATA ENTERED; ENTER DATA OR <CLEAR> TO QUIT'          
                    TO MSM-D-MESSAGE                                            
      * INSERT NAME OF MESSAGE NAME ABOVE                                       
                 PERFORM 1200-SEND-MAP-DATAONLY                                 
                 PERFORM 8000-RETURN-WITH-TRANS-ID                              
              WHEN OTHER                                                        
                 PERFORM 9999-ABORT                                             
           END-EVALUATE.                                                        
      
       1200-SEND-MAP-DATAONLY.                                                  

           EXEC CICS
              SEND MAP        (WS-MAP-NAME)                                     
                   MAPSET     (WS-MAPSET-NAME)                                  
                   FROM       (SYMBOLIC-MAP)                                    
                   DATAONLY
                   CURSOR
           END-EXEC.

       3333-ALWAYS-TEST.                                                        

           MOVE EIBAID TO EIBAID-TEST-FIELD.                                    
           IF CLEAR-KEY                                                         
              PERFORM 8900-QUIT
           END-IF.                                                              
           MOVE EIBCALEN TO WS-FIRST-TIME-FLAG.                                 

       5000-VALIDATE-FIELD.                                                     

      * DON'T FORGET, VALIDATE BOTTOM RIGHT TO TOP LEFT                         

           MOVE AV-UNPROT-NORM-MDT TO WCM-A-FIRST-NAME                          
                                      WCM-A-LAST-NAME.                          
           MOVE AV-UNPROT-NUM-MDT  TO WCM-A-HOURS-WORKED                        
                                      WCM-A-HOURLY-WAGE.                        

           IF WCM-L-HOURLY-WAGE = ZERO                                          
              MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                       
              SET WS-DATA-BAD TO TRUE
              MOVE -1 TO WCM-L-HOURLY-WAGE                                      
              MOVE 'YOU MUST ENTER THE HOURLY WAGE' TO                          
                 WCM-D-ERROR-MESSAGE                                            
           ELSE                                                                 
              IF WCM-L-HOURLY-WAGE NOT NUMERIC                                  
                 MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                    
                 SET WS-DATA-BAD TO TRUE
                 MOVE -1 TO WCM-L-HOURLY-WAGE                                   
                 MOVE 'HOURLY WAGE MUST BE NUMERIC' TO                          
                    WCM-D-ERROR-MESSAGE                                         
              ELSE                                                              
                 IF WCM-L-HOURLY-WAGE NOT > ZERO                                
                    MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                 
                    SET WS-DATA-BAD TO TRUE
                    MOVE -1 TO WCM-L-HOURLY-WAGE                                
                    MOVE 'HOURLY WAGE MUST BE POSITIVE' TO                      
                       WCM-D-ERROR-MESSAGE                                      
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              

           IF WCM-L-HOURS-WORKED = ZERO
              MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
              SET WS-DATA-BAD TO TRUE
              MOVE -1 TO WCM-L-HOURS-WORKED
              MOVE 'YOU MUST ENTER THE AMOUNT OF HOURS WORKED' TO               
                 WCM-D-ERROR-MESSAGE                                            
           ELSE                                                                 
              IF WCM-L-HOURS-WORKED NOT NUMERIC
                 MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
                 SET WS-DATA-BAD TO TRUE
                 MOVE -1 TO WCM-L-HOURS-WORKED
                 MOVE 'NUMBER OF HOURS MUST BE NUMERIC' TO                      
                    WCM-D-ERROR-MESSAGE                                         
              ELSE                                                              
                 IF WCM-L-HOURS-WORKED NOT > ZERO
                    MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
                    SET WS-DATA-BAD TO TRUE
                    MOVE -1 TO WCM-L-HOURS-WORKED
                    MOVE 'NUMBER OF HOURS WORKED MUST BE POSITIVE' TO           
                       WCM-D-ERROR-MESSAGE                                      
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              

           IF WCM-D-LAST-NAME = SPACES                                          
              MOVE AV-UNPROT-BRT TO WCM-A-LAST-NAME                             
              SET WS-DATA-BAD TO TRUE
              MOVE -1 TO WCM-L-LAST-NAME                                        
              MOVE 'YOU MUST ENTER A LAST NAME' TO                              
                 WCM-D-ERROR-MESSAGE                                            
           END-IF.                                                              

           IF WCM-D-FIRST-NAME = SPACES
              MOVE AV-UNPROT-BRT TO WCM-A-FIRST-NAME
              SET WS-DATA-BAD TO TRUE
              MOVE -1 TO WCM-L-FIRST-NAME
              MOVE 'YOU MUST ENTER A FIRST NAME' TO                             
                 WCM-D-ERROR-MESSAGE                                            
           END-IF.                                                              

       5500-COMPUTE-RESULTS.                                                    

           IF WCM-D-HOURS-WORKED IS LESS THAN 40 OR                             
              WCM-D-HOURS-WORKED = 40                                           
              COMPUTE WCM-S-BASE-PAY =                                          
                 WCM-D-HOURS-WORKED * WCM-D-HOURLY-WAGE                         
           ELSE                                                                 
              IF WCM-D-HOURS-WORKED > 40                                        
                 COMPUTE WCM-S-BASE-PAY =                                       
                    40 * WCM-D-HOURLY-WAGE                                      
                 COMPUTE WCM-S-OVERTIME =                                       
                    (1.5 * (WCM-D-HOURS-WORKED - 40))                           
                       * WCM-D-HOURLY-WAGE                                      
              END-IF                                                            
           END-IF.                                                              

           MOVE WCM-S-BASE-PAY TO WCM-D-BASE-PAY.                               
           MOVE WCM-S-OVERTIME TO WCM-D-OVERTIME.                               
                                                                                
           COMPUTE WCM-D-GROSS-PAY =                                            
              WCM-S-BASE-PAY + WCM-S-OVERTIME                                   
              MOVE 'WAGES COMPUTED, ENTER MORE DATA OR <CLEAR> TO QUIT'         
              ON SIZE ERROR                                                     
                 MOVE ZERO TO WCM-D-GROSS-PAY                                   
                 MOVE 'CANNOT DISPLAY GROSS-PAY, NUMBER TOO LARGE'              
           END-COMPUTE.                                                         

       8000-RETURN-WITH-TRANS-ID.                                               

           EXEC CICS
              RETURN TRANSID   (WS-TRANS-ID)                                    
                     COMMAREA  (WS-COMMUNICATION-AREA)                          
           END-EXEC.

       8900-QUIT.                                                               

           EXEC CICS
              SEND TEXT FROM   (WS-END-MESSAGE)                                 
                   ERASE
                   FREEKB
           END-EXEC.
           EXEC CICS
              RETURN
           END-EXEC.

       9000-FIRST-TIME.                                                         

      * INSERT NAME OF SYMBOLIC MAP BELOW                                       
           MOVE LOW-VALUE TO                                                    
      * INSERT FIRST 'L' FIELD BELOW                                            
           MOVE -1 TO                                                           
      * INSERT NAME OF MESSAGE NAME BELOW                                       
           MOVE 'ENTER VALUES, PRESS <ENTER> OR <CLEAR> TO QUIT'                
              TO                                                                
           EXEC CICS
              SEND MAP         (WS-MAP-NAME)                                    
                   MAPSET      (WS-MAPSET-NAME)                                 
                   FROM        (SYMBOLIC-MAP-NAME)                              
                   ERASE
                   CURSOR
           END-EXEC.                                                            

       9100-CLEAN-THE-SCREEN.                                                   

      * INSERT NAME OF SYMBOLIC MAP BELOW                                       
           MOVE LOW-VALUE TO                                                    
      * INSERT FIRST 'L' FIELD BELOW                                            
           MOVE -1 TO                                                           
      * INSERT NAME OF MESSAGE NAME BELOW                                       
           MOVE 'ENTER VALUES, PRESS <ENTER> OR <CLEAR> TO QUIT'                
              TO                                                                
      * YOU MIGHT HAVE TO DO SOME CLEAN UP OF COMPUTED FIELD.                   
      *    MOVE SPACES TO CSM-D-TOTAL-BEFORE-TAX-X                              
      *                   CSM-D-TAX-X.                                          
      *                   CSM-D-GRAND-TOTAL-X.                                  

           EXEC CICS
              SEND MAP         (WS-MAP-NAME)                                    
                   MAPSET      (WS-MAPSET-NAME)                                 
                   FROM        (SYMBOLIC-MAP-NAME)                              
                   DATAONLY
                   ERASEAUP
                   CURSOR
           END-EXEC.                                                            

       9200-INVALID-KEY-MESSAGE.                                                

      * INSERT NAME OF SYMBOLIC MAP BELOW                                       
           MOVE LOW-VALUE TO                                                    
      * INSERT FIRST 'L' FIELD BELOW                                            
           MOVE -1 TO                                                           
      * INSERT NAME OF MESSAGE NAME BELOW                                       
           MOVE 'YOU PRESSED A WRONG KEY, PRESS <ENTER>,<CLEAR> TO QUIT'        
              TO                                                                
      * YOU MIGHT HAVE TO DO SOME CLEAN UP OF COMPUTED FIELD.                   
      *    MOVE SPACES TO CSM-D-TOTAL-BEFORE-TAX-X                              
      *                   CSM-D-TAX-X.                                          
      *                   CSM-D-GRAND-TOTAL-X.                                  

           EXEC CICS
              SEND MAP         (WS-MAP-NAME)                                    
                   MAPSET      (WS-MAPSET-NAME)                                 
                   FROM        (SYMBOLIC-MAP-NAME)                              
                   DATAONLY
                   ERASEAUP
                   CURSOR
           END-EXEC.                                                            

       9999-ABORT.                                                              

           MOVE EIBTRNID    TO WS-EIBTRNID.                                     
           MOVE EIBRSRCE    TO WS-EIBRSRCE.
           MOVE EIBRESP     TO WS-EIBRESP.                                      
           MOVE EIBRESP2    TO WS-EIBRESP2.                                     

           EXEC CICS
              SEND TEXT FROM(WS-ERROR-MESSAGES)                                 
                   ERASE
                   ALARM
                   FREEKB
           END-EXEC.

           EXEC CICS
              RETURN
           END-EXEC.


       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    CALCTEST.                                                 
      *REMARKS        THIS PROGRAM CALCULATES BASE PAY, OVERTIME,               
      *               AND GROSS PAY BASED ON HOURS WORKED AND                   
      *               HOURLY WAGE ENTERED BY THE OPERATOR.                      

       ENVIRONMENT DIVISION.                                                    

       DATA DIVISION.                                                           

       WORKING-STORAGE SECTION.                                                 

       01  WS-STORAGE-VALUES.                                                   
           12 WCM-S-BASE-PAY           PIC S9(4)V99   VALUE +0.                 
           12 WCM-S-OVERTIME           PIC S9(4)V99   VALUE +0.                 

       01  WS-END-MESSAGE              PIC X(60)      VALUE                     
            'QUITTING AS REQUESTED - USE CESF LOGOFF TO END CICS'.              

       01  WS-GOOD-BAD-FLAG            PIC X(01)      VALUE ' '.                
           88 WS-GOOD                                 VALUE 'G'.                
           88 WS-BAD                                  VALUE 'B'.                

       01  COMMUNICATION-AREA          PIC X.                                   

       COPY WAGEMAP.                                                            

       COPY DFHAID.                                                             

       COPY AVDEFN.                                                             

       COPY CICSERC.                                                            

       LINKAGE SECTION.                                                         

       01  DFHCOMMAREA                 PIC X.                                   

       PROCEDURE DIVISION.                                                      

           IF EIBCALEN = 0                                                      
              MOVE LOW-VALUE TO WAGES-CALCULATION-MAP                           
              MOVE 'ENTER VALUES, PRESS<ENTER>, <CLEAR> TO QUIT'                
                 TO WCM-D-OPERATOR-MESSAGE                                      
              MOVE -1 TO WCM-L-FIRST-NAME                                       
              EXEC CICS                                                         
                 SEND MAP('W192')                                               
                      MAPSET(W192M')                                            
                      FROM(WAGES-CALCULATION-MAP)                               
                      ERASE                                                     
                      CURSOR                                                    
              END-EXEC                                                          
              EXEC CICS                                                         
                 RETURN TRANSID('W192')                                         
                        COMMAREA(COMMUNICATION-AREA)                            
                        LENGTH(1)                                               
              END-EXEC                                                          
           END-IF.                                                              

           EXEC CICS                                                            
              RECIEVE MAP('W192')                                               
                      MAPSET('W192M')                                           
                      INTO(WAGES-CALCULATION-MAP)                               
                      NOHANDLE                                                  
           END-EXEC.                                                            

           IF EIBAID = DFHCLEAR                                                 
              EXEC CICS                                                         
                 SEND TEXT FROM(WS-END-MESSAGE)                                 
                      LENGTH(60)                                                
                      ERASE                                                     
                      FREEKB                                                    
              END-EXEC                                                          
              EXEC CICS                                                         
                 RETURN                                                         
              END-EXEC                                                          
           END-IF.                                                              

           MOVE EIBRCODE TO CICS-HEX-TO-DECIMAL.                                
           IF CICS-MAPFAIL                                                      
              MOVE -1 TO WCM-L-FIRST-NAME                                       
              MOVE 'NO DATA ENTERED; ENTER DATA OR <CLEAR> TO QUIT' TO          
                 WCM-D-ERROR-MESSAGE                                            
              EXEC CICS                                                         
                 SEND MAP('W192')                                               
                      MAPSET('W192M')                                           
                      FROM(WAGES-CALCULATION-MAP)                               
                      DATAONLY                                                  
                      CURSOR                                                    
              END-EXEC                                                          
              EXEC CICS                                                         
                 RETURN TRANSID('W192')                                         
                        COMMAREA(COMMUNICATION-AREA)                            
                        LENGTH(1)                                               
              END-EXEC                                                          
           END-IF.                                                              
                                                                                
           MOVE AV-UNPROT-NORM-MDT TO WCM-A-FIRST-NAME                          
                                      WCM-A-LAST-NAME.                          
           MOVE AV-UNPROT-NUM-MDT  TO WCM-A-HOURS-WORKED                        
                                      WCM-A-HOURLY-WAGE.                        

           IF WCM-L-HOURLY-WAGE = ZERO                                          
              MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                       
              SET WS-BAD TO TRUE                                                
              MOVE -1 TO WCM-L-HOURLY-WAGE                                      
              MOVE 'YOU MUST ENTER THE HOURLY WAGE' TO                          
                 WCM-D-ERROR-MESSAGE                                            
           ELSE                                                                 
              IF WCM-L-HOURLY-WAGE NOT NUMERIC                                  
                 MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                    
                 SET WS-BAD TO TRUE                                             
                 MOVE -1 TO WCM-L-HOURLY-WAGE                                   
                 MOVE 'HOURLY WAGE MUST BE NUMERIC' TO                          
                    WCM-D-ERROR-MESSAGE                                         
              ELSE                                                              
                 IF WCM-L-HOURLY-WAGE NOT > ZERO                                
                    MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURLY-WAGE                 
                    SET WS-BAD TO TRUE                                          
                    MOVE -1 TO WCM-L-HOURLY-WAGE                                
                    MOVE 'HOURLY WAGE MUST BE POSITIVE' TO                      
                       WCM-D-ERROR-MESSAGE                                      
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              

           IF WCM-L-HOURS-WORKED = ZERO
              MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
              SET WS-BAD TO TRUE                                                
              MOVE -1 TO WCM-L-HOURS-WORKED
              MOVE 'YOU MUST ENTER THE AMOUNT OF HOURS WORKED' TO               
                 WCM-D-ERROR-MESSAGE                                            
           ELSE                                                                 
              IF WCM-L-HOURS-WORKED NOT NUMERIC
                 MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
                 SET WS-BAD TO TRUE                                             
                 MOVE -1 TO WCM-L-HOURS-WORKED
                 MOVE 'NUMBER OF HOURS MUST BE NUMERIC' TO                      
                    WCM-D-ERROR-MESSAGE                                         
              ELSE                                                              
                 IF WCM-L-HOURS-WORKED NOT > ZERO
                    MOVE AV-UNPROT-NUM-BRT TO WCM-A-HOURS-WORKED
                    SET WS-BAD TO TRUE                                          
                    MOVE -1 TO WCM-L-HOURS-WORKED
                    MOVE 'NUMBER OF HOURS WORKED MUST BE POSITIVE' TO           
                       WCM-D-ERROR-MESSAGE                                      
                 END-IF                                                         
              END-IF                                                            
           END-IF.                                                              

           IF WCM-D-LAST-NAME = SPACES                                          
              MOVE AV-UNPROT-BRT TO WCM-A-LAST-NAME                             
              SET WS-BAD TO TRUE                                                
              MOVE -1 TO WCM-L-LAST-NAME                                        
              MOVE 'YOU MUST ENTER A LAST NAME' TO                              
                 WCM-D-ERROR-MESSAGE                                            
           END-IF.                                                              

           IF WCM-D-FIRST-NAME = SPACES
              MOVE AV-UNPROT-BRT TO WCM-A-FIRST-NAME
              SET WS-BAD TO TRUE                                                
              MOVE -1 TO WCM-L-FIRST-NAME
              MOVE 'YOU MUST ENTER A FIRST NAME' TO                             
                 WCM-D-ERROR-MESSAGE                                            
           END-IF.                                                              

           IF WS-BAD                                                            
              MOVE 'HIGHLIGHT FIELDS IN ERROR, CORRECT, PRESS <ENTER>'          
                 TO WCM-D-ERROR-MESSAGE                                         
              EXEC CICS                                                         
                 SEND MAP('W192')                                               
                      MAPSET('W192M')                                           
                      FROM WAGES-CALCULATION-MAP                                
                      DATAONLY                                                  
                      CURSOR                                                    
              END-EXEC                                                          
              EXEC CICS                                                         
                 RETURN TRANID('W192')                                          
                        COMMAREA(COMMUNICATION-AREA)                            
                        LENGTH(1)                                               
              END-EXEC                                                          
           END-IF.                                                              

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

           EXEC CICS                                                            
              SEND MAP('W192')                                                  
                   MAPSET('W192M')                                              
                   FROM(WAGES-CALCULATION-MAP)                                  
                   DATAONLY                                                     
                   CURSOR                                                       
           END-EXEC.                                                            

           EXEC CICS                                                            
              RETURN TRANSID('W192')                                            
                     COMMAREA(COMMUNICATION-AREA)                               
                     LENGTH(1)                                                  
           END-EXEC.                                                            




















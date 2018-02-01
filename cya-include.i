/********** Temp-Table Definitions **********/
DEFINE TEMP-TABLE tt-cards
    FIELD tt_cards_uid AS INTEGER
    FIELD card-title   AS CHARACTER
    FIELD card-value   AS INTEGER
    FIELD discarded    AS LOGICAL
    FIELD in-hand      AS LOGICAL
    FIELD in-stack     AS LOGICAL
    INDEX idx1 IS PRIMARY UNIQUE tt_cards_uid.
    
DEFINE TEMP-TABLE tt-Shuffled-Deck LIKE tt-cards
    FIELD shuffle_order AS INTEGER
    INDEX idx2 IS PRIMARY UNIQUE shuffle_order.

DEFINE TEMP-TABLE tt-player
    FIELD player-name AS CHARACTER FORMAT "X(20)"
    FIELD player-id   AS INTEGER  
    FIELD player-hand AS CHARACTER EXTENT 4
    INDEX idx1 IS PRIMARY UNIQUE player-name.

DEFINE TEMP-TABLE tt-stack
    FIELD player-id      AS CHARACTER
    FIELD stack-num      AS INTEGER
    FIELD stack-contents AS CHARACTER
    INDEX idx1 IS PRIMARY UNIQUE player-id stack-num.

/********** Variable Definitions **********/    
DEFINE VARIABLE itx          AS INTEGER   NO-UNDO.
DEFINE VARIABLE jtx          AS INTEGER   NO-UNDO.
DEFINE VARIABLE chFivers     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chTenners    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chFifteeners AS CHARACTER NO-UNDO.
DEFINE VARIABLE chHome       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chSilver     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chGold       AS CHARACTER NO-UNDO.

DEFINE VARIABLE inUID        AS INTEGER   NO-UNDO.

DEFINE VARIABLE inNumPlayers AS INTEGER NO-UNDO.

/********** Variable Initialize **********/
ASSIGN chFivers     = "Piggy Bank,Baseball Cards,Stamp Collection,Cash Under the Mattress"
       chTenners    = "Bank Account,Coin Collection,Stocks"
       chFifteeners = "Classic Auto,Jewels"
       chHome       = "Home"
       chSilver     = "Silver"
       chGold       = "Gold"
       inUID        = 1.

/********** Internal Procedures **********/
PROCEDURE p-CreateCards:
    DEFINE INPUT PARAMETER ipinValue AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipchTitle AS CHARACTER NO-UNDO.
    
    IF CAN-DO(ipchTitle,"Piggy Bank") OR
       CAN-DO(ipchTitle,"Bank Account") OR
       CAN-DO(ipchTitle,"Classic Auto") THEN
    DO itx = 1 TO 10:
        DO jtx = 1 TO NUM-ENTRIES(ipchTitle):
            CREATE tt-cards.
            ASSIGN tt-cards.tt_cards_uid = inUID 
                   tt-cards.card-title   = ENTRY(jtx,ipchTitle) 
                   tt-cards.card-value   = ipinValue 
                   inUID                 = inUID + 1. 
        END.
    END.
    
    ELSE IF CAN-DO(ipchTitle,"Home") OR
            CAN-DO(ipchTitle,"Silver") THEN
    DO itx = 1 TO 8:
        DO jtx = 1 TO NUM-ENTRIES(ipchTitle): 
            CREATE tt-cards. 
            ASSIGN tt-cards.tt_cards_uid = inUID 
                   tt-cards.card-title   = ENTRY(jtx,ipchTitle) 
                   tt-cards.card-value   = ipinValue 
                   inUID                 = inUID + 1. 
        END.
    END.
    ELSE
    DO itx = 1 TO 4:
        DO jtx = 1 TO NUM-ENTRIES(ipchTitle): 
            CREATE tt-cards. 
            ASSIGN tt-cards.tt_cards_uid = inUID 
                   tt-cards.card-title   = ENTRY(jtx,ipchTitle) 
                   tt-cards.card-value   = ipinValue 
                   inUID                 = inUID + 1. 
        END. 
    END.
    
END PROCEDURE.

PROCEDURE p-ShuffleDeck:
    DEFINE VARIABLE iax         AS INTEGER NO-UNDO.
    DEFINE VARIABLE ibx         AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRandom     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iShuffleUID AS INTEGER NO-UNDO.
    
    iShuffleUID = 1.
    
    DO WHILE ibx < 110:
        DO iax = 1 TO 500:
            ASSIGN iRandom = RANDOM(1,110)
                   ibx     = 0.
            IF NOT CAN-FIND(tt-Shuffled-Deck WHERE
                            tt-Shuffled-Deck.tt_cards_uid = iRandom) THEN
            DO:
                FIND tt-cards WHERE tt-cards.tt_cards_uid = iRandom NO-LOCK.
                CREATE tt-Shuffled-Deck.
                BUFFER-COPY tt-cards TO tt-Shuffled-Deck.
                ASSIGN tt-Shuffled-Deck.shuffle_order = iShuffleUID
                       iShuffleUID                    = iShuffleUID + 1.
            END.         
            DOWN.           
        END.
        FOR EACH tt-shuffled-Deck NO-LOCK:
            ASSIGN ibx = ibx + 1.
        END.
    END.
END PROCEDURE.    

PROCEDURE p-DealHand:
    DEFINE VARIABLE icx AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO icx = 1 TO 4:
        DO idx = 1 TO inNumPlayers:
        
            FIND tt-player WHERE tt-player.player-id = idx NO-ERROR.
            IF NOT AVAILABLE tt-player THEN
            DO:
                MESSAGE "Player not found!" VIEW-AS ALERT-BOX.
                QUIT.
            END.

            FIND FIRST tt-shuffled-deck WHERE tt-shuffled-deck.in-hand = FALSE NO-ERROR.
            IF NOT AVAIL tt-shuffled-deck THEN
            DO:
                MESSAGE "No more cards to be dealt!" VIEW-AS ALERT-BOX.
                QUIT.
            END.
            
            ASSIGN tt-player.player-hand[icx] = tt-shuffled-deck.card-title
                   tt-shuffled-deck.in-hand        = TRUE.        
            RELEASE tt-shuffled-deck.   
                        
        END.        
    END.

END PROCEDURE.

PROCEDURE p-DiscardAndDraw:
    DEFINE INPUT PARAMETER ipchPlayer AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipinCard   AS INTEGER   NO-UNDO.
    
    FIND tt-player WHERE tt-player.player-name = ipchPlayer.
    IF NOT AVAILABLE tt-player THEN DO:
        MESSAGE SUBSTITUTE("Player &1 not found!",ipchPlayer) VIEW-AS ALERT-BOX.
        RETURN.
    END.  
    
    FIND FIRST tt-shuffled-deck
         WHERE tt-shuffled-deck.card-title = tt-player.player-hand[ipinCard]
           AND tt-shuffled-deck.in-hand    = TRUE
           AND tt-shuffled-deck.discarded  = FALSE
           AND tt-shuffled-deck.in-stack   = FALSE.
           
    ASSIGN tt-shuffled-deck.in-hand   = FALSE
           tt-shuffled-deck.discarded = TRUE
           tt-player.player-hand[ipinCard] = "".
           
    RELEASE tt-shuffled-deck.           
           
    FIND FIRST tt-shuffled-deck 
         WHERE tt-shuffled-deck.in-hand   = FALSE 
           AND tt-shuffled-deck.discarded = FALSE NO-ERROR.
    IF NOT AVAIL tt-shuffled-deck THEN
    DO:
        MESSAGE "No more cards to be dealt!" VIEW-AS ALERT-BOX.
        RETURN.
    END.
    ELSE ASSIGN tt-player.player-hand[ipinCard] = tt-shuffled-deck.card-title
                tt-shuffled-deck.in-hand        = TRUE.                               
          
END PROCEDURE.  

PROCEDURE p-PickupAndLayDown:
    DEFINE INPUT PARAMETER ipchPlayer AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE icx AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE loMatch AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bu-tt-stack FOR tt-stack.
    
    FIND LAST tt-shuffled-deck
        WHERE tt-shuffled-deck.discarded = TRUE
          AND tt-shuffled-deck.in-hand   = FALSE
          AND tt-shuffled-deck.in-stack  = FALSE.
           
    IF NOT AVAILABLE tt-shuffled-deck THEN
    DO:
        MESSAGE "No cards in the discard pile!" VIEW-AS ALERT-BOX.
        RETURN.
    END.                   
           
    FIND tt-player WHERE tt-player.player-name = ipchPlayer.
    
    DO icx = 1 TO 4:
        IF CAN-DO(tt-shuffled-deck.card-title,tt-player.player-hand[icx]) THEN
        DO:
            ASSIGN loMatch                    = TRUE
                   idx                        = icx
                   tt-player.player-hand[icx] = ""
                   tt-shuffled-deck.in-hand   = FALSE
                   tt-shuffled-deck.in-stack  = TRUE.
            LEAVE.
        END.            
        ELSE loMatch = FALSE.
    END.          
    
    IF NOT loMatch THEN 
    DO:
        MESSAGE "Can't pick up card!" VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    IF loMatch THEN
    DO:
        /* Make the stack */
        FIND LAST bu-tt-stack WHERE bu-tt-stack.player-id = tt-player.player-name NO-ERROR.
        CREATE tt-stack.
        ASSIGN tt-stack.player-id     = tt-player.player-name
               tt-stack.stack-num     = IF AVAILABLE bu-tt-stack THEN
                                        bu-tt-stack.stack-num + 1
                                        ELSE 1
               tt-stack.stack-contents = tt-shuffled-deck.card-title.
                                                           
        /* Pick up card */      
        RELEASE tt-shuffled-deck.                                      
        FIND FIRST tt-shuffled-deck
             WHERE tt-shuffled-deck.in-hand   = FALSE
               AND tt-shuffled-deck.discarded = FALSE 
               AND tt-shuffled-deck.in-stack  = FALSE NO-ERROR.
        IF NOT AVAIL tt-shuffled-deck THEN
        DO:
            MESSAGE "No more cards to be dealt!" VIEW-AS ALERT-BOX.
            RETURN.
        END.
        ELSE ASSIGN tt-player.player-hand[idx] = tt-shuffled-deck.card-title
                    tt-shuffled-deck.in-hand        = TRUE
                    tt-shuffled-deck.in-stack       = TRUE.           
    END.                
    
           
    
END.      


#lang racket

;; Bank Heist Adventure Game
;; A text-based adventure game with inventory system and room progression
;; Using purely functional programming style

;; ---- GAME STATE STRUCTURE ----
;; The game state is represented as a structure with the following fields:
;; - current-room: The ID of the room the player is currently in
;; - inventory: A list of items the player is carrying
;; - rooms: An immutable hash table mapping room IDs to room structures
;; - unlocked-exits: A list of unlocked exit paths (represented as [from-room to-room])
;; - game-over: A boolean indicating whether the game is over (success or failure)
;; - message: A message to display to the player

(struct game-state (current-room inventory rooms unlocked-exits game-over message) #:transparent)

;; Room structure
(struct room (name description items exits) #:transparent)

;; Exit structure (represents a connection between rooms)
(struct exit (direction target key-item description locked-msg) #:transparent)

;; ---- GAME INITIALIZATION ----

;; Create a new game state with initial values
(define (new-game)
  (let ([rooms (make-immutable-hash
        `((outside . ,(room "Outside the Bank" 
              "You're standing outside the Grand National Bank. It's after hours and the building is mostly dark.\nA security guard patrols around the corner every few minutes." 
              (list "lockpick" "ski mask")
              (list (exit 'east 'entrance (list "ski mask" "lockpick") "The front entrance to the bank is to the east." "The bank is locked and you don't want to be seen get a ski mask and a lockpick!"))))
        
        (entrance . ,(room "Bank Entrance" 
              "You're in the main entrance of the bank. Security cameras sweep the area.\nThe vault is visible at the far end of the lobby." 
              (list "security keycard")
              (list (exit 'west 'outside #f "The exit to the street is to the west." #f)
                    (exit 'north 'security-room "security keycard" "A door marked 'Security' is to the north. It has a keycard reader." 
                          "The door is locked. You need a security keycard to enter.")
                    (exit 'east 'lobby #f "The main lobby is to the east." #f))))
        
        (security-room . ,(room "Security Room" 
              "Monitors display feeds from cameras throughout the bank.\nA control panel has various buttons and switches." 
              (list "vault code")
              (list (exit 'south 'entrance #f "The door back to the entrance is to the south." #f))))
        
        (lobby . ,(room "Bank Lobby" 
              "The grand lobby has marble floors and several teller windows.\nThe vault door is visible to the south." 
              (list)
              (list (exit 'west 'entrance #f "The entrance is back to the west." #f)
                    (exit 'south 'vault-door #f "The imposing vault door is to the south." #f))))
        
        (vault-door . ,(room "Vault Entrance" 
              "You're standing in front of the massive bank vault door.\nIt has both a keypad for a code and requires a special key." 
              (list "vault key")
              (list (exit 'north 'lobby #f "The lobby is to the north." #f)
                    (exit 'south 'vault (list "vault code" "vault key") "The vault is beyond this door."
                          "The vault door is locked. You need both the vault code and the vault key to open it."))))
        
        (vault . ,(room "Bank Vault" 
              "You've made it into the vault! Gold bars, cash, and safety deposit boxes line the walls.\nYou've successfully pulled off the heist!" 
              (list "money bag" "gold bar" "diamonds")
              (list (exit 'north 'vault-door #f "The way out is back north through the vault door." #f))))))])
    
    ;; Return initial game state with empty unlocked-exits list
    (game-state 'outside '() rooms '() #f "Welcome to the Bank Heist Adventure!\nYour mission: Break into the vault and escape with the loot.")))

;; ---- GAME ACTIONS ----

;; Look around the current room
(define (look game)
  (let* ([current-room-id (game-state-current-room game)]
         [rooms (game-state-rooms game)]
         [current-room (hash-ref rooms current-room-id)]
         [items-str (if (empty? (room-items current-room))
                       "There are no items here."
                       (string-append "You see: " (string-join (room-items current-room) ", ")))]
         [exits-str (string-join 
                     (map (lambda (exit) 
                            (exit-description exit)) 
                          (room-exits current-room))
                     "\n")]
         [message (string-append 
                   "--- " (room-name current-room) " ---\n\n"
                   (room-description current-room) "\n\n"
                   items-str "\n\n"
                   "Available exits:\n" exits-str)])
    (struct-copy game-state game [message message])))

;; Take an item from the current room
(define (take game item-name)
  (let* ([current-room-id (game-state-current-room game)]
         [rooms (game-state-rooms game)]
         [current-room (hash-ref rooms current-room-id)])
    (if (member item-name (room-items current-room))
        ;; Item exists in room, take it
        (let* ([new-room-items (remove item-name (room-items current-room))]
               [new-room (struct-copy room current-room [items new-room-items])]
               [new-rooms (hash-set rooms current-room-id new-room)]
               [new-inventory (cons item-name (game-state-inventory game))])
          (struct-copy game-state game 
                      [inventory new-inventory] 
                      [rooms new-rooms]
                      [message (format "You picked up the ~a." item-name)]))
        ;; Item doesn't exist in room
        (struct-copy game-state game 
                     [message (format "There is no ~a here." item-name)]))))

;; Drop an item from inventory into the current room
(define (drop game item-name)
  (let* ([current-room-id (game-state-current-room game)]
         [rooms (game-state-rooms game)]
         [current-room (hash-ref rooms current-room-id)])
    (if (member item-name (game-state-inventory game))
        ;; Item exists in inventory, drop it
        (let* ([new-inventory (remove item-name (game-state-inventory game))]
               [new-room-items (cons item-name (room-items current-room))]
               [new-room (struct-copy room current-room [items new-room-items])]
               [new-rooms (hash-set rooms current-room-id new-room)])
          (struct-copy game-state game 
                      [inventory new-inventory] 
                      [rooms new-rooms]
                      [message (format "You dropped the ~a." item-name)]))
        ;; Item doesn't exist in inventory
        (struct-copy game-state game 
                     [message (format "You don't have a ~a." item-name)]))))

;; Check inventory
(define (inventory game)
  (let ([inv (game-state-inventory game)])
    (if (empty? inv)
        (struct-copy game-state game [message "Your inventory is empty."])
        (struct-copy game-state game 
                    [message (string-append "Inventory: " (string-join inv ", "))]))))

;; Helper function to check if an exit is unlocked
(define (exit-unlocked? game from-room to-room)
  (member (list from-room to-room) (game-state-unlocked-exits game)))

;; Try to move in a direction
(define (move game direction)
  (let* ([current-room-id (game-state-current-room game)]
         [rooms (game-state-rooms game)]
         [current-room (hash-ref rooms current-room-id)]
         [possible-exit (findf (lambda (exit) (eq? (exit-direction exit) direction)) 
                               (room-exits current-room))])
    (cond
      [(not possible-exit)
       (struct-copy game-state game 
                   [message (format "You can't go ~a from here." direction)])]
      
      ;; Check if the exit is already unlocked
      [(exit-unlocked? game current-room-id (exit-target possible-exit))
       (let ([new-room (exit-target possible-exit)])
         (if (eq? new-room 'vault)
             ;; Winning condition - reached the vault
             (struct-copy game-state game 
                         [current-room new-room]
                         [message (string-append 
                                   (room-description (hash-ref rooms new-room))
                                   "\n\nCONGRATULATIONS! You've successfully broken into the vault!")]
                         [game-over #t])
             ;; Normal room transition
             (struct-copy game-state game 
                         [current-room new-room]
                         [message (string-append 
                                   "You move " (symbol->string direction) ".\n\n"
                                   "--- " (room-name (hash-ref rooms new-room)) " ---\n\n"
                                   (room-description (hash-ref rooms new-room)))])))]
      
      ;; Check if the exit requires a key and player doesn't have it
      [(and (exit-key-item possible-exit)
            (not (has-required-items? game (exit-key-item possible-exit))))
       (struct-copy game-state game 
                   [message (exit-locked-msg possible-exit)])]
      
      ;; Player has the key, unlock the door and proceed
      [else
       (let* ([new-room (exit-target possible-exit)]
              ;; Add this exit to unlocked-exits list if it has a key requirement
              [new-unlocked-exits (if (exit-key-item possible-exit)
                                     (cons (list current-room-id new-room) 
                                           (game-state-unlocked-exits game))
                                     (game-state-unlocked-exits game))]
              [unlock-msg (if (exit-key-item possible-exit)
                             (string-append 
                              "You unlock the door with " 
                              (if (string? (exit-key-item possible-exit))
                                  (string-append "the " (exit-key-item possible-exit))
                                  (string-append "your items"))
                              ". ")
                             "")])
         (if (eq? new-room 'vault)
             ;; Winning condition - reached the vault
             (struct-copy game-state game 
                         [current-room new-room]
                         [unlocked-exits new-unlocked-exits]
                         [message (string-append 
                                   unlock-msg
                                   (room-description (hash-ref rooms new-room))
                                   "\n\nCONGRATULATIONS! You've successfully broken into the vault!")]
                         [game-over #t])
             ;; Normal room transition
             (struct-copy game-state game 
                         [current-room new-room]
                         [unlocked-exits new-unlocked-exits]
                         [message (string-append 
                                   unlock-msg
                                   "You move " (symbol->string direction) ".\n\n"
                                   "--- " (room-name (hash-ref rooms new-room)) " ---\n\n"
                                   (room-description (hash-ref rooms new-room)))])))])))

;; Check if the player has all required items
(define (has-required-items? game required-items)
  (let ([inventory (game-state-inventory game)])
    (cond
      [(string? required-items) (member required-items inventory)]
      [(list? required-items) (andmap (lambda (item) (member item inventory)) required-items)]
      [else #t])))

;; ---- GAME LOOP AND COMMAND PARSING ----

;; Parse a command string and perform the corresponding action
(define (parse-command game command-str)
  (let* ([cmd-parts (string-split (string-downcase command-str))]
         [process-item-name (lambda (parts) (string-join (cdr parts) " "))])
    (if (empty? cmd-parts)
        (struct-copy game-state game [message "Please enter a command."])
        (let ([cmd (first cmd-parts)])
          (cond
            [(or (string=? cmd "quit") (string=? cmd "exit"))
             (struct-copy game-state game [game-over #t] [message "Thanks for playing!"])]
            
            [(string=? cmd "look")
             (look game)]
            
            [(string=? cmd "inventory")
             (inventory game)]
            
            [(string=? cmd "take")
             (if (>= (length cmd-parts) 2)
                 (take game (process-item-name cmd-parts))
                 (struct-copy game-state game [message "Take what?"]))]
            
            [(string=? cmd "drop")
             (if (>= (length cmd-parts) 2)
                 (drop game (process-item-name cmd-parts))
                 (struct-copy game-state game [message "Drop what?"]))]
            
            [(or (string=? cmd "go") (string=? cmd "move"))
             (if (>= (length cmd-parts) 2)
                 (move game (string->symbol (second cmd-parts)))
                 (struct-copy game-state game [message "Go where?"]))]
            
            [(member cmd '("north" "south" "east" "west" "up" "down"))
             (move game (string->symbol cmd))]
            
            [(string=? cmd "help")
             (struct-copy game-state game 
                         [message "Available commands:\n- look: Look around\n- inventory: Check your inventory\n- take [item]: Pick up an item\n- drop [item]: Drop an item\n- go [direction]: Move in a direction\n- north, south, east, west: Move in that direction\n- quit: Exit the game"])]
            
            [else (struct-copy game-state game [message "I don't understand that command."])])))))

;; Main game loop
(define (game-loop)
  (let loop ([game (new-game)])
    (displayln (game-state-message game))
    (newline)
    
    (when (not (game-state-game-over game))
      (display "> ")
      (flush-output)
      (let* ([input (read-line)]
             [new-game (parse-command game input)])
        (newline)
        (loop new-game)))))

;; Start the game
(game-loop)
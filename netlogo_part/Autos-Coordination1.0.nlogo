;Project at Santa Fe.
; Evolve automata to play the Battle of the sexes, without and with exogenous signals

;On changing the code:
; I believe it works good for different number of internal states, and potential observed states of the world. However, when changing to more states (i.e include signal), be careful to 
; also change (or activate with a switch) the potential things to observe.

;Although I can change the "n-outputs" parameters, it is not ready for having two decisions. If want to include that, I would have to re-check everything, although seems like a first start
; having it in the code for creating the strategy string, which I have.



          
globals [
  ;N                  ;Number of strategies
  ;rounds             ;Number of rounds played EACH GENERATION versus EACH strategy. One generation is one tick count
  ;N-parents          ;Number of parents to include each generation. For example, in paper is 20. The top 20 scorers are kept, the others die, and the top 20 have offspring
  ;n-internal-states  ;Number of internal states of the machine
  n-observables       ;Number of observable inputs. For example, in the prisoner's dilemma, each round you can observe Defect or Cooperate,
                      ;so the possible inputs are 2 (if the machine only cares about the action of the rival, not its own)
  ;n-signals          ;Number of signals
  n-outputs          ; Number of decisions to make. If binary (like cooperate or defect) should be 1.
  ;n-populations      ; Can be only 1 or 2. For two, is "row" vs "column" players
  ;game-to-play       ;Can be "prisoner-dilemma", "battle-of-sexes" or "chicken".
  ;action-mutation-prob     ;probability of actions bits to be changed
  ;transition-mutation-prob ;probability of transitions bits to be changed
  ;n-signal-cards     ; The exogenous signals works as an outsider choosing one "card": for example, card one gives player1 suggestion 0, and player 2 suggestion 1.
  
  ;print?             ;If True, will print the outpute files. Turn off if just want to run the model without creating or changing output files
  ;files-name-modifier ;Choose in the interface for altering the name of the output files
  
  
  action-positions
  transition-positions
  
  parents 
  parents-row             ;List (not agentset). Decides the top scorers in a round so they can reproduce. This is a list of the potential parents.
  parents-column
  
  offspring-row           ;List (not agentset). To keep track in the outputs of the newborn strategies
  offspring-column
  
  mutants-row            ;List showing, in the same order as offspring-row and offspring-list, which of those offspring are mutants
  mutants-column
  
  signal-card      ;The "card" from which the signal is selected
  times-of-miscoordination    ;These three globals are for output analysis. How many times during a generation, each equilibrium was played. (miscoordination, players action was A and B, or B and A)
  times-of-row-preference     ;player row played B, and player column played A
  times-of-column-preference  ;player column played B, and player row played A
   
  Heads-A-count               ;These are to measure if they are coordinating in using the signal. For example, if two autos observed "H" (heads) and both played "A", then that's a count for "Heads-A-count"
  Heads-B-count
  Tails-A-count
  Tails-B-count
]

turtles-own[
  strategy              ;Defines the automata. Consists of one initial transition number, and N-internal-states internal states
  
  initial-transition    ;First transition (first number in the strategy)
  
  transition            ;Current transition number
  internal-state        ;Determines current action, and transition states for each possible observable state of the world
  action                ;What the player plays. Defines binary as 0 and 1
  
  rival-action          ;What the current rival plays ("A" or "B")          
  overall-score         ;Sum of score for playing vs all others
  played-against        ;List to keep track of which other strategies have been faced, so the algorithm doesn't have to repeat games
  normalised-score      ;The score of this generation (after playing vs all strategies and self), normalized
  new-born?             ;"true" if the turtle was just born. This is to select, out of these new-borns, some to mutate and some to leave as they are
  
  population-type       ;Can be "row" or "column"
  signal-received       ;Saves the signal received this round
  
  ;The next are for output only, not used in the basic running of the model
  average-score        ;Overall score divided by the number of rounds and by the number of rivals (N).
  mutant?
]

;==============================================================
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;     SET UP     ;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



to setup
    clear-all
    set n-observables 2 + 2 * n-signals  ;Only two potential observables when no signal included (the rivals' action). If there's a signal, then there are 4 observables
    set n-outputs 1 ;Might change for other projects, but for this has to be one
    
    create-turtles N * n-populations [           ; Creates N turtles per population
      set-starting-population    ; Uses a reporter to initiate all necessary variables for newborn strategies.
    ]
    
    if n-populations = 2 [
      ask n-of N turtles [set population-type "column"]  ;If two populations allowed, half of the total turtles (N) is assigned to "column" population
    ]
       
    
    create-some-globals          ;Creates some lists that are necessary for the mutation procedure
    reset-ticks
end

;==============================================================



to set-starting-population           ;Used under turtle context. This reporters creates strategy strings from scratch.
    set initial-transition random n-internal-states   ;Defines the initial transition as an integer between 0 and n-internal-states (starting with 0)
    set strategy []
    set strategy lput initial-transition strategy       ;Defines the first bit (the first transition) in the strategy, in order to initialise it

    ;To create the strategy string, uses two loops. The first, for each internal state, chooses the action (binary)
    ;The second, for each potential observable world (rival's actions in games with no signal), creates the required transition states
    let i 0
    while [i < n-internal-states] [    ;This loop generates the indicated number of internal states, and puts them together in the strategy string
      let a one-of ["A" "B"]               ;Choose one action, as the first number of an internal state randomly (
      set strategy lput a strategy     ;Add the action to the strategy string
      
      let i1 0
      while [i1 < n-observables][        ;Creates the required transitions contained in each internal state. One transition per potential observable worlds (actions of rival when no signal included)
        let t random n-internal-states   ;Creates one random transition (which is a number to select one potential internal state)
        set strategy lput t strategy     ;Attaches the transition to the strategy
        set i1 i1 + 1                ; For second loop
      ] 
     set i i + 1                     ; For first loop
    ]
    
    ;Once the strategy string is created, use it to select the first internal-state
    set transition item 0 strategy    ;Sets the transition without any prior play (i.e. based on the first bit of the strategy string)
    update-internal-state-action      ;Updates the internal-state (i.e. select the new string) based on the current transition (is a number). Also, select action (first bit of internal-state)
    if n-populations = 2 [set population-type "row"]         ;Initialises all strategies with one single type.Used only when two populations allowed
  
end


to update-internal-state-action              ;Updates the internal-state (i.e. select the new string) based on the current transition (is a number). Also, select action (first bit of internal-state)
  set internal-state new-internal-state      ;Choose internal state, based on the "transition" variable. "New-internal-state" is a "to-report" block
  set action item 0 internal-state           ;Choose action (0 or 1), based on the "internal-state" string. Is the first position of it.
end

    
; This reporter uses the "transition" number, and reports the corresponding internal state (the whole string for that state, starting with the action, followed by the transitions)
; Basically "cuts" the strategy in the appropriate places to have the internal state dictated by the variable transition
; To be used under turtle context, because transition is a turtle variable (e.g. after "ask turtles")
; So for a given transition, chooses the corresponding internal-state string from the bigger strtegy string

to-report new-internal-state
  let starting-point (transition * (n-observables + n-outputs) + 1) ;Calculate the initial position in the strategy for each internal state.
  let end-point (starting-point + n-observables)                    ;Calculate the final position of the cut
  report (sublist strategy starting-point (end-point + 1))          ;Report the internal state as a string. The "+1" is in order to include the end-point, because "sublist" excludes it (i.e. numbers below)
end


to create-some-globals   ;Creates two lists:one with the positions in the strings that represent actions, and another for transitions

  ask one-of turtles [        ;They are all global lists that are the same for all turtles (hence a global), but asks one random turtle to run it because uses some turtle variables (e.g. length of strategy)
    ;This first block creates a list with the positions that represent actions.
    set action-positions []         ;This list will mark which positions are actions in the strategy string
    let i 1                         ; The index 'i' starts equal to one, so that the first bit in the strategy string to be changed is the first action
    while [i < length strategy] [                 ;Creates a list with the positions in the strategy string that represent an action (not a transition)
      set action-positions lput i action-positions
      set i i + (n-observables + n-outputs)      ;Increases counter 'i' in the amount required to go to the next bit that is an action in the strategy string
    ] 
    
    ;Next two blocks creates a list of the positions in the string that represent a transition
    ;First, create a list, in steps of one, of the length of the strategy string
    let j 0
    set transition-positions []
    while [j < length strategy] [
      set transition-positions lput j transition-positions
      set j j + 1
    ]
        
    ;This foreach is to create a list transition positions, by removing the positions that correspond to an action
    foreach transition-positions [
      if member? ? action-positions [
        set transition-positions remove ? transition-positions
      ]
    ]
  ]
  set parents-row [] 
  set parents-column []
  set signal-card random n-signals ;initialise the signal-card so not always starts with zero Might be useful in the "two-worlds-test-BOS"
  
end


;==============================================================
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;     GO!!   ;;;;;;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  
  if ticks != 0[
    choose-parents-list  ;Creates a list with the turtles ordered from higher to lower payoffs. This is to select the ones that potentially will reproduce
    create-offspring     ;Pairwise tournament to reproduce
    mutate-offspring     ;Mutation occurs only to new-borns
  ]
  
  restart-values    ;Restarts variables like score and rivals so far, and also restarts the strategy of each turtle (set internal state and action)
  play
  ;normalise-score
  calculate-for-outputs
  if print? = true [  ;Print files only if chosen in the interface (sometimes one just wants to run and test the model, without messing around with outputs)
    print-outputs-summary
    print-outputs-turtles
  ]
  tick
end

;==========================================================

to restart-values
  ask turtles [
    set overall-score 0                          ;Reset the overall score for each generation
    set normalised-score 0
    set played-against []                        ;Reset value as a list
    set new-born? 0
    set mutant? false
    ;set played-against (lput self played-against) ;Include this line if don't want to include playing against a clone. Use for testing the software coding, not the model itself
    
    ;The next two lines seem redundant (because was used during setup as well, but is necessary because the new offspring created in last round don't have these variables set
    ;This restart has to be with the INITIAL internal-state, for it sets the action and internal state before observing anything in the environment (before first round).
    ;So it should be the same commands as in setup
    
    set transition item 0 strategy    ;Sets the transition without any prior play (i.e. based on the first bit of the strategy string)
    update-internal-state-action      ;Updates the internal-state (i.e. select the new string) based on the current transition (is a number). Also, select action (first bit of internal-state)
  ]
  set times-of-miscoordination 0
  set times-of-row-preference 0
  set times-of-column-preference 0

  set Heads-A-count 0
  set Heads-B-count 0
  set Tails-A-count 0
  set Tails-B-count 0
end


to play
  ask population-row [                                              ;For this procedure, player1 will call all the other turtles (players2), and play against each of them
    ;show strategy show internal-state show action
    
    let player1 self
    ;Everytime start playing versus a new rival, use the first action dictated by the strategy. This is to avoid using the last action versus the last rival.
    set transition item 0 strategy    ;Sets the transition without any prior play (i.e. based on the first bit of the strategy string)
    update-internal-state-action      ; Reporter to set internal-state as the first one in the strategy string, and choose action
    
 
    ask population-column with [(member? player1 played-against) = false][     ;This command makes the caller ("myself") to play only versus strategies that haven't played before in this generation. This is to avoid repeating plays.
      let player2 self
      
      ;The whole next loop simply plays the iterated selected game (e.g. prisoner dilemma, or chicken game) for the specified amount of "rounds", between two turtles (player 1 and player 2)
      let i 0
      while [i < rounds][                              ;Play for the determined number of rounds versus each rival
        
        set rival-action [action] of player1
        let one-shot-score2 one-shot-score     ;This is just to use below, and make player1, when playing against self, take the average payoffs between both
        
        ;PLAYER 2 SETS SCORE
        if player1 != player2 [                                ;For player2, won't count the score when playing against self
          set overall-score (overall-score + one-shot-score)   ;Sums current score to player 2. If playing against self, player2 doesn't count the score. CHECK one-shot-score REPORTER TO CHANGE PAYOFFS
        ]

        ask player1 [
          set rival-action [action] of player2
          
          ;PLAYER 1 SETS SCORE
          ifelse player1 != player2                             ;In the paper, each strategy plays against a clone once. This conditional is to sum the payoffs differently when playing the clone
          [set overall-score (overall-score + one-shot-score)]   ;Sums current score to player 1. CHECK one-shot-score REPORTER TO CHANGE PAYOFFS
          [set overall-score (overall-score + ((one-shot-score + one-shot-score2) / 2))]   ;If playing against self, take the average payoffs
          count-equilibrium-played ; This reporter updates the globals for output, the ones keeping track of how many times each equilibrium or miscoordination happened in a generation



;;;;;;;;SIGNAL CARD SELECTED. This will be the same for both players;;;;;;
        ;Here, with 50% chance choose card 0, with 50% card 1 (since signal-card is 2)
        set signal-card random n-signal-cards  ;Sets a "card" for the exogenous signal generation. This is necessary so that the signal is related for both players. This is used in the "exogenous-signal" procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
                  
                    
          ;READ SIGNAL RECEIVED FOR PLAYER 1
          set signal-received exogenous-signal
         
          ;Next two lines are the core of the strategies (for player1): evaluate rival-action, choose a new internal-state depending on it, and then decide next move.
          
          ;show strategy show internal-state show rival-action show signal-received;======
          
          
          set transition new-transition                ;This line updates the transition function (reporter uses current internal-state and rival-action)
          ;print "transition" show transition ;========
          update-internal-state-action      ;Updates the internal-state (i.e. select the new string) based on the current transition (is a number). Also, select action (first bit of internal-state)
          
        ]
        
        ;READ SIGNAL RECEIVED FOR PLAYER 2
        set signal-received exogenous-signal

        ;Next two lines are the core of the strategies (for player2): evaluate rival-action, choose a new internal-state depending on it, and then decide next move.
        
        ;show strategy show internal-state show rival-action show signal-received;======
        
        set transition new-transition                ;This line updates the transition function (reporter uses current internal-state and rival-action)
        ;print "transition" show transition ;========
        update-internal-state-action      ;Updates the internal-state (i.e. select the new string) based on the current transition (is a number). Also, select action (first bit of internal-state)
        
        set i (i + 1)                    ;Counter for the "while" loop, so here finishes one round. When i is big enough, choose next player 1
      ]
      
      ;Before finishing and moving to the next rival, mark that these rival has been played against in this generation (tick)
      set played-against lput player1 played-against
      ask player1[
        set played-against lput player2 played-against
        ;show length played-against show population-type;==== 
      ]
      ;show length played-against show population-type;==== 
    ]
  ]  
end

;;==================================================================
;
;;;; THIS IS KEY!!!! CHANGE HERE IF THERE ARE MORE THAN 4 TRANSITIONS IN EACH INTERNAL STATE
to-report new-transition                 ; Using the current internal-state and the rival-action, choose the new transition
  ;If there's no signal, then the transition depends only on 2 observable states of the world: whether rival's action was 0 or 1
  
  ; THIS PROCEDURE COULD BE IMPROVED IN TERMS OF SIMPLIFYING THE CODE
  ; SOMETHING LIKE "FOREACH STATE OF THE WORLD, REPORT ITEM i
  if n-signals = 0 [ 
    if rival-action = "A" [report (item 1 internal-state)]
    if rival-action = "B" [report (item 2 internal-state)]
  ]
  
  ;If there's one signal, then there are 4 possible observable states
  if n-signals = 1 [
    if rival-action = "A" and signal-received = "H" [report (item 1 internal-state)]
    if rival-action = "B" and signal-received = "H" [report (item 2 internal-state)]
    if rival-action = "A" and signal-received = "T" [report (item 3 internal-state)]
    if rival-action = "B" and signal-received = "T" [report (item 4 internal-state)]
  ]
  
end
;
;;==================================================================


to-report one-shot-score     ;Games score for one round, from the perspective of player one in traditional matrix representations of the game
  if game-to-play = "prisoner-dilemma" [
    ;1 is cooperate, 0 defect
    if action = "B" and rival-action = "B" [report 3]   ;Coop-coop payoffs
    if action = "A" and rival-action = "A" [report 1]   ;Defect-defect payoffs
    if action = "B" and rival-action = "A" [report 0]   ;I cooperate, but the other defect (worst payoff)
    if action = "A" and rival-action = "B" [report 5]   ;I exploit the other cooperator (best payoff)
  ]
  ;BATTLE OF THE SEXES: only works with two populations
  if game-to-play = "battle-of-sexes"[
    ;B is boxing (say, male's preferred), A is aunt-visit (say, female preferred)
    ;This first two lines are the same for both row and column, since is zero in any case
    if action = "B" and rival-action = "A" [report 0]   ;"Go to different places" payoffs
    if action = "A" and rival-action = "B" [report 0]   ;"Go to different places" payoffs
    
    ;If payoffs for row players (males)
    if population-type = "row"[
      if action = "A" and rival-action = "A" [report 2]   ;Both go to female's preferred option 
      if action = "B" and rival-action = "B" [report 3]   ;Both go to male's preferred option
    ]
    ;Payoffs for columns players (femeales)
    if population-type = "column"[
      if action = "A" and rival-action = "A" [report 3]   ;Both go to female's preferred option 
      if action = "B" and rival-action = "B" [report 2]   ;Both go to male's preferred option
    ]
  ]
  
  ;This game is to test if the learning happens in a world where it definitely should. If signal=0, only both playing A pays. If signal=1, only both playing B pays.
  if game-to-play = "two-worlds-BOS-test"[
    ;In any case, playing different (A and B or B and A) gives zero pays...
    if action = "B" and rival-action = "A" [report 0]   ;"Go to different places" payoffs
    if action = "A" and rival-action = "B" [report 0]   ;"Go to different places" payoffs
    
    ;If the signal-card is 0 (players receive "H" signal-received), only pays to coordinate on A
    if signal-card = 0 [
      if action = "A" and rival-action = "A" [report 3]
      if action = "B" and rival-action = "B" [report 0]
    ]
    
    ;If the signal-card is 1 (players receive "T" signal-received), only pays to coordinate on B
    if signal-card = 1 [
      if action = "A" and rival-action = "A" [report 0]
      if action = "B" and rival-action = "B" [report 3]
    ]
  ]
  
;  if game-to-play = "chicken"[
;    ;1 is cooperate, 0 defect
;    if action = 1 and rival-action = 1 [report 3]   ;Coop-coop payoffs
;    if action = 0 and rival-action = 0 [report 1]   ;Defect-defect payoffs
;    if action = 1 and rival-action = 0 [report 0]   ;I cooperate, but the other defect (worst payoff)
;    if action = 0 and rival-action = 1 [report 5]   ;I exploit the other cooperator (best payoff)    
;  ]
end


to normalise-score
  let mean-score (mean [overall-score] of turtles)
  let sd-score (standard-deviation [overall-score] of turtles)
  
  ask turtles[
    let a (overall-score - mean-score)
    
    ifelse sd-score = 0      ;This conditional is just to avoid dividing by zero. Is almost impossible for it to happen with full agents and rounds, but when debugging with one or two, is an issue
    [set normalised-score 0]
    [set normalised-score (a / sd-score)]
    ;set normalised-score (normalised-score + alpha) ; When alpha=2, strategies with scores inferior to 2 standard deviations, don't get to reproduce (Because they have negative probability)
    ;if normalised-score < 0 [set normalised-score 0]
  ]
  
end


to calculate-for-outputs
  ; Calculate average-score: this is the average payoff per generation, per round.
  ask turtles [
    set average-score (overall-score / (rounds * N))
  ]
  
end
to choose-parents-list    ;Organises potential parents in a list, and kills the rest of the agents, for both populations
  
  ;Choose parents list for population row
  let a reverse (sort-on [average-score] population-row) ;Organises agents from higher to lower scores (population-row)
  set parents-row (sublist a 0 N-parents)   ;Define the agentset that will reproduce. These are the first "N-parents" in the list "a", basically the top scorers. Is an ordered list
  ask population-row with [(member? self parents-row) = false] [die] ;Kill all the turtles that are not parents (i.e. the ones with lower scores)
  ;Choose parents list for population column
  let b reverse (sort-on [average-score] population-column) ;Organises agents from higher to lower scores (population-column)
  set parents-column (sublist b 0 N-parents)   ;Define the agentset that will reproduce. These are the first "N-parents" in the list "a", basically the top scorers. Is an ordered list
  ask population-column with [(member? self parents-column) = false] [die] ;Kill all the turtles that are not parents (i.e. the ones with lower scores)
  ;show count population-row   show count population-column
end


to create-offspring    ;Repopulates both populations independently
  
  ;Offspring for population row
  while [count population-row < N] [
    let parent1 one-of parents-row                ;Choose two parents, parent1 randomly from the population...
    let parent2 0
    let winner-parent 0
    ask parent1 [set parent2 one-of other population-row]   ;... and then parent2. This "ask" is used to include the "other" primitive, so parent2 will never be the same parent1
  
    ifelse [overall-score] of parent1 >= [overall-score] of parent2   ; Compares the overall-score of the two parents. The one with higher, is selected as "winner-parent"
           [set winner-parent parent1]
           [set winner-parent parent2]
  
    ask winner-parent [     ;The winning parent replicates one time (i.e. has one offspring, as a copy of itself)
      hatch 1 [
        set new-born? true   ;To keep track of the strategies that are just born
        ;show count population-row;=====       
      ]
    ]
  ]
  
  ;Offspring for population column
  while [count population-column < N] [
    let parent1 one-of parents-column                ;Choose two parents, parent1 randomly from the population...
    let parent2 0
    let winner-parent 0
    ask parent1 [set parent2 one-of other population-column]   ;... and then parent2. This "ask" is used to include the "other" primitive, so parent2 will never be the same parent1
  
    ifelse [overall-score] of parent1 >= [overall-score] of parent2   ; Compares the overall-score of the two parents. The one with higher, is selected as "winner-parent"
           [set winner-parent parent1]
           [set winner-parent parent2]
  
    ask winner-parent [     ;The winning parent replicates one time (i.e. has one offspring, as a copy of itself)
      hatch 1 [
        set new-born? true   ;To keep track of the strategies that are just born
        ;show count population-row;=====             
      ]
    ]
  ]
  
  set offspring-row reverse (sort-on [average-score] population-row with [new-born? = true])        ;Creates a list with the offspring, used for printing output in behaviour space
  set offspring-column reverse (sort-on [average-score] population-column with [new-born? = true])  
end

to mutate-offspring
  
  ask turtles with [new-born? = true] [
    if random-float 1 < 0.5 [
      set mutant? true  ; Keep track of which strategies are new mutants
      
      ;Choose randomly one internal state to change
      let mutant-state random n-internal-states
      
      ;Calculate the starting and end position, in the strategy string, of the state just randomly chosen (mutant-state)
      let starting-point (mutant-state * (n-observables + n-outputs) + 1) ;Calculate the initial position in the strategy the internal state to mutate.
      let end-point (starting-point + n-observables)                     ;Calculate the final position of the cut
      
      let positions-list []      ;Initiates the lists with the positions of the internal-state
      
      ;show starting-point show end-point;=======
      
      ;This loop creates the list of positions to be mutated. Those positions are the ones corresponding to the internal state to be mutated
      let i 1
      while [starting-point + i <= end-point] [
        set positions-list lput (starting-point + i) positions-list ;Positions list is created as the positions containing only the transitions (not the action, because i=1 at the beggining)
        set i i + 1
      ]
      ;show strategy show mutant-state show positions-list;========
      
      ;With the positions to be changes in the strategy, given in "positions-list", start the mutation.
      ;Mutations works as follows: flip a coing. If tails (with 50% chance), flip the action (i.e. first position of "positions-list")
      ;If heads (the other 50%) choose randomly one of the other bits (i.e. transitions) and change it
      
      ;First, flip coin. With 50%, flip the action in the internal state
      ifelse random-float 1 < 0.5 [
        let a item starting-point strategy
        ;show strategy show mutant-state;=======
        if a = "A" [set strategy replace-item starting-point strategy "B"]    ;With probability 50%, flipt the action (i.e. first bit in the mutant-state, or starting-point)
        if a = "B" [set strategy replace-item starting-point strategy "A"]
        ;show strategy  ;=========
        ]
      
      
      ;Else (i.e. if didn't mutate the action), change one of the transitions
      [
        let t one-of positions-list         ;Choose one of the positions that correspond to the transitions in the mutant-state
        set strategy replace-item t strategy random n-internal-states
        ;show strategy  ;=========
      ]
    ]
  ]
  
  ;Next blocks creates a list, to be checked in the same order as offspring-row, to know which one of the offsprings have been mutated
  set mutants-row []
  foreach offspring-row[
    ask ?[
      set mutants-row lput mutant? mutants-row
    ]
  ]
  
  set mutants-column []
  foreach offspring-column[
    ask ?[
      set mutants-column lput mutant? mutants-column
    ]
  ]
  
  ;show mutants-row show count turtles with [mutant? = true and population-type = "row"]
  ;show mutants-column show count turtles with [mutant? = true and population-type = "column"]
end


;Next two reporters report the agentset of each population-type
to-report population-row
  report turtles with [population-type = "row"]  
end

to-report population-column
  report turtles with [population-type = "column"]  
end

;This reporter is the exogenous signal generator. Can be changed easily
;Signals can be tried correlated, non-correlated, good signals and bad signals.
to-report exogenous-signal
  
  ;This signal gives the same recommendation to both players (so they can try to coordinate in choosing the same in Battle of the sexes)
  ;"signal-card" is set in the "play" procedure, each card 0 and 1 having 50% chance of being selected
  if signal-card = 0 [report "H"]  ;Heads of tails as a signal
  if signal-card = 1 [report "T"]
  
  
end

to count-equilibrium-played
  if action != rival-action [set times-of-miscoordination times-of-miscoordination + 1] ;If chose different action, count a miscoordination
  if action = "B" and rival-action = "B" [set times-of-row-preference times-of-row-preference + 1]; If both played B, is the equilibrium were row gets the high payoffs
  if action = "A" and rival-action = "A" [set times-of-column-preference times-of-column-preference + 1]; If both played A, is the equilibrium were column gets the high payoffs
  
  ;To count the number of times they coordinate on the signal
  if n-signals = 1[
    if signal-received = "H" and action = "A" and rival-action = "A" [set Heads-A-count Heads-A-count + 1]
    if signal-received = "H" and action = "B" and rival-action = "B" [set Heads-B-count Heads-B-count + 1]
    if signal-received = "T" and action = "A" and rival-action = "A" [set Tails-A-count Tails-A-count + 1]
    if signal-received = "T" and action = "B" and rival-action = "B" [set Tails-B-count Tails-B-count + 1]
  ]
end

to print-outputs-summary ;This is to print overall tables. Is basically the same I could do on Behaviour Space.
  
  let path-file "/Users/luisalejandrolee/Desktop/autos_coordination_computational.git/netlogo_part/outputs/"
  set-current-directory path-file
 
  let summary-file (word "summary_" files-name-modifier "_states_" n-internal-states "_signal_" n-signals "_rounds_" rounds "_N_" N "_parents_" n-parents ".txt") ;Name the file for summary stats, by using the main global values
  if ticks = 0 [ ;Only once need to delete previous file, open the file and print the variable names
    if file-exists? summary-file [
      file-delete summary-file ; Delete the file (to overwrite it) only in the first generation
    ]
    file-open summary-file
    file-type "generation,av_score_row,av_score_col,miscoordination_perc,coordination_B_perc,coordination_A_perc,heads_A_perc,heads_B_perc,tails_A_perc,tails_B_perc\n" ;Variables to include
    file-close
  ]
  
  file-open summary-file
  file-type ticks file-type","
  file-type mean [average-score] of turtles with [population-type = "row"] file-type ","
  file-type mean [average-score] of turtles with [population-type = "column"] file-type ","
  file-type times-of-miscoordination / (rounds * N * N) file-type ","
  file-type times-of-row-preference / (rounds * N * N) file-type ","
  file-type times-of-column-preference / (rounds * N * N)file-type ","
  file-type Heads-A-count / (rounds * N * N) file-type ","
  file-type Heads-B-count / (rounds * N * N) file-type ","
  file-type Tails-A-count / (rounds * N * N) file-type ","
  file-type Tails-B-count / (rounds * N * N) file-type "\n" ;If more variables added, replace the \n by a comma. The carriage return should only go with the final reported variable
  
  file-close
end

to print-outputs-turtles ;To print the outputs directly from the turtles
  
  let strategies-file (word "strategies_" files-name-modifier "_states_" n-internal-states "_signal_" n-signals "_rounds_" rounds "_N_" N "_parents_" n-parents ".txt")
  if ticks = 0 [ ;Only once need to delete previous file, open the file and print the variable names
    if file-exists? strategies-file [
      file-delete strategies-file ; Delete the file (to overwrite it) only in the first generation
    ]
      
    file-open strategies-file
    file-type "generation,ID,population,score,auto_long\n" ;Variables to include. Need carriage return at the end in case more variables are included
    file-close
  ]
  
  file-open strategies-file
  
  ask turtles[
    
    file-type ticks file-type ","
    file-type who file-type ","
    file-type population-type file-type ","
    file-type average-score file-type ","
    file-type strategy file-type "\n" ;Remeber carriage return should go only with the last variable, so if add more, change it for comma or organise properly
    
    
    ;file-type file-type ","    
  ]
  
  file-close
end





































@#$#@#$#@
GRAPHICS-WINDOW
794
23
1039
227
16
16
5.242424242424242
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

INPUTBOX
317
42
367
102
N
40
1
0
Number

BUTTON
62
56
126
89
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
81
106
144
139
Go!
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
317
105
374
165
rounds
5
1
0
Number

TEXTBOX
375
59
525
101
Number of strategies per population. Total N is twice this number\n
11
0.0
1

TEXTBOX
382
130
532
158
Number of rounds (150 in the paper)
11
0.0
1

INPUTBOX
40
200
125
261
N-parents
20
1
0
Number

TEXTBOX
133
203
283
259
Number of parents each round (20 in paper, so top 20 scores live and have offspring, the rest die)
11
0.0
1

BUTTON
39
139
204
172
Go go go go !!!! YEAH!
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
735
13
1074
234
Average payoff per game iteration
NIL
NIL
0.0
10.0
0.0
3.0
true
true
"" ""
PENS
"Row players" 1.0 0 -16777216 true "" "if ticks != 0 [plot mean [average-score] of turtles with [population-type = \"row\"]]"
"Column players" 1.0 0 -13345367 true "" "if ticks != 0 [plot mean [average-score] of turtles with [population-type = \"column\"]]"
"Total average" 1.0 0 -10899396 true "" "if ticks != 0 [plot mean [average-score] of turtles]"

INPUTBOX
419
291
520
351
n-internal-states
8
1
0
Number

TEXTBOX
528
301
678
329
The more Internal states, more complex the machine
11
0.0
1

TEXTBOX
532
356
747
404
If more than one signal, change the button and arrange the first part of \"setup\" procedure
11
0.0
1

CHOOSER
532
39
712
84
game-to-play
game-to-play
"prisoner-dilemma" "battle-of-sexes" "two-worlds-BOS-test" "chicken"
1

SLIDER
415
356
509
389
n-signals
n-signals
0
1
1
1
1
NIL
HORIZONTAL

SLIDER
535
106
707
139
n-populations
n-populations
1
2
2
1
1
NIL
HORIZONTAL

TEXTBOX
541
147
691
203
Code only working with two populations. If wanna run just one population, use earlier versions
11
0.0
1

SLIDER
401
400
526
433
n-signal-cards
n-signal-cards
2
2
2
1
1
NIL
HORIZONTAL

TEXTBOX
547
413
697
483
If more than two cards, have to go to \"new-transition\" procedure and allow more than 4 states
11
0.0
1

PLOT
734
235
1152
449
Signal use
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Heads-A" 1.0 0 -16777216 true "" "plot Heads-A-count"
"Heads-B" 1.0 0 -7500403 true "" "plot Heads-B-count"
"Tails-A" 1.0 0 -2674135 true "" "plot Tails-A-count"
"Tails-B" 1.0 0 -955883 true "" "plot Tails-B-count"

SWITCH
93
324
196
357
print?
print?
0
1
-1000

TEXTBOX
217
330
367
400
If ON, will print the outpute files. Turn OFF if just want to run the model without creating or changing output files
11
0.0
1

INPUTBOX
20
405
255
465
files-name-modifier
trial
1
0
String

TEXTBOX
22
473
271
546
Use different names to print different output files. For example, if set to \"trial\", then the printed outputs will be \"summary_trial_...\" or \"strategies_trial_...\". Useful for easily saving different files
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="first for python (4 treatments, all strategies)" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="8S treatment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="two worlds test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;two-worlds-BOS-test&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4NS treatment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="8NS treatment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4S treatment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4S 100,000 periods" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="template for duplication" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <metric>Heads-A-count</metric>
    <metric>Heads-B-count</metric>
    <metric>Tails-A-count</metric>
    <metric>Tails-B-count</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="8S and 8NS signal measure" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>mean [average-score] of turtles with [population-type = "row"]</metric>
    <metric>mean [average-score] of turtles with [population-type = "column"]</metric>
    <metric>mean [average-score] of turtles</metric>
    <metric>[strategy] of item 0 parents-column</metric>
    <metric>[strategy] of item 1 parents-column</metric>
    <metric>[strategy] of item 2 parents-column</metric>
    <metric>[strategy] of item 3 parents-column</metric>
    <metric>[strategy] of item 4 parents-column</metric>
    <metric>[strategy] of item 5 parents-column</metric>
    <metric>[strategy] of item 6 parents-column</metric>
    <metric>[strategy] of item 7 parents-column</metric>
    <metric>[strategy] of item 8 parents-column</metric>
    <metric>[strategy] of item 9 parents-column</metric>
    <metric>[strategy] of item 10 parents-column</metric>
    <metric>[strategy] of item 11 parents-column</metric>
    <metric>[strategy] of item 12 parents-column</metric>
    <metric>[strategy] of item 13 parents-column</metric>
    <metric>[strategy] of item 14 parents-column</metric>
    <metric>[strategy] of item 15 parents-column</metric>
    <metric>[strategy] of item 16 parents-column</metric>
    <metric>[strategy] of item 17 parents-column</metric>
    <metric>[strategy] of item 18 parents-column</metric>
    <metric>[strategy] of item 19 parents-column</metric>
    <metric>[strategy] of item 0 offspring-column</metric>
    <metric>[strategy] of item 1 offspring-column</metric>
    <metric>[strategy] of item 2 offspring-column</metric>
    <metric>[strategy] of item 3 offspring-column</metric>
    <metric>[strategy] of item 4 offspring-column</metric>
    <metric>[strategy] of item 5 offspring-column</metric>
    <metric>[strategy] of item 6 offspring-column</metric>
    <metric>[strategy] of item 7 offspring-column</metric>
    <metric>[strategy] of item 8 offspring-column</metric>
    <metric>[strategy] of item 9 offspring-column</metric>
    <metric>[strategy] of item 10 offspring-column</metric>
    <metric>[strategy] of item 11 offspring-column</metric>
    <metric>[strategy] of item 12 offspring-column</metric>
    <metric>[strategy] of item 13 offspring-column</metric>
    <metric>[strategy] of item 14 offspring-column</metric>
    <metric>[strategy] of item 15 offspring-column</metric>
    <metric>[strategy] of item 16 offspring-column</metric>
    <metric>[strategy] of item 17 offspring-column</metric>
    <metric>[strategy] of item 18 offspring-column</metric>
    <metric>[strategy] of item 19 offspring-column</metric>
    <metric>[strategy] of item 0 parents-row</metric>
    <metric>[strategy] of item 1 parents-row</metric>
    <metric>[strategy] of item 2 parents-row</metric>
    <metric>[strategy] of item 3 parents-row</metric>
    <metric>[strategy] of item 4 parents-row</metric>
    <metric>[strategy] of item 5 parents-row</metric>
    <metric>[strategy] of item 6 parents-row</metric>
    <metric>[strategy] of item 7 parents-row</metric>
    <metric>[strategy] of item 8 parents-row</metric>
    <metric>[strategy] of item 9 parents-row</metric>
    <metric>[strategy] of item 10 parents-row</metric>
    <metric>[strategy] of item 11 parents-row</metric>
    <metric>[strategy] of item 12 parents-row</metric>
    <metric>[strategy] of item 13 parents-row</metric>
    <metric>[strategy] of item 14 parents-row</metric>
    <metric>[strategy] of item 15 parents-row</metric>
    <metric>[strategy] of item 16 parents-row</metric>
    <metric>[strategy] of item 17 parents-row</metric>
    <metric>[strategy] of item 18 parents-row</metric>
    <metric>[strategy] of item 19 parents-row</metric>
    <metric>[strategy] of item 0 offspring-row</metric>
    <metric>[strategy] of item 1 offspring-row</metric>
    <metric>[strategy] of item 2 offspring-row</metric>
    <metric>[strategy] of item 3 offspring-row</metric>
    <metric>[strategy] of item 4 offspring-row</metric>
    <metric>[strategy] of item 5 offspring-row</metric>
    <metric>[strategy] of item 6 offspring-row</metric>
    <metric>[strategy] of item 7 offspring-row</metric>
    <metric>[strategy] of item 8 offspring-row</metric>
    <metric>[strategy] of item 9 offspring-row</metric>
    <metric>[strategy] of item 10 offspring-row</metric>
    <metric>[strategy] of item 11 offspring-row</metric>
    <metric>[strategy] of item 12 offspring-row</metric>
    <metric>[strategy] of item 13 offspring-row</metric>
    <metric>[strategy] of item 14 offspring-row</metric>
    <metric>[strategy] of item 15 offspring-row</metric>
    <metric>[strategy] of item 16 offspring-row</metric>
    <metric>[strategy] of item 17 offspring-row</metric>
    <metric>[strategy] of item 18 offspring-row</metric>
    <metric>[strategy] of item 19 offspring-row</metric>
    <metric>mutants-row</metric>
    <metric>mutants-column</metric>
    <metric>times-of-miscoordination</metric>
    <metric>times-of-row-preference</metric>
    <metric>times-of-column-preference</metric>
    <metric>Heads-A-count</metric>
    <metric>Heads-B-count</metric>
    <metric>Tails-A-count</metric>
    <metric>Tails-B-count</metric>
    <enumeratedValueSet variable="n-internal-states">
      <value value="8"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signal-cards">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="game-to-play">
      <value value="&quot;battle-of-sexes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rounds">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-populations">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-outputs">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-signals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-parents">
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@

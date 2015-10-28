'''
This file keeps the functions required for the minimization of autos. Contains:

new_empty_auto()
convert_to_canonical(normal_auto)
    remap(state0, transitions, statemap, nextstate)
minimized_automaton()
to_clean_auto(auto_clean)


'''
# Big part of this code (canonical_automaton, remamp function and minimized_auto)is based on Warren's
# version of Jhon Miller's code, originally in Java.

import pandas as pd
import numpy as np
import copy

# Create an empty auto as a numpy array
def new_empty_auto(n_obs, n_states):
    dtype = [('actions', 'S1'), ('transitions', np.int32, n_obs)] # structure for "normal_auto" variable
    new_auto = np.zeros(n_states, dtype) # initialize normal_auto (make all transitions zero)
    new_auto['actions'] = 'x' # Make all actions x
    return new_auto



# return a cannonically ordered auto stripped of inaccessible states
def convert_to_canonical(normal_auto, n_states, init_state, n_obs):
    # statemap will keep track of which states are remapped, and the order in which they should be
    # nextstate tracks the next available state number and eventually provides the number of accessible states

    statemap = np.ones(n_states)*(-1) #initialize map with null values
    statemap[init_state] = 0 # initial state renumbered to state 0
    nextstate = 1 # number of states remapped
    # start it off with the start state, off to recursive remapper
    nextstate = remap(init_state, normal_auto['transitions'], statemap, nextstate, n_obs) # nextstate is the number of states remapped

    # Here, after the recursion of the 'remap' function, the two key variables obtained are nextstates and statemap
    # nextstates was used to build statemap, but it contains the number of available states on the machine
    # statemap shows which are those states, and in which order they are accessed.

    #print 'nextstate = ', nextstate
    #print 'statemap (for canonical)= ', statemap

    auto = new_empty_auto(n_obs, n_states) # Create new empty auto
    for s in xrange(n_states):
        if statemap[s] >= 0:   #if state is remapped (and accesible)
            auto[statemap[s]]['actions'] = normal_auto[s]['actions']
            for t in xrange(n_obs):
                auto[statemap[s]]['transitions'][t] = statemap[normal_auto[s]['transitions'][t]]
    auto = auto[:nextstate] # Cuts the auto to contain only the accesible states

    #global updated_n_states # Used to "get out" of this function the local variable nexstate
    #updated_n_states = nextstate # To update n_states. Use this because nexstate is local variable
    return auto


# Function 'remap' is used to convert into canonical form ('convert_to_canonical')
# ***recursively*** branches down automata and remaps everything via the ordered inputs
def remap(state0, transitions, statemap, nextstate, n_obs):
    for t in xrange(n_obs): # for all possible observation/input
        if statemap[transitions[state0][t]] < 0: # if that state is not yet remapped (i.e. is not -1)
            statemap[transitions[state0][t]] = nextstate # assign it next available state num, then inc nstate (next line is the increase)
            nextstate += 1
            nextstate = remap(transitions[state0][t], transitions, statemap, nextstate, n_obs) # recursively remap on this state
    return nextstate


def minimized_automaton(canon_auto, n_states, init_state, n_obs):
    # define the equivalence matrix
    equiv = np.zeros((n_states, n_states), dtype = bool)
    for s1 in xrange(n_states): #All possible states pair combinations
        for s2 in xrange(s1, n_states):
            if canon_auto[s1]['actions'] == canon_auto[s2]['actions']: # Mark potentially equivalent states (i.e. with same action)
                equiv[s1][s2] = True
                equiv[s2][s1] = True # probably not needed, but cost is low
    #print equiv

    # now refine the equivalence matrix by iterating transitions on transitions until it stablizes
    while True:
        changed = False # Track if changes ocurred during the iteration. If no changes, we are done!
        newequiv = np.zeros((n_states, n_states), dtype = bool) # Will contain the new equivalence matrix
        for s1 in xrange(n_states): # All possible states pair combinations
            for s2 in xrange(s1, n_states):
                if equiv[s1][s2]: # for all potential equivalent states (i.e. states with same action)
                    sametransitions = True # Assume they have the same transitions
                    for t in xrange(n_obs): # for all transitions
                        # Compares if the transitions in both states are the same (i.e. lead to the same action)
                        if (not equiv[canon_auto[s1]['transitions'][t]][canon_auto[s2]['transitions'][t]]):
                            sametransitions = False
                    newequiv[s1][s2] = sametransitions
                    newequiv[s2][s1] = sametransitions
                    if (not sametransitions):
                        changed = True # At least one change was made, so iterate again
        equiv = newequiv # Update the equivalence matrix to the modified one
        if not changed: # if changed is False (no changes ocurred), then exit the loop
            break

    # equiv now holds the truly equivalent states, so remap these into a new, minimized automaton
    # Make a new statemap to copy the new auto
    statemap = np.array(xrange(n_states)) # initial statemap just maps to current state
    for s1 in xrange(n_states):
        for s2 in xrange(s1+1, n_states):
            if equiv[s1][s2]:
                statemap[s2] = statemap[s1]
    #statemap[init_state] = 0 # I think not needed
    #print 'statemap (for minimization) = ', statemap

    # This looks like the same procedure used for copying the canonical auto (uses the new statemap)
    newauto = copy.deepcopy(canon_auto)
    #print 'newauto', newauto
    for s in xrange(n_states):
        newauto[statemap[s]]['actions'] = canon_auto[s]['actions']
        for t in xrange(n_obs):
            newauto[statemap[s]]['transitions'][t] = statemap[canon_auto[s]['transitions'][t]]
    newauto = convert_to_canonical(newauto, n_states, init_state, n_obs)
    return newauto


def to_format_netlogo_auto(auto_clean):
    auto_clean = auto_clean.encode("ascii") # Convert to string
    #print type(big_auto) # Should be string
    auto_clean = auto_clean.replace('[','') # Delete useless characters
    auto_clean = auto_clean.replace(']','')
    auto_clean = auto_clean.replace('"','')
    auto_clean = auto_clean.split(' ') # Converts the string into a list
    for i,j in enumerate(auto_clean): # Converts all the numbers (transitions) into integers
        if auto_clean[i] != 'A' and auto_clean[i] != 'B':
            auto_clean[i] = int(auto_clean[i])
    return auto_clean

def update_state_no_signal(auto, current_state, rival_action):
    if rival_action == "A":
        a = 0
    elif rival_action == "B":
        a = 1
    new_action = auto[current_state][1][a]

    return new_action


def create_joint_machine_no_signal(auto0, auto1):
    #Initialize variables
    temp = [None, None]
    size = len(auto0) * len(auto1) + 1  #Max states before joint machine cycles
    states = [temp[:] for i in xrange(size)]
    actions = [temp[:] for i in xrange(size)]
    auto0_state = 0 #Current state of the machines (intial is zero)
    auto1_state = 0

    metastate = -1
    cyclestart = -1
    cycle = False

    #Detect cycle (i.e. when the same pair of states appears)
    while not cycle:
        metastate += 1
        states[metastate][0] = auto0_state #Add current state of autos
        states[metastate][1] = auto1_state
        actions[metastate][0] = auto0[auto0_state][0] #Add current action of autos (based on current state)
        actions[metastate][1] = auto1[auto1_state][0] #Remember auto1 is the minimized auto

        #Move autos to next state based on action of other
        auto0_state = update_state_no_signal(auto0, auto0_state, actions[metastate][1])
        auto1_state = update_state_no_signal(auto1, auto1_state, actions[metastate][0])

        #Now cycle to check if we have been at the two new states before
        for ms in xrange(metastate): #previous metastates
            if (states[metastate][0] == states [ms][0]) and (states[metastate][1] == states [ms][1]):#cycle has started
                cyclestart = ms
                cycle = True

    states = states[:metastate+1] #delete the empty states
    actions = actions[:metastate+1]
    #Save joint machine and generation as list (to later make them a dataframe)
    jm = {"metastate": metastate, "cyclestart": cyclestart, "states": states, "actions": actions} #joint machine
    return jm


def minimize_joint_machine_no_signal(mm):
    #print "full metmachine = ", mm

    #Trans contains the transition structure of the joint machine. For example, the 1 in position 0 indicates that in
    #state 0 the machine transitions to state 1, and so on. This makes the structure quite simple (linear).
    #Then the final state is linked to transition towards the one where the cycle starts.

    #Create transition structure
    tstates = mm["metastate"] + 1 #number of states of the metamachine (mm)
    trans = [st+1 for st in xrange(tstates)]
    trans[mm["metastate"]] = mm["cyclestart"]
    #print "trans = ", trans, "\n"

    #Create tstates*tstates equivalence matrix
    temp = [False for i in xrange(tstates)]
    equiv = [temp[:] for i in xrange(tstates)]#Will contain True for pairs of states that are potentially equivalent

    for s1 in xrange(tstates):
        for s2 in xrange(s1, tstates): #all possible states combinations
            if mm["actions"][s1] == mm["actions"][s2]: #actions in both states of the joint machine are the same
                equiv[s1][s2] = True
                equiv[s2][s1] = True

    #Use equivalence matrix to find the truly equivalent states
    while True: #repeat until changes becomes True
        changes = False
        #Create tstates*tstates equivalence matrix ("False" in all coordinates)
        temp1 = [False for i in xrange(tstates)]
        newequiv = [temp1[:] for i in xrange(tstates)]

        for s1 in xrange(tstates):
            for s2 in xrange(s1, tstates):#all possible state combinations
                if (equiv[s1][s2]): #a potential mapping exists
                    #print "s1 = ", s1, "s2 = ", s2
                    sametrans = True
                    if (not equiv[trans[s1]][trans[s2]]):#...ask if their transitions go to states with same actions
                        sametrans = False
                    newequiv[s1][s2] = sametrans
                    newequiv[s2][s1] = sametrans
                    if (not sametrans):
                        changes = True # at least one change made, so iterate again
        equiv = newequiv
        if not changes:
            break

    #equiv now has the truly equivalent states of the machine, so remap them into a new statemap
    statemap = [sm for sm in xrange(tstates)] # Will be used like this: --> statemap[old_State] = new_state

    for s1 in xrange(tstates-1):
            for s2 in xrange(s1+1, tstates): #all possible relevant state combinations
                if (equiv[s1][s2]): #states are truly equivalent
                    statemap[s2] = statemap[s1] #remaps, while capturing any earlier remaps

    #Final minimization
    #as the joint machines (with no signal) are linear, is easy to minimize from here
    neededstates = 0 #states needed for the minimized joint machine
    for sm in xrange(tstates):
        if statemap[sm] > neededstates:
            neededstates = statemap[sm] #finds the maximum number in statemap?
    neededstates += 1 #to account for position zero

    temp2 = [None for i in xrange(neededstates)]
    #min_jm = {"actions": temp2[:], "states": temp2[:], "cyclestart": None, "tstates": neededstates}
    min_jm = {"actions": temp2[:], "cyclestart": None} #simpler... don't really need to track states of individual autos or neededstates
    transitions = temp2[:]
    currentstate = 0
    for sm in xrange(tstates):
        if (statemap[sm] == currentstate):
            #min_jm["states"][currentstate] = list(mm["states"][sm]) #probably useless to track the states...
            min_jm["actions"][currentstate] = list(mm["actions"][sm])
            transitions[currentstate] = statemap[trans[sm]]
            currentstate += 1

    min_jm["cyclestart"] = transitions[neededstates-1] #save where the cycle starts

    #for debugging
    """
    print "\n", "joint machine: ", "\n"
    print "states jm = ", jm["states"]
    print "actions jm = ", jm["actions"]
    print "transitions = ", trans
    print "truly equiv = ", equiv

    print "\n","minimized_jm: ", "\n"
    print min_jm
    print "actions min = ", min_jm["actions"]
    print "cyclestart min = ", min_jm["cyclestart"], "\n"
    print "final transitions = ", transitions
    print "statemap = ", statemap
    print "neededstates = ", neededstates

"""
    return min_jm

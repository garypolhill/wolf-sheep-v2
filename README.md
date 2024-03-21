# wolf-sheep-v2
An updated version of the canonical [NetLogo Wolf Sheep Predation](http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation) library model (Copyright 1997 Uri Wilensky, and released under a CC-BY-NC-SA 3.0 licence.)

Some very minor changes have been introduced to Wilensky's code to do the following:

  + Enable a fixed seed to be used during initialization and an arbitrary seed thereafter;
  + Enable one sheep to move after all sheep and wolves have been created;
  + Enable one wolf to be killed each tick by a 'hunter'.

The model also keeps track of the maximum numbers of sheep and wolves, and contains two BehaviorSpace experiments that explore the above options. To do this without causing memory to run out if the sheep population takes off while there are still wolves, an absolute maximum on the number of sheep has also been introduced.

The purpose of these changes is to allow exploration of the extent of the variation in outcomes arising from the same initial conditions.

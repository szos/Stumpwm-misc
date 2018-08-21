# Stumpwm-misc
A collection of my hacking on stumpwm. everything from modules to small bits of code.

Reclassify.lisp contains functionality for running shell commands and reclassifying the window thats opened. It also contains a tiny hombrewed object system (if you could call it that) that I use for expressing one component of the reclassification process.

Ratcontrol is a pointer controller for stumpwm.

key-simulator.lisp attempts to simulate keys based on the class of the current window, a la EXWM.

Window-property-search.lisp allows you to search and return windows based on substrings. 

System-control.lisp is a set of closures useful for manipulating volume, brightness, etc. 

little-bits-of-utility.lisp is a misc file, where everything thats to small or to general to fit in one file. 

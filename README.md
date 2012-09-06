rehoo rebuilds `default.hoo` in parallel from an arbitrarily large number of
`.hoo` files.

Usage:

    rehoo -j8 .
    
This finds all `.hoo` files in the current directory, and results in a new
`default.hoo` in the same directory.  The aim is to be as quick at this as
possible, while minimizing the hit on system resources (like open file
handles).

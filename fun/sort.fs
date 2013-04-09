: bidup over over ;

: rollnm dup if 1- over 2+ roll -rot recurse else drop drop then ;

: -roll dup rollnm ;

: pack over 1+ -roll ;

: unpack over 1+ + pick ;

: swapnm bidup 3 + roll swap 2+ -roll swap 2+ roll swap -roll ;

: storeindices 2 pick 2+ swap pack 1- swap pack drop ;

: indices dup 1+ pick over 3 + pick ;

: getindexed indices 2+ pick swap 2+ pick ;

: swapindexed indices 1+ swap 1+ swap swapnm ;

: indices! dup 1+ roll over 2+ roll ;

: inci swap 1+ swap ;

: incj drop 1+ 0 swap ;

: testi bidup = ;

: testj dup 3 pick >= ;

: advance indices! inci
    testi if incj then ;

: sortiter testj
    if drop drop
    else storeindices
	getindexed < if swapindexed then advance recurse
    then ;

: sort 0 1 sortiter ;

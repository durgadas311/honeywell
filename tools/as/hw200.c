// TODO: add more H200/2000 structures/data

// Conversion table for ASCII-7 to H200/2000 characters
char hw200[128] = {
	036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036,
	036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036, 036,
//       sp    !    "    #    $    %    &    '    (    )    *    +    ,    -    .    /
	015, 057, 055, 052, 053, 035, 017, 012, 074, 034, 054, 020, 073, 040, 033, 061,
//        0    1    2    3    4    5    6    7    8    9    :    ;    <    =    >    ?
	000, 001, 002, 003, 004, 005, 006, 007, 010, 011, 014, 032, 060, 013, 016, 037,
//        @    A    B    C    D    E    F    G    H    I    J    K    L    M    N    O
	072, 021, 022, 023, 024, 025, 026, 027, 030, 031, 041, 042, 043, 044, 045, 046,
//        P    Q    R    S    T    U    V    W    X    Y    Z    [    \    ]    ^    _
	047, 050, 051, 062, 063, 064, 065, 066, 067, 070, 071, 076, 056, 036, 077, 036,
//        `    a    b    c    d    e    f    g    h    i    j    k    l    m    n    o
	036, 021, 022, 023, 024, 025, 026, 027, 030, 031, 041, 042, 043, 044, 045, 046,
//        p    q    r    s    t    u    v    w    x    y    z    {    |    }    ~  del
	047, 050, 051, 062, 063, 064, 065, 066, 067, 070, 071, 036, 036, 036, 075, 036,
};

PROGRAM mtadvent;

{$s+}

    USES  applestuff,
          {$u#5:mt1.code} advdata,
          {$u#5:probs.code} probs,
          {$u#5:cmds3.code} cmds3,
          {$u#5:cmds2.code} cmds2,
          {$u#5:cmds1.code} cmds1,
          {$u#5:locs.code} locs;
          
PROCEDURE t1 (ll: rooms);
BEGIN

  CASE ll OF
  
    start:    travel (trail1, trail4, nowhere,
                      trail3, nowhere, nowhere);
    trail1:   travel (rubble1, start, nowhere,
                      nowhere, nowhere, nowhere);
    trail2:   travel (trail6, rubble2, nowhere,
                      trail5, trail6, rubble1);
    trail3:   travel (nowhere, nowhere, start,
                      slope5, nowhere, nowhere);
    trail4:   travel (start, mound, nowhere,
                      nowhere, nowhere, nowhere);
    trail5:   travel (nowhere, nowhere, trail2,
                      saddle1, saddle1, nowhere);
    trail6:   travel (saddle3, trail2, nowhere,
                      nowhere, saddle3, trail2);
    col1:     travel (nowhere, nowhere, saddle1,
                      nowhere, saddle2, saddle1);
    col2:     travel (slope5, rut3, nowhere,
                      nowhere, slope5, rut3);
    col3:     travel (nowhere, nowhere, steep3,
                      nowhere, peak1, hill1);
    col4:     travel (nowhere, gully1, nowhere,
                      saddle3, nowhere, gully1);
    rubble1:  travel (trail2, trail1, rubble2,
                      nowhere, trail2, nowhere);
    rubble2:  travel (nowhere, nowhere, nowhere,
                      rubble1, nowhere, nowhere);
    mound:    travel (trail4, nowhere, nowhere,
                      nowhere, nowhere, monastery);
    monastery:travel (nowhere, nowhere, nowhere,
                      nowhere, mound, cellar);
    cellar:   travel (nowhere, nowhere, nowhere,
                      nowhere, monastery, cave);
    cave:     travel (nowhere, nowhere, nowhere,
                      nowhere, cellar, nowhere);
    slope1:   travel (nowhere, saddle2, nowhere,
                      nowhere, steep1, saddle2);
    slope2:   travel (nowhere, nowhere, nowhere,
                      nowhere, steep2, trolls);
    slope5:   travel (steep5, col2, trail3,
                      chasm, steep5, nowhere);
    rut1:     travel (nowhere, nowhere, ridge2,
                      trolls, nowhere, nowhere);
    rut2:     travel (nowhere, nowhere, ridge1,
                      nowhere, nowhere, nowhere);
    rut3:     travel (col2, nowhere, nowhere,
                      nowhere, col2, nowhere);
  END { CASE ll OF };

END { PROCEDURE t1 };

PROCEDURE t2 (ll: rooms);
BEGIN

  CASE ll OF
  
    steep1:   travel (nowhere, nowhere, chasm,
                      nowhere, wmm, slope1);
    steep2:   travel (nowhere, nowhere, chasm,
                      nowhere, peak2, slope2);
    steep3:   travel (nowhere, nowhere, nowhere,
                      chasm, peak3, mtcave2c);
    steep4:   travel (nowhere, nowhere, chasm,
                      nowhere, peak4, ridge1);
    steep5:   travel (peak5, slope5, nowhere,
                      chasm, peak5, slope5);
    peak1:    travel (nowhere, nowhere, nowhere,
                      chasm, nowhere, col3);
    peak2:    travel (nowhere, steep2, chasm,
                      nowhere, nowhere, mtcave1c);
    peak3:    travel (nowhere, nowhere, nowhere,
                      chasm, nowhere, steep3);
    peak4:    travel (nowhere, chasm, nowhere,
                      nowhere, nowhere, steep4);
    peak5:    travel (nowhere, nowhere, chasm,
                      nowhere, nowhere, steep5);
    saddle1:  travel (trolls, nowhere, rut1,
                      col1, trolls, mtcave1a);
    saddle2:  travel (slope1, col1, steep2,
                      ridge1, slope1, col1);
    saddle3:  travel (nowhere, trail6, col4,
                      nowhere, hill1, trail6);
    mtcave1a: travel (mtcave1b, nowhere, nowhere,
                      nowhere, trail5, nowhere);
    mtcave1b: travel (mtcave1c, nowhere, nowhere,
                      nowhere, nowhere, nowhere);
    mtcave1c: travel (nowhere, mtcave1b, nowhere,
                      nowhere, peak2, nowhere);
    mtcave2a: travel (mtcave2b, nowhere, nowhere,
                      nowhere, gully2, nowhere);
    mtcave2b: travel (mtcave2c, mtcave2a, nowhere,
                      nowhere, nowhere, nowhere);
    mtcave2c: travel (nowhere, mtcave2b, nowhere,
                      nowhere, steep3, nowhere);
    gully1:   travel (col4, gully2, nowhere,
                      nowhere, col4, gully2);
    gully2:   travel (gully1, nowhere, nowhere,
                      nowhere, gully1, mtcave2a);
    hill1:    travel (nowhere, nowhere, nowhere,
                      nowhere, col3, saddle3);
    trolls:   travel (slope2, saddle1, chasm,
                      nowhere, slope2, saddle1);
    ridge1:   travel (nowhere, nowhere, saddle2,
                      rut2, steep4, nowhere);
    ridge2:   travel (nowhere, nowhere, saddle3,
                      nowhere, nowhere, nowhere);
    wmm:      BEGIN
                pwmm;
                travel (nowhere, nowhere, nowhere,
                        nowhere, nowhere, steep1);
              END;
    chasm:    BEGIN
                done := true;
                killed := true;
              END;
              
  END { CASE location OF };
  
END { PROCEDURE t2 };

BEGIN
  
  writeln ('Adventure 3 begins...');
  writeln ('There are ', memavail, ' bytes of memory left.');
  
  repeat
  
    visited[location] := visited[location] + 1;
    show (location);
    
    IF ord (location) < ord (steep1)
    THEN
      t1 (location)
    ELSE
      t2 (location)
    { END IF ord ... };
    
  UNTIL done;

END.
